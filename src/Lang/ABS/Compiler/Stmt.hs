-- | Functions for translating ABS statements (inside an ABS method) to Haskell code
module Lang.ABS.Compiler.Stmt 
    (tBlockWithReturn
    ,tInitBlockWithReturn
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import Control.Monad.Trans.State (evalState, withState, put, get, modify)
import Control.Monad.Trans.Reader (runReaderT, mapReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad (ap)
import Lang.ABS.Compiler.ExprLifted
import Lang.ABS.Compiler.Expr (tPattern, tType)
import qualified Data.Map as M
import Data.List (nub)
import Data.Foldable (foldlM)
import Control.Monad (liftM)

-- | Translating a method block or main block that can return
tBlockWithReturn :: (?moduleTable :: ModuleTable,?moduleName::ABS.QType) => [ABS.AnnotStm] -> String -> ScopeTable -> ScopeTable -> [ScopeTable] -> String -> [ABS.LIdent] -> HS.Exp
tBlockWithReturn stmts cls clsScope mthScope scopes interfName meths = 
    let stmts' = if null stmts
                 then [ABS.AnnStm [] $ ABS.SReturn $ ABS.ExpP $ ABS.ESinglConstr $ ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((1,1), "Unit")]]
                 else case last stmts of
                   ABS.AnnStm _ (ABS.SReturn _) -> stmts
                   _ -> stmts ++ [ABS.AnnStm [] $ ABS.SReturn $ ABS.ExpP $ ABS.ESinglConstr $ ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((1,1), "Unit")]]
    in evalState (runReaderT 
                  (tBlock stmts' True)
                  (clsScope, mthScope, interfName, cls, False, meths))
    scopes

-- | Translating an init block
--
-- can always return (with: return Unit),
-- can not run await and/or sync calls
tInitBlockWithReturn :: (?moduleTable :: ModuleTable,?moduleName::ABS.QType) => [ABS.AnnotStm] -> String -> ScopeTable -> ScopeTable -> [ScopeTable] -> String -> [ABS.LIdent] -> HS.Exp
tInitBlockWithReturn stmts cls clsScope mthScope scopes interfName meths = 
    let stmts' = if null stmts
                 then [ABS.AnnStm [] $ ABS.SReturn $ ABS.ExpP $ ABS.ESinglConstr $ ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((1,1), "Unit")]]
                 else case last stmts of
                   ABS.AnnStm _ (ABS.SReturn _) -> stmts
                   _ -> stmts ++ [ABS.AnnStm [] $ ABS.SReturn $ ABS.ExpP $ ABS.ESinglConstr $ ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((1,1), "Unit")]]
    in evalState (runReaderT 
                  (tBlock stmts' True)
                  (clsScope, mthScope, interfName, cls, True, meths))
    scopes

-- | Translating any block . This functions pushes a new scope to the 'StmtM' scopes-stack-state.
tBlock :: (?moduleTable :: ModuleTable,?moduleName::ABS.QType) => [ABS.AnnotStm] -> Bool -> StmtM HS.Exp
tBlock stmts canReturn = do
  ts <- mapReaderT (withState (M.empty:)) $ tStmts stmts canReturn -- add the new scope level
  lift $ modify tail     -- remove the added scope level, after executing
  return $ case ts of
             [] -> eReturnUnit
             _ -> HS.Do $ case last ts of
                           (HS.Generator _ _ _) -> ts ++  [HS.Qualifier eReturnUnit]
                           _ -> ts
      where
        eReturnUnit :: HS.Exp
        eReturnUnit = (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure")
                             (HS.Con $ HS.Special $ HS.UnitCon)) -- pure ()


-- | Translating a bunch of ABS statements
tStmts :: (?moduleTable :: ModuleTable,?moduleName::ABS.QType) => [ABS.AnnotStm] -> Bool -> StmtM [HS.Stmt]

tStmts [] _canReturn = return []
tStmts ((ABS.AnnStm _ (ABS.SReturn (ABS.ExpE eexp@(ABS.SyncMethCall _ _ _)))):[]) True = do
  texp <- tEffExpStmt eexp False (Just (HS.Var $ HS.UnQual $ HS.Ident "return'"))
  return [HS.Qualifier $ texp]
tStmts ((ABS.AnnStm _ (ABS.SReturn (ABS.ExpE eexp@(ABS.ThisSyncMethCall _ _)))):[]) True = do
  texp <- tEffExpStmt eexp False (Just (HS.Var $ HS.UnQual $ HS.Ident "return'"))
  return [HS.Qualifier $ texp]
tStmts ((ABS.AnnStm _ (ABS.SReturn e)):[]) True = do
  texp <- case e of  -- TODO: have to force to WHNF
           ABS.ExpE eexp -> tEffExpStmt eexp False Nothing
           ABS.ExpP texp -> tPureExpStmt texp
  return [HS.Qualifier $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "return'") (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<") texp]
tStmts (ABS.AnnStm _ (ABS.SReturn _):_) _ = error "Return must be the last statement"


tStmts (ABS.AnnStm _ (ABS.SDecAss typ ident@(ABS.LIdent (_,var)) (ABS.ExpE eexp@(ABS.SyncMethCall _ _ _))) : rest) canReturn = do
  addToScope ident typ -- add to scope, but it's lazy monad, otherwise use Monad.State.Strict
  fscope <- funScope -- so we have to force to WHNF with a lookup
  case M.lookup ident fscope of -- has to be a lookup and not a member, to force to WHNF
      Just _ -> do
        tafter <- tStmts rest canReturn
        texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
                   (HS.Do $ (HS.Generator HS.noLoc 
                                 (case typ of
                                    ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                    ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType ptyp)))
                                 (HS.App (HS.Var $ identI "newRefP") (HS.Var $ HS.UnQual $ HS.Ident "res'"))) : tafter))
        return [HS.Qualifier $ texp]                          
tStmts (ABS.AnnStm _ (ABS.SDecAss typ ident@(ABS.LIdent (_,var)) (ABS.ExpE eexp@(ABS.ThisSyncMethCall _ _))) : rest) canReturn = do
  addToScope ident typ -- add to scope, but it's lazy monad, otherwise use Monad.State.Strict
  fscope <- funScope -- so we have to force to WHNF with a lookup
  case M.lookup ident fscope of -- has to be a lookup and not a member, to force to WHNF
      Just _ -> do
        tafter <- tStmts rest canReturn
        texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
                   (HS.Do $ (HS.Generator HS.noLoc 
                                 (case typ of
                                    ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                    ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType ptyp)))
                                 (HS.App (HS.Var $ identI "newRefP") (HS.Var $ HS.UnQual $ HS.Ident "res'"))) : tafter))
        return [HS.Qualifier $ texp]                          

tStmts (ABS.AnnStm as (ABS.SAss ident@(ABS.LIdent (p,var)) exp@(ABS.ExpE eexp@(ABS.SyncMethCall _ _ _))) : rest) canReturn = do
  fscope <- funScope
  (cscope,_,_,_,_,_) <- ask
  case M.lookup ident fscope of
      Just _ -> do
        tafter <- tStmts rest canReturn
        texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
          (HS.Do $
            (HS.Qualifier $ HS.App (HS.App (HS.Var $ identI "writeRefP") (HS.Var $ HS.UnQual $ HS.Ident var)) (HS.Var $ HS.UnQual $ HS.Ident "res'"))
            : tafter))
        return [HS.Qualifier $ texp]
      Nothing -> 
        case M.lookup ident cscope of -- maybe it is in the class scope
          Just _t -> tStmts ((ABS.AnnStm as (ABS.SFieldAss ident exp)) :rest) canReturn -- then normalize it to a field ass
          Nothing -> errorPos p (var ++ " not in scope or cannot modify the variable")


tStmts (ABS.AnnStm as (ABS.SAss ident@(ABS.LIdent (p,var)) exp@(ABS.ExpE eexp@(ABS.ThisSyncMethCall _ _))) : rest) canReturn = do
  fscope <- funScope
  (cscope,_,_,_,_,_) <- ask
  case M.lookup ident fscope of
      Just _ -> do
        tafter <- tStmts rest canReturn
        texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
          (HS.Do $
            (HS.Qualifier $ HS.App (HS.App (HS.Var $ identI "writeRefP") (HS.Var $ HS.UnQual $ HS.Ident var)) (HS.Var $ HS.UnQual $ HS.Ident "res'"))
            : tafter))
        return [HS.Qualifier $ texp]
      Nothing -> 
        case M.lookup ident cscope of -- maybe it is in the class scope
          Just _t -> tStmts ((ABS.AnnStm as (ABS.SFieldAss ident exp)) :rest) canReturn -- then normalize it to a field ass
          Nothing -> errorPos p (var ++ " not in scope or cannot modify the variable")

tStmts (ABS.AnnStm as (ABS.SFieldAss ident@(ABS.LIdent (p,var)) (ABS.ExpE eexp@(ABS.SyncMethCall _ _ _))) : rest) canReturn = do
  (clsScope, _, _, cls,_,_) <- ask
  tafter <- tStmts rest canReturn
  texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
        (HS.Do $                         
               (HS.Qualifier (HS.Paren $ HS.App (HS.Var $ identI "join") $ HS.Paren $ 
                          (HS.App (HS.App
                                 (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "set'") (HS.Lit $ HS.Int $ toInteger $ M.findIndex ident clsScope))
                                 (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "v__", HS.PVar $ HS.Ident "c__"] $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "c__") [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower cls ++ "_" ++ var) (HS.Var $ HS.UnQual $ HS.Ident "v__")]))
                             (HS.Var $ HS.UnQual $ HS.Ident "res'"))
                           (HS.Var (HS.UnQual $ HS.Ident "this")))))
               : tafter)) -- paren are necessary here
  return [HS.Qualifier $ texp]

tStmts (ABS.AnnStm _ (ABS.SFieldAss ident@(ABS.LIdent (_,var)) (ABS.ExpE eexp@(ABS.ThisSyncMethCall _ _))) : rest) canReturn = do
  (clsScope, _, _, cls,_,_) <- ask
  tafter <- tStmts rest canReturn
  texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "res'"] $
        (HS.Do $                         
               (HS.Qualifier (HS.Paren $ HS.App (HS.Var $ identI "join") $ HS.Paren $ 
                          (HS.App (HS.App
                                 (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "set'") (HS.Lit $ HS.Int $ toInteger $ M.findIndex ident clsScope))
                                 (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "v__", HS.PVar $ HS.Ident "c__"] $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "c__") [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower cls ++ "_" ++ var) (HS.Var $ HS.UnQual $ HS.Ident "v__")]))
                             (HS.Var $ HS.UnQual $ HS.Ident "res'"))
                           (HS.Var (HS.UnQual $ HS.Ident "this")))))
               : tafter)) -- paren are necessary here
  return [HS.Qualifier $ texp]

tStmts (ABS.AnnStm _ (ABS.SExp (ABS.ExpE eexp@(ABS.SyncMethCall _ _ _))) : rest) canReturn = do
  tafter <- tStmts rest canReturn
  texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PWildCard] $ (HS.Do $ tafter))                         
  return [HS.Qualifier $ texp]

tStmts (ABS.AnnStm _ (ABS.SExp (ABS.ExpE eexp@(ABS.ThisSyncMethCall _ _))) : rest) canReturn = do
  tafter <- tStmts rest canReturn
  texp <- tEffExpStmt eexp False (Just $ HS.Lambda HS.noLoc [HS.PWildCard] $ (HS.Do $ tafter))                         
  return [HS.Qualifier $ texp]



tStmts (ABS.AnnStm _ (ABS.SAwait g) : rest) canReturn = do
  (_,_,_, cls,isInit,_) <- ask
  if isInit 
   then error "Await statements not allowed inside init block"
   else do

     let (fs,as) = splitGuard g
     -- OPTIMIZATION by nubbing same futures
     let nfs = nub fs -- TODO: the nubbing could be better e.g. FutGuard "f" == FutFieldGuard "f" iff no other local f
     -- the ordering of fs does not matter that much: semantically is equivalent, and at runtime yields to the final same result
     tfs <- mapM (\ g -> runExpr (tAwaitGuard g cls) False Nothing) nfs
     tas <- if length as > 0
           then liftM Just $ runExpr (tAwaitGuard (foldl1 (\ (ABS.ExpGuard acc) (ABS.ExpGuard exp) -> -- trick to comb all attrsguards: & -> &&
                                                               ABS.ExpGuard $ acc `ABS.EAnd` exp) as) cls) False Nothing
           else return Nothing

     tafter <- tStmts rest canReturn -- TODO: maybe? scope bug here because tafter is run before the await, turning a futurefield to futurelocal

     return [HS.Qualifier $ foldr (\ texp tacc -> do
              (HS.App (HS.Var (identI "join")) 
                     (HS.Paren $ HS.InfixApp 
                            (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "await'") (HS.Var $ HS.UnQual $ HS.Ident "this"))
                               tacc)
                            (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>") texp))
              ) (HS.Do tafter) (tfs ++ maybe [] return tas)]

     -- texp <- runExpr (tAwaitGuard g cls) False Nothing
     -- return $ [HS.Qualifier $ HS.App (HS.Var (identI "join")) 
     --                 (HS.Paren $ HS.InfixApp 
     --                        (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "await'") (HS.Var $ HS.UnQual $ HS.Ident "this"))
     --                           (HS.Do tafter))
     --                        (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>") texp)]
     where
       splitGuard g = splitGuard' g ([],[])
       splitGuard' g (fs,as)= case g of
                                ABS.FutFieldGuard _ -> (g:fs,as)
                                ABS.FutGuard _ -> (g:fs,as)
                                ABS.ProGuard _ -> (g:fs,as)
                                ABS.ProFieldGuard _ -> (g:fs,as)
                                ABS.ExpGuard _ -> (fs,g:as)
                                ABS.AndGuard gl gr -> let 
                                         (fsl,asl) = splitGuard gl
                                         (fsr,asr) = splitGuard gr 
                                    in (fsl++fs++fsr,asl++as++asr)

tStmts (ABS.AnnStm _ ABS.SSuspend : rest) canReturn = do
  tafter <- tStmts rest canReturn
  return [HS.Qualifier $ HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "suspend'") (HS.Var $ HS.UnQual $ HS.Ident "this")) (HS.Do tafter)]

tStmts (ABS.AnnStm _ (ABS.SIf pexp stm): rest) canReturn  = do
  texp <- tPureExpStmt pexp
  tthen <- tBlock ((case stm of
                     ABS.AnnStm _ (ABS.SBlock stmts) ->  stmts 
                     stmt -> [stmt]) ++ 
                   [ABS.AnnStm [] $ ABS.SExp $ ABS.ExpP $ ABS.EVar $ ABS.LIdent ((1,1), "return'")]
                  )
                  False
  tafter <- tStmts rest canReturn
  return [HS.Qualifier $ HS.App (HS.App 
                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenM'") texp)
                                       (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "return'"] tthen))
          (HS.Do tafter)]

tStmts (ABS.AnnStm _ (ABS.SIfElse pexp stm_then stm_else) : rest) canReturn = do
  texp <- tPureExpStmt pexp
  tthen <- tBlock ((case stm_then of
                     ABS.AnnStm _ (ABS.SBlock stmts) ->  stmts 
                     stmt -> [stmt]) ++
                   [ABS.AnnStm [] $ ABS.SExp $ ABS.ExpP $ ABS.EVar $ ABS.LIdent ((1,1), "return'")]
                  ) False
  telse <- tBlock ((case stm_else of
                     ABS.AnnStm _ (ABS.SBlock stmts) ->  stmts 
                     stmt -> [stmt]) ++
                   [ABS.AnnStm [] $ ABS.SExp $ ABS.ExpP $ ABS.EVar $ ABS.LIdent ((1,1), "return'")])
                  False
  tafter <- tStmts rest canReturn
  return [HS.Qualifier $ HS.App (HS.App
                                (HS.App
                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM'") texp)
                               (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "return'"] tthen))
                          (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "return'"] telse))
         (HS.Do tafter)]

tStmts (ABS.AnnStm _ (ABS.SWhile pexp stm) : rest) canReturn = do
  texp <- tPureExpStmt pexp
  tblock <- tBlock ((case stm of
                     ABS.AnnStm _ (ABS.SBlock stmts) ->  stmts 
                     stmt -> [stmt]) ++
                   [ABS.AnnStm [] $ ABS.SExp $ ABS.ExpP $ ABS.EVar $ ABS.LIdent ((1,1), "return'")]) False
  tafter <- tStmts rest canReturn
  return [HS.Qualifier $ HS.App (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "while'") texp) 
                                       (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "return'"] tblock)) (HS.Do tafter)]              


tStmts (ABS.AnnStm _ ABS.SSkip : rest) canReturn = tStmts rest canReturn -- ignore skip
tStmts (ABS.AnnStm _ stmt:rest) canReturn = do
    s <- tStmt stmt
    r <- tStmts rest canReturn
    return (s++r) -- can return multiple HS.Stmt

-- | Translating a single ABS-statement
tStmt :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => ABS.Stm -> StmtM [HS.Stmt]
-- tStmt (ABS.SExp (ABS.ExpE (ABS.AsyncMethCall pexp method args))) = do
--   texp <- runExpr $ tSyncOrAsync "^!!" pexp method args
--   return $ [HS.Qualifier texp]

-- tStmt (ABS.SExp (ABS.ExpE (ABS.ThisAsyncMethCall method args))) = do
--  (_,_,_,_,_,meths) <- ask
--  if method `elem` meths
--   then --  it is actually a method (interface-declared), treat is same as above
--      tStmt (ABS.SExp (ABS.ExpE (ABS.AsyncMethCall (ABS.ELit $ ABS.LThis) method args)))
--   else do
--     -- it is a non-method, thus do not wrap the object with the existential type of the interface
--     texp <- runExpr $ tNonSyncOrAsync "^!!" method args
--     return $ [HS.Qualifier texp]

tStmt (ABS.SExp (ABS.ExpP (ABS.EVar (ABS.LIdent (_,"return'"))))) = return [HS.Qualifier (HS.Var $ HS.UnQual $ HS.Ident "return'")] -- tying-the-knot for if/while
tStmt (ABS.SExp pexp) = do
  texp <- case pexp of  -- TODO: have to force to WHNF
           ABS.ExpE eexp -> tEffExpStmt eexp True Nothing
           ABS.ExpP texp -> tPureExpStmt texp
  return [HS.Qualifier texp]

tStmt (ABS.SBlock stmts) = do
  tblock <- tBlock stmts False
  return [HS.Qualifier tblock]

tStmt (ABS.SThrow pexp) = do
  texp <- tPureExpStmt pexp
  return [HS.Qualifier (HS.App 
                              (HS.Var (HS.UnQual $ HS.Ident "throw'"))
                              texp
                       )]  -- takes a pureexp to throw


tStmt (ABS.SAssert pexp) = do
  texp <- tPureExpStmt pexp
  return [HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "assert'") 
                          texp)]

tStmt (ABS.SPrint pexp) = do
  texp <- tPureExpStmt pexp
  return [HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "println") 
                          texp)]

tStmt (ABS.SDec typ ident@(ABS.LIdent (p,var))) = -- just rewrites it to Interface x=null, adds also to current scope
    if isInterface typ 
    then tStmt (ABS.SDecAss typ ident (ABS.ExpP $ ABS.ELit $ ABS.LNull)) 
    else case typ of
       -- fut is allowed to be uninitialized
       ABS.TGen (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Fut"))]) _ -> do
                    addToScope ident typ
                    return [HS.Generator HS.noLoc
                                  (HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType typ))) (HS.App (HS.Var $ identI "newRef") (HS.App (HS.Var $ identI "empty_fut") (HS.Var $ HS.UnQual $ HS.Ident "this")))]
       ABS.TGen (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Promise"))]) _ -> do
                    addToScope ident typ
                    return [HS.Generator HS.noLoc
                                  (HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType typ))) (HS.App (HS.Var $ identI "newRef") (HS.App (HS.Var $ identI "empty_pro") (HS.Var $ HS.UnQual $ HS.Ident "this")))]
       _ -> warnPos p (var ++ " is ADT and has to be initialized") (do
                                                                    addToScope ident typ
                                                                    return [HS.Generator HS.noLoc
                                                                            (case typ of
                                                                               ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                                                               ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType ptyp))) (HS.App (HS.Var $ identI "newRef") (HS.Var $ identI "undefined"))]
                                                                  )
    -- TODO: remove the ident from the class attributes to check

tStmt (ABS.SDecAss typ ident@(ABS.LIdent (_,var)) exp) = do
  addToScope ident typ -- add to scope, but it's lazy monad, otherwise use Monad.State.Strict
  fscope <- funScope -- so we have to force to WHNF with a lookup
  case M.lookup ident fscope of -- has to be a lookup and not a member, to force to WHNF
      Just _ -> do
        texp <- case exp of
           ABS.ExpP pexp -> tPureExpStmt pexp
           ABS.ExpE eexp -> liftInterf ident eexp `ap` tEffExpStmt eexp False Nothing
        return [HS.Generator HS.noLoc 
                (case typ of
                   ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                   ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (HS.TyApp (HS.TyCon $ identI "IORef") (tType ptyp)))
                (HS.App (HS.Var $ identI "newRef") texp)]

tStmt (ABS.SAss ident@(ABS.LIdent (p,var)) exp) = do
  fscope <- funScope
  (cscope,_,_,_,_,_) <- ask
  case M.lookup ident fscope of
      Just _ -> do
        texp <- case exp of
           ABS.ExpP pexp -> tPureExpStmt pexp
           ABS.ExpE eexp -> liftInterf ident eexp `ap` tEffExpStmt eexp False Nothing
        return [HS.Qualifier $ HS.App
                -- lhs
                (HS.App (HS.Var $ identI "writeRef") (HS.Var $ HS.UnQual $ HS.Ident var))
                -- rhs
                texp
                -- disabled, no need to explicitly put type, because the type is already declared upon newIORef
                -- (case t of -- maybe wrap rhs with its type
                --     ABS.TUnderscore -> texp -- just infer the type
                --     ptyp -> HS.ExpTypeSig HS.noLoc texp (tType ptyp))
               ]
      Nothing -> 
        case M.lookup ident cscope of -- maybe it is in the class scope
          Just _t -> tStmt (ABS.SFieldAss ident exp) -- then normalize it to a field ass
          Nothing -> errorPos p (var ++ " not in scope or cannot modify the variable")
                                                                             
tStmt (ABS.SFieldAss ident@(ABS.LIdent (_,var)) exp) = do
  (clsScope, _, _, cls,_,_) <- ask
  texp <- case exp of
           ABS.ExpP pexp -> tPureExpStmt pexp
           ABS.ExpE eexp -> liftInterf ident eexp `ap` tEffExpStmt eexp False Nothing

  return [HS.Qualifier (HS.Paren $ HS.App (HS.Var $ identI "join") $ HS.Paren $ HS.InfixApp 
                          (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "set'") (HS.Lit $ HS.Int $ toInteger $ M.findIndex ident clsScope))
                                 (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident "v__", HS.PVar $ HS.Ident "c__"] $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "c__") [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower cls ++ "_" ++ var) (HS.Var $ HS.UnQual $ HS.Ident "v__")]))
                          (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>")
                          (HS.InfixApp   
                           texp
                           (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                           (HS.App (HS.Var (HS.UnQual $ HS.Ident "pure")) (HS.Var (HS.UnQual $ HS.Ident "this")))
                          )
                          
                       )] -- paren are necessary here

tStmt (ABS.SGive pro val) = do
  tpro <- tPureExpStmt pro
  tval <- tPureExpStmt val
  return [HS.Qualifier $ HS.App (HS.Paren $ HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "pro_give'")
                                       (HS.Paren tpro))
                          tval] -- paren are necessary here
  

tStmt (ABS.STryCatchFinally try_stm cbranches mfinally) = do
  tfin <- case mfinally of
           ABS.NoFinally -> return id
           ABS.JustFinally fstm -> do
                         tblock <- tBlock [fstm] False 
                         return $ \ try_catch -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "finally'")  try_catch) tblock
                                   
  ttry <- tBlock [try_stm] False
  tbranches <- mapM (\ (ABS.CatchBranc pat cstm) -> do
                      tcstm <- tBlock [cstm] False
                      return $ HS.App
                              (HS.Con (identI "Handler"))
                              -- TODO: if hse support lambdacase, it will lead to cleaner code
                              (HS.Lambda  HS.noLoc (case pat of
                                                      -- a catch-all is a wrapped someexception
                                                      ABS.PUnderscore -> [HS.PApp 
                                                                               (identI "SomeException")
                                                                         [HS.PWildCard]]
                                                                        -- otherwise generate a pattern-match catch_all
                                                      _ -> [HS.PVar $ HS.Ident "__0"]
                                                   )
                               (case pat of
                                  -- wrap the normal returned expression in a just
                                  ABS.PUnderscore -> HS.InfixApp (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>") (HS.Paren $ tcstm)
                                  _ -> HS.Case (HS.Var $ HS.UnQual $ HS.Ident "__0")
                                      [HS.Alt HS.noLoc (tPattern pat)
                                       -- wrap the normal returned expression in a just
                                       (HS.UnGuardedAlt (HS.InfixApp (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>") (HS.Paren $ tcstm))) (HS.BDecls []),
                                       -- pattern match fail, return nothing
                                       HS.Alt HS.noLoc HS.PWildCard (HS.UnGuardedAlt $ (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Con $ HS.UnQual $ HS.Ident "Nothing"))) (HS.BDecls [])])))
              cbranches

  return [HS.Qualifier
             -- (optionally) wrap the try-catch in a finally block
             (tfin
              (HS.App
                     (HS.App (HS.Var (HS.UnQual $ HS.Ident "catches'"))
                        ttry)
                     (HS.List tbranches)))]



liftInterf :: ABS.LIdent -> ABS.EffExp -> StmtM (HS.Exp -> HS.Exp)
liftInterf ident exp@(ABS.New _ _) = liftInterf' ident exp
liftInterf ident exp@(ABS.NewLocal _ _) = liftInterf' ident exp
liftInterf ident exp@(ABS.Spawns _ _ _) = liftInterf' ident exp
liftInterf ident exp = return id
liftInterf' ident@(ABS.LIdent (p,var)) exp =  do
  fscope <- funScope
  (cscope, _ , _, _,_,_)<- ask
  return $ case M.lookup ident (M.union fscope cscope) of
      Nothing -> errorPos p $ "Identifier " ++ var ++ " cannot be resolved from scope"
      Just (ABS.TUnderscore) -> errorPos p $ "Cannot infer interface type for variable" ++ var
      Just (ABS.TSimple (ABS.QTyp qids)) -> HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QTypeSegmen (ABS.UIdent (_,iid))) -> iid) (last qids))
                                            (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$!>")
      Just _ -> errorPos p $ var ++ " not of interface type"


-- | this scope is the oo-scope: it does not allow re-declaration
-- pure-scope is done with lambdas, so it allows re-declaration
addToScope :: ABS.LIdent -> ABS.Type -> StmtM ()
addToScope var@(ABS.LIdent (p,pid)) typ = do
  (topscope:restscopes) <- lift get
  if (any (\ scope -> var `M.member` scope) restscopes)
    then errorPos p $ pid ++ " already defined in an outer scope"
    else lift $ put $ M.insertWith (const $ const $ errorPos p $ pid ++ " already defined in this scope") var typ topscope  : restscopes




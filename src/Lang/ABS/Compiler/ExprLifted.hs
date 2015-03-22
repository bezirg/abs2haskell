module Lang.ABS.Compiler.ExprLifted
    (tPureExpStmt
    ,tEffExpStmt
    ,runExpr
    ,tAwaitGuard
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import Lang.ABS.Compiler.Expr (tPattern, joinSub, tType, tTypeOrTyVar)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import Control.Monad.Trans.Reader (ask, runReader)
import Data.List (nub, findIndices)
import qualified Data.Map as M
import Data.Foldable (foldlM, find)
import Control.Monad (liftM)

-- tPureExpStmt is a pure expression in the statement world
-- it does 3 things:
-- 1) if a sub-expression reads this fields it *wraps* the whole expression in readThis (by tPureExpWrap)
-- 2) if a sub-expression is pure it lifts it with return/pure (by tPureExp')
-- 3) if a sub-expression reads local-variables it calls readIORef on them (by tPureExp')
tPureExpStmt :: (?moduleTable::ModuleTable, ?moduleName::ABS.QType) => ABS.PureExp -> StmtM HS.Exp
tPureExpStmt pexp = do
  (_,_,_,cls, _) <- ask
  runExpr (tPureExpWrap pexp cls)

tPureExpWrap pexp cls = do
      vcscope <- visible_cscope
      let thisFields = collectVars pexp vcscope
      texp <- tPureExp' pexp []
      -- translate the pure expression with no typevars since stateful code cannot contain type variables (par polymorphism)
      -- TODO: no type variables, has to be changed for polymorphic methods
      return $ if null thisFields
               then texp  --  rhs  
               else HS.Paren $ HS.InfixApp 
                      (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                      (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                      (HS.Lambda HS.noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                            map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                         (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))
                                          ] texp)

tEffExpStmt :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => ABS.EffExp -> StmtM HS.Exp
tEffExpStmt eexp = do
  (_,_,_,cls, _) <- ask
  runExpr (tEffExpWrap eexp cls)


-- | tEffExpWrap is a wrapper arround tEffExp' that adds a single read to the object pointer to collect the necessary fields
-- it is supposed to be an optimization compared to reading each time the field at the place it is accessed
tEffExpWrap eexp cls = do
      vcscope <- visible_cscope
      let argsExps = case eexp of
                         ABS.Get pexp -> [pexp]
                         ABS.New _ pexps  -> pexps
                         ABS.NewLocal _ pexps -> pexps
                         ABS.SyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                         ABS.ThisSyncMethCall _ pexps -> pexps
                         ABS.AsyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                         ABS.ThisAsyncMethCall _ pexps -> pexps
      let thisFields = concatMap ((flip collectVars) vcscope) argsExps
      texp <- tEffExp' eexp
      return $ if null thisFields
               then texp
               else -- readObject this >>= \ Class1 { record bindings   } ->
                   HS.Paren $ HS.InfixApp 
                   (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                   (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
                   (HS.Lambda HS.noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                         map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                      (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))]
                    texp)


-- this is the "statement-lifted" version of tPureExp
tPureExp' :: (?moduleTable::ModuleTable,
             ?moduleName::ABS.QType)=>
            ABS.PureExp 
          -> [ABS.UIdent] -- ^ TypeVarsInScope
          -> ExprLiftedM HS.Exp

tPureExp' (ABS.If predE thenE elseE) tyvars = do
  tpred <- tPureExp' predE tyvars
  tthen <- tPureExp' thenE tyvars
  telse <- tPureExp' elseE tyvars
  return $ HS.Paren $ (HS.App (HS.App (HS.App 
                                                       (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM") -- don't lift ifthenelseM to applicative, bcs it is already lifted
                                                       tpred)
                                    tthen)
                       telse)

-- | translate it into a lambda exp
tPureExp' (ABS.Let (ABS.Par ptyp pid@(ABS.LIdent (_,var))) eqE inE) tyvars = do
  tin <- -- local (\ (fscope, cscope,mscope,interf,isInit) -> (M.insert pid ptyp fscope, -- add to function scope of the "in"-expression
        --                                     cscope,mscope,interf,isInit)) $ 
        -- turned off, let to haskell the pure scoping
        tPureExp' inE tyvars
  teq <- tPureExp' eqE tyvars
  let pat = HS.PVar $ HS.Ident var
  return $ HS.Paren $ HS.InfixApp -- apply the created lamdba to the equality expr
             teq
             (HS.QVarOp $ HS.UnQual $ HS.Symbol ">>=")
             (HS.Lambda HS.noLoc
                [if ptyp == ABS.TUnderscore
                 then pat -- infer the parameter type
                 else HS.PatTypeSig HS.noLoc pat (tTypeOrTyVar tyvars ptyp)] -- wrap with an explicit type annotation -- bound variable
                tin) 

-- NOTE: leave the case for now. It might work out of the box
tPureExp' (ABS.Case matchE branches) tyvars = do
  tmatch <- tPureExp' matchE tyvars
  (fscope,cscope,_,_,_) <- ask
  let scope = fscope `M.union` cscope
  case matchE of
    ABS.EVar ident | M.lookup ident scope == Just (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (undefined, "Exception"))])) -> tCaseException tmatch branches
    ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen tid]) | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen tid]) _ | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    _ -> do
        talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
                        texp <- tPureExp' pexp tyvars
                        return $ HS.Alt HS.noLoc (tPattern pat) (HS.UnGuardedAlt texp) (HS.BDecls []))
                branches
        return $ HS.Case tmatch talts

  where
    tCaseException tmatch brs = do
      talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
             texp <- tPureExp' pexp tyvars
             return $ HS.App (HS.Con (identI "PHandler"))
                    -- TODO: if hse support lambdacase, it will lead to cleaner code
                    (HS.Lambda  HS.noLoc (case pat of
                                            -- a catch-all is a wrapped someexception
                                            ABS.PUnderscore -> [HS.PApp 
                                                                     (identI "SomeException")
                                                               [HS.PWildCard]]
                                            -- otherwise generate a pattern-match catch_all
                                            _ -> [HS.PVar $ HS.Ident "__0"])
                     (case pat of
                        -- wrap the normal returned expression in a just
                        ABS.PUnderscore -> HS.App (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.Paren texp)
                        _ -> HS.Case (HS.Var $ HS.UnQual $ HS.Ident "__0")
                            [-- wrap the normal returned expression in a just
                             HS.Alt HS.noLoc (tPattern pat) (HS.UnGuardedAlt (HS.App (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.Paren texp))) (HS.BDecls [])
                            --,pattern match fail, return nothing
                            ,HS.Alt HS.noLoc HS.PWildCard (HS.UnGuardedAlt $ HS.Con $ HS.UnQual $ HS.Ident "Nothing") (HS.BDecls [])]
                     ))) brs
      return $ HS.App (HS.App (HS.Var (identI "caseEx")) tmatch) (HS.List talts)


tPureExp' (ABS.EFunCall (ABS.LIdent (_,cid)) args) tyvars = liftM (case find (\ m -> moduleName m == ?moduleName) ?moduleTable of
        Just (ModuleInfo {fimports = is})  -> case find (\case 
                                                        (ABS.AnyIden (ABS.LIdent (_,cid'))) -> cid == cid'
                                                        _ -> False) is of
                                               Nothing -> id
                                               Just _ -> HS.App (HS.Var (identI "liftIO")) . (HS.App (HS.Var (identI "join")))
        Nothing -> error "ModuleInfo not found for this module") $
                                               liftM HS.Paren $ foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp' nextArg tyvars
                                               return $ HS.Paren $ HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") targ) -- intersperse "<*>", aka "ap" for applicative
                                            (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") (HS.Var $ HS.UnQual $ HS.Ident cid)) -- lift the function
                                            args

tPureExp' (ABS.EQualFunCall (ABS.TTyp tsegs) (ABS.LIdent (_,cid)) args) tyvars = liftM HS.Paren $ foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp' nextArg tyvars
                                               return $ HS.Paren $ HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") targ) -- intersperse "<*>", aka "ap" for applicative
                                            (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") (HS.Var $ HS.Qual (HS.ModuleName (joinTTypeIds tsegs)) $ HS.Ident cid)) -- lift the function
                                            args


-- normalize
tPureExp' (ABS.ENaryFunCall fun@(ABS.LIdent (p,_)) args) tyvars = tPureExp' (ABS.EFunCall fun 
                                                          -- transform it to one list-argument
                                                          [foldr (\ arg l -> ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Cons"))]) [arg, l]) -- arg:rest
                                                                 (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Nil"))])) -- []
                                                                 args]) tyvars

-- normalize
tPureExp' (ABS.ENaryQualFunCall ttyp fun@(ABS.LIdent (p,_)) args) tyvars = tPureExp' (ABS.EQualFunCall ttyp fun 
                                                          -- transform it to one list-argument
                                                          [foldr (\ arg l -> ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Cons"))]) [arg, l]) -- arg:rest
                                                                 (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Nil"))])) -- []
                                                                 args]) tyvars

         

-- it does some extras than pure equality
-- it liftA2 the (==) operator through the applicative style
-- it surrounds the explicit type hints in ABS _o (tType t)
-- it also implements equality checking for "this"

-- constants
tPureExp' (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LNull)) _tyvars = return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp' (ABS.EEq (ABS.ELit ABS.LThis) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp' (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ HS.Con $ HS.UnQual $ HS.Ident "False"

tPureExp' (ABS.EEq pnull@(ABS.ELit ABS.LNull) pvar@(ABS.EVar ident@(ABS.LIdent (p,str)))) _tyvars = do
  tnull <- tPureExp' pnull _tyvars
  tvar <- tPureExp' pvar _tyvars
  (fscope, _,_, _,_) <- ask
  case M.lookup ident fscope of -- check the type of the right var
    Just t -> if isInterface t
             then return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                              (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                              (HS.ExpTypeSig HS.noLoc tnull (wrapTypeToABSMonad (tType t))))
                      (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                      (HS.ExpTypeSig HS.noLoc tvar (wrapTypeToABSMonad (tType t)))
             else errorPos p "cannot equate datatype to null"
    Nothing -> errorPos p $ str ++ " not in scope or not object variable"

tPureExp' (ABS.EEq pthis@(ABS.ELit ABS.LThis) pvar@(ABS.EVar ident@(ABS.LIdent (p,str)))) _tyvars = do
  tthis <- tPureExp' pthis _tyvars
  tvar <- tPureExp' pvar _tyvars
  (fscope, _,_, _,_) <- ask
  case M.lookup ident fscope of -- check the type of the right var
    Just t -> if isInterface t
             then return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                              (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                              (HS.ExpTypeSig HS.noLoc tthis (wrapTypeToABSMonad (tType t))))
                      (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                      (HS.ExpTypeSig HS.noLoc tvar (wrapTypeToABSMonad (tType t)))
             else errorPos p "cannot equate datatype to this"
    Nothing -> errorPos p $ str ++ " not in scope or not object variable"
  
-- commutative
tPureExp' (ABS.EEq pexp pnull@(ABS.ELit (ABS.LNull))) _tyvars = tPureExp' (ABS.EEq pnull pexp) _tyvars

-- commutative
tPureExp' (ABS.EEq pexp pthis@(ABS.ELit (ABS.LThis))) _tyvars = tPureExp' (ABS.EEq pthis pexp) _tyvars

tPureExp' (ABS.EEq pvar1@(ABS.EVar ident1@(ABS.LIdent (p1, str1))) pvar2@(ABS.EVar ident2@(ABS.LIdent (p2,str2)))) _tyvars = do
  tvar1 <- tPureExp' pvar1 _tyvars
  tvar2 <- tPureExp' pvar2 _tyvars
  (fscope, _,_, _,_) <- ask
  case M.lookup ident1 fscope of -- check the type of the right var
    Just t1 -> case M.lookup ident2 fscope of
                Just t2 -> if isInterface t1 && isInterface t2
                          then case joinSub t1 t2 of
                              Just t ->
                                return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                                            (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                                                            (HS.ExpTypeSig HS.noLoc tvar1 (wrapTypeToABSMonad (tType t))))
                                           (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
                                           (HS.ExpTypeSig HS.noLoc tvar2 (wrapTypeToABSMonad (tType t)))
                              Nothing -> error ("cannot unify the interface " ++ str1 
                                               ++ " at position " ++ showPos p1 ++ " with interface " ++ str2 ++ " at position " ++ showPos p2)
                          -- treat them as both datatypes and let haskell figure out if there is a type mismatch
                          else return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                                           (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                                                           tvar1)
                                          (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
                                          tvar2
                Nothing -> return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                                           (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                                                           tvar1)
                                          (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
                                          tvar2
                          -- error $ str2 ++ " not in scope" -- todo: turn it into warning
    Nothing -> return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")
                                                           (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                                                           tvar1)
                                          (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
                                          tvar2
              -- error $ str1 ++ " not in scope" -- todo: turn it into warning

-- a catch-all for literals,constructors maybe coupled with vars
tPureExp' (ABS.EEq pexp1 pexp2) _tyvars = do
  texp1 <- tPureExp' pexp1 _tyvars
  texp2 <- tPureExp' pexp2 _tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol "==")  
                              (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                              texp1)
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
         texp2



-- normalizess to not . ==
tPureExp' (ABS.ENeq left right) tyvars = tPureExp' (ABS.ELogNeg $ ABS.EEq left right) tyvars


-- be careful to parenthesize infix apps
tPureExp' (ABS.EOr left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "||") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2

tPureExp' (ABS.EAnd left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "&&") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2

tPureExp' (ABS.ELt left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "<") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2

tPureExp' (ABS.ELe left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "<=") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.EGt left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol ">") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.EGe left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol ">=") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.EAdd left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "+") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.ESub left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "-") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.EMul left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "*") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2

tPureExp' (ABS.EDiv left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "/") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2


tPureExp' (ABS.EMod left right) tyvars = do
  tleft <- tPureExp' left tyvars
  tright <- tPureExp' right tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp 
                                         (HS.Var $ HS.UnQual $ HS.Symbol "%") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         tleft)                -- operand1
                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
                                  tright -- operand2

tPureExp' (ABS.ELogNeg pexp) tyvars = do
  texp <- tPureExp' pexp tyvars
  return $ HS.Paren $ HS.InfixApp 
                             (HS.Var $ HS.UnQual $ HS.Ident "not") -- operator
                             (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                             texp                -- operand1

tPureExp' (ABS.EIntNeg pexp) tyvars = do
  texp <- tPureExp' pexp tyvars
  return $ HS.Paren $ HS.InfixApp 
                             (HS.Var $ HS.UnQual $ HS.Ident "negate") -- operator
                             (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                             texp                -- operand1

tPureExp' (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Nil"))])) _ = 
    return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $  HS.Con $ HS.Special HS.ListCon -- for the translation to []

tPureExp' (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"EmptyMap"))])) _ = 
    return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ HS.Var $ HS.UnQual $ HS.Ident "empty" -- for the translation to Data.Map

tPureExp' (ABS.ESinglConstr (ABS.QTyp qids)) _ = return $
  let mids = init qids
      tid@(ABS.UIdent (_,sid)) = (\ (ABS.QTypeSegmen cid) -> cid) (last qids)
  in HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ if tid `elem` concatMap exceptions ?moduleTable
                                                     -- if is an exception constructor, replace it with its smart constructor
                                                     then HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ headToLower sid  
                                                     else
                                                         HS.Con $ (if null mids 
                                                                   then HS.UnQual 
                                                                   else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                                                                  ) $ HS.Ident sid

-- transform it to a function-call of the smart-contructor
tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Triple"))]) pexps) tyvars =  
    if length pexps == 3
    then liftM HS.Paren $ 
        foldlM (\ acc pexp -> do
                  texp <- tPureExp' pexp tyvars
                  return (HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") texp))
                   (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") (HS.Var $ HS.UnQual $ HS.Symbol "(,,)"))  pexps
    else errorPos p "wrong number of arguments to Triple"

tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Pair"))]) pexps) tyvars = 
    if length pexps == 2
    then liftM HS.Paren $ 
        foldlM (\ acc pexp -> do
                  texp <- tPureExp' pexp tyvars
                  return (HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") texp))
                   (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") (HS.Var $ HS.UnQual $ HS.Symbol "(,)"))  pexps
    else errorPos p "wrong number of arguments to Pair"

tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Cons"))]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp' pexp1 tyvars
  texp2 <- tPureExp' pexp2 tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.Special $ HS.Cons) -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")
                                         texp1) -- operand1
             (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
             texp2 -- operand2

tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Cons"))]) _) _ = errorPos p "wrong number of arguments to Cons"

tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"InsertAssoc"))]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp' pexp1 tyvars
  texp2 <- tPureExp' pexp2 tyvars
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") -- operator
                                         (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>") 
                                         texp1) -- operand1
             (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>")
             texp2              -- operand2

tPureExp' (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"InsertAssoc"))]) _) _ = errorPos p "wrong number of arguments to InsertAssoc"

tPureExp' (ABS.EParamConstr qids args) tyvars = do
    tcon <- tPureExp' (ABS.ESinglConstr qids) tyvars -- first translate the constructor
    liftM HS.Paren $ foldlM (\ acc nextArg -> do
              targ <- tPureExp' nextArg tyvars 
              return $ HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") targ) tcon args


tPureExp' (ABS.EQualVar (ABS.TTyp tsegs) (ABS.LIdent (_,pid))) _tyvars = -- todo: we cannot use any scope, so we have to lookup the other modules, to check if it is of an interface type (upcast it) or int (fromIntegral) 
    return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident pid
    -- we tread it as pure for now

tPureExp' (ABS.EVar var@(ABS.LIdent (_,pid))) _tyvars = do
    (fscope, cscope, mscope,_,_) <- ask
    return $ HS.Paren $ case M.lookup var fscope of
      Nothing -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ -- fields are read from the Wrap function, so they are pure
                case M.lookup var mscope of
                  Just (ABS.TSimple (ABS.QTyp ([ABS.QTypeSegmen (ABS.UIdent (_,"Int"))]))) -> 
                      HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident $ pid)
                  Just t -> (if isInterface t
                            then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                            else id)
                           (HS.Var $ HS.UnQual $ HS.Ident $ pid)
                  Nothing ->
                      case M.lookup var cscope of -- lookup in the cscope
                        -- if it of an int type, upcast it
                        Just (ABS.TSimple (ABS.QTyp ([ABS.QTypeSegmen (ABS.UIdent (_,"Int"))]))) -> 
                            HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid)
                        Just t -> (if isInterface t
                                  then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                                  else id)
                                 (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid)
                        Nothing -> HS.Var $ HS.UnQual $ HS.Ident pid -- error $ pid ++ " not in scope" -- TODO: this should be turned into warning
        -- if it of an int type, upcast it
      Just (ABS.TSimple (ABS.QTyp ([ABS.QTypeSegmen (ABS.UIdent (_,"Int"))]))) -> HS.InfixApp (HS.Var $ identI "fromIntegral") 
                                                                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>") 
                                                                                  (HS.App (HS.Var $ identI "readRef") (HS.Var $ HS.UnQual $ HS.Ident pid))
      Just t -> HS.Paren $ (if isInterface t
                           then HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>")  -- upcasting if it is of a class type
                           else id) 
               (HS.App (HS.Var $ identI "readRef") (HS.Var $ HS.UnQual $ HS.Ident pid))

tPureExp' (ABS.ELit lit) _ = return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ case lit of
                                    ABS.LStr str ->  HS.Lit $ HS.String str
                                    ABS.LInt i ->  HS.Lit $ HS.Int i
                                    ABS.LThis -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                    ABS.LNull -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "null")
                                    ABS.LThisDC -> HS.Var $ HS.UnQual $ HS.Ident "thisDC"

-- translate this.field
-- this is a trick for sync_call and async_call 
tPureExp' (ABS.EThis (ABS.LIdent (_,ident))) _ = return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") 
                                            (HS.Var $ HS.UnQual $ HS.Ident ("__" ++ ident))

-- this is the "statement-lifted" version of tEffExp
tEffExp' :: (?moduleTable::ModuleTable, ?moduleName::ABS.QType) => ABS.EffExp -> ExprLiftedM HS.Exp
tEffExp' (ABS.New (ABS.TSimple (ABS.QTyp qtids)) pexps) = tNewOrNewLocal "new" qtids pexps 
tEffExp' (ABS.New _ _) = error "Not valid class name"
tEffExp' (ABS.NewLocal (ABS.TSimple (ABS.QTyp qtids)) pexps) = tNewOrNewLocal "new_local" qtids pexps
tEffExp' (ABS.NewLocal _ _) = error "Not valid class name"


tEffExp' (ABS.SyncMethCall pexp (ABS.LIdent (_,method)) args) = tSyncOrAsync "sync" pexp method args
tEffExp' (ABS.AsyncMethCall pexp (ABS.LIdent (_,method)) args) = tSyncOrAsync "async" pexp method args

-- normalize
tEffExp' (ABS.ThisSyncMethCall method args) = tEffExp' (ABS.SyncMethCall (ABS.ELit $ ABS.LThis) method args)
-- normalize
tEffExp' (ABS.ThisAsyncMethCall method args) = tEffExp' (ABS.AsyncMethCall (ABS.ELit $ ABS.LThis) method args)

tEffExp' (ABS.Get pexp) = do
  texp <- tPureExp' pexp []
  return $ HS.Paren $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "get") (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<") texp

-- | shorthand generator, because new and new local are similar
tNewOrNewLocal :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => String -> [ABS.QTypeSegment] -> [ABS.PureExp] -> ExprLiftedM HS.Exp
tNewOrNewLocal newOrNewLocal qtids args = do 
  targs <- foldlM
           (\ acc pexp -> do
              texp <- tPureExp' pexp []
              return $ HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") texp)
           (HS.Paren (HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") (HS.Var  
                  ((let mids = init qtids
                    in
                      if null mids
                      then HS.UnQual
                      else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                   (HS.Ident $ "__" ++ headToLower ( (\ (ABS.QTypeSegmen (ABS.UIdent (_,cid))) -> cid) (last qtids))))))) args -- wrap with the class-constructor function
  return $ HS.Paren $ HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident newOrNewLocal) 
             (HS.QVarOp $ HS.UnQual $ HS.Symbol "=<<")
             targs

-- | shorthand generator, because sync and async are similar
tSyncOrAsync :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => String -> ABS.PureExp -> String -> [ABS.PureExp] -> ExprLiftedM HS.Exp
tSyncOrAsync syncOrAsync pexp method args = do
  (_,_,_,_, isInit) <- ask
  if (isInit && syncOrAsync == "sync")
    then error "Synchronous method calls are not allowed inside init block"
    else do
      texp <- tPureExp' pexp [] -- the callee object (usually, the this)
      tapp <- foldlM                                -- the method's arguments
             (\ acc arg -> do
                targ <- tPureExp' arg []
                return $ HS.Paren (HS.InfixApp acc (HS.QVarOp $ HS.UnQual $ HS.Symbol "<*>") targ))
             (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_" ++ syncOrAsync) (HS.QVarOp $ HS.UnQual $ HS.Symbol "<$>") texp))
             args
      return $ HS.Paren $ HS.App (HS.Var (identI "join")) tapp


tAwaitGuard :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => ABS.Guard -> String -> ExprLiftedM HS.Exp
-- NOTE: both VarGuard and FieldGuard contain futures, but "this.f?" is distinguished as FieldGuard to take into account Cosimo's consideration
-- awaitguard: f?
tAwaitGuard (ABS.VarGuard ident) _cls = do
  vcscope <- visible_cscope
  if ident `M.member` vcscope   -- if it is statically scoped to a field (pointing implicitly to a field) then rewrite it to a field
   then tAwaitGuard (ABS.FieldGuard ident) _cls
   else do
    texp <- tPureExp' (ABS.EVar ident) [] -- treat the input as variable
    return $ HS.Paren $ HS.InfixApp
             (HS.Con $ HS.UnQual $ HS.Ident "FutureGuard")
             (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
             texp
                                             
-- fieldguard: this.f?
tAwaitGuard (ABS.FieldGuard (ABS.LIdent ident)) cls = error "Not implemented yet, take Cosimo's consideration into account"


-- guards with expressions: fields and/or local variables
-- if the expression contains no fields, then the guard can evaluate it only once and either block (show an error) or continue
-- if it contains fields, collect them to try them out whenever the object is mutated
tAwaitGuard (ABS.ExpGuard pexp) cls = do
  (_,cscope,_,_,_) <- ask
  vcscope <- visible_cscope
  let awaitFields = collectVars pexp vcscope
  texp <- tPureExpWrap pexp cls
  return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "pure") $ 
         HS.App (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ThisGuard") 
                   (HS.List (map (HS.Lit . HS.Int . toInteger) (findIndices ((\ (ABS.LIdent (_,field)) -> field `elem` awaitFields)) (M.keys cscope)))))
         texp

tAwaitGuard (ABS.AndGuard gl gr) cls = do
  tleft <- tAwaitGuard gl cls 
  tright <- tAwaitGuard gr cls
  return $ HS.Paren $ HS.InfixApp (HS.InfixApp (HS.Var $ HS.UnQual  $ HS.Symbol ":&:")
                                         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<$>")
                                         tleft)
                                       (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<*>")
                                       tright


interf :: ExprLiftedM String
interf = do 
  (_,_,_,i,_) <- ask
  return i


wrapTypeToABSMonad :: HS.Type -> HS.Type
wrapTypeToABSMonad t = (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual $ HS.Ident "ABS"))
                                        (HS.TyVar $ HS.Ident "_o"))
                                    t)

visible_cscope :: ExprLiftedM ScopeTable
visible_cscope = do
 (fscope, cscope, _, _,_) <- ask
 return $ cscope M.\\ fscope


runExpr :: ExprLiftedM a -> StmtM a
runExpr e = do
  fscope <- funScope
  (cscope,mscope,interf,_,isInit) <- ask
  return $ runReader e (fscope,cscope,mscope,interf,isInit)

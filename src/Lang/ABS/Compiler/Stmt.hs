module Lang.ABS.Compiler.Stmt 
    (tBlockWithReturn
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import Control.Monad.Trans.State (evalState, withState, put, get)
import Control.Monad.Trans.Reader (runReader, runReaderT, mapReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad (ap)

import Lang.ABS.Compiler.Expr

import qualified Data.Map as M
import Data.List (nub)

-- | method block or main block
-- can return and also pushes a new scope
tBlockWithReturn :: (?moduleTable :: ModuleTable) => [ABS.Stm] -> String -> ScopeTable -> [ScopeTable] -> String -> HS.Exp
tBlockWithReturn stmts cls clsScope scopes interfName = evalState (runReaderT 
                                                                    (tBlock stmts True)
                                                                    (clsScope, interfName, cls))
                                                        scopes
tBlock :: (?moduleTable :: ModuleTable) => [ABS.Stm] -> Bool -> StmtM HS.Exp
tBlock [] _canReturn = return $ eReturnUnit
tBlock stmts canReturn = do
  ts <- mapReaderT (withState (M.empty:)) $ tStmts stmts canReturn
  return $ HS.Do $ ts  ++
                         -- if the last stmt is an assignment, then add a return (R ())
                         -- 
                       (case last stmts of
                          ABS.SAss _ _ -> [HS.Qualifier eReturnUnit]
                          ABS.SExp _ ->  [HS.Qualifier eReturnUnit] -- although an expression has a value, we throw it away, since it must explicitly be returned
                          ABS.SFieldAss _ _ -> [HS.Qualifier eReturnUnit]
                          ABS.SDecAss _ _ _ ->  [HS.Qualifier eReturnUnit]
                          ABS.SWhile _ _ -> [HS.Qualifier eReturnUnit]
                          _ -> []
                       )

tStmts :: (?moduleTable :: ModuleTable) => [ABS.Stm] -> Bool -> StmtM [HS.Stmt]
tStmts (ABS.SReturn e:[]) True = tStmt (ABS.SExp e)
tStmts (ABS.SReturn _:_) _ = error "Return must be the last statement"
tStmts [] _canReturn = return $ []
tStmts (stmt:rest) canReturn = do
    s <- tStmt stmt
    r <- tStmts rest canReturn
    return (s++r) -- can return multiple HS.Stmt

tStmt :: (?moduleTable::ModuleTable) => ABS.Stm -> StmtM [HS.Stmt]
tStmt (ABS.SExp pexp) = do
  texp <- case pexp of  -- TODO: have to force to WHNF
           ABS.ExpE eexp -> tEffExpWrap eexp
           ABS.ExpP texp -> tPureExpWrap texp
  return [HS.Qualifier texp]

tStmt ABS.SSuspend = return [HS.Qualifier (HS.Var $ HS.UnQual $ HS.Ident "suspend")]

tStmt (ABS.SAwait g) = do
  (_,_,cls) <- ask
  texp <- runExpr $ tAwaitGuard g cls
  return $ [HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "await") texp]

tStmt (ABS.SBlock stmts) = do
  tblock <- tBlock stmts False
  return [HS.Qualifier tblock]

tStmt (ABS.SThrow pexp) = do
  texp <- tPureExpWrap pexp
  return [HS.Qualifier (HS.App 
                              (HS.Var (HS.UnQual $ HS.Ident "throw"))
                              texp
                       )]  -- takes a pureexp to throw

tStmt (ABS.SSkip) = return [HS.Qualifier $ HS.Var $ HS.UnQual $ HS.Ident "skip"]

tStmt (ABS.SIf pexp stm) = do
  texp <- tPureExpWrap pexp
  tblock <- tBlock [stm] False
  return [HS.Qualifier $ HS.App 
                (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenM") texp)
                tblock]

tStmt (ABS.SIfElse pexp stm_then stm_else) = do
  texp <- tPureExpWrap pexp
  tthen <- tBlock [stm_then] False
  telse <- tBlock [stm_else] False
  return [HS.Qualifier $ (HS.App
                                (HS.App
                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM") texp)
                               tthen)
                          telse)]

tStmt (ABS.SAssert pexp) = do
  texp <- tPureExpWrap pexp
  return [HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "assert") 
                          texp)]

tStmt (ABS.SWhile pexp stm) = do
  fscope <- funScope
  let vars = nub $ collectAssigns stm fscope
  let patVars = map (\ v -> HS.PVar $ HS.Ident v) vars
  let initVars = map (\ v -> if ABS.Ident v `M.member` fscope
                            then HS.Var $ HS.UnQual $ HS.Ident v -- it's already in scope
                            else (HS.Var $ identI "undefined") -- initialize to undefined
                     ) vars
  let expVars = map (\ v -> HS.Var $ HS.UnQual $ HS.Ident v) vars
  texp <- tPureExpWrap pexp
  HS.Do ts <- tBlock (case stm of
                     ABS.SBlock stmts ->  stmts 
                     stmt -> [stmt]) False
  return [HS.Generator HS.noLoc (HS.PTuple HS.Boxed patVars) -- lhs
          (HS.App (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "while")
                                 (HS.Tuple HS.Boxed initVars)) -- initial environment, captured by the current environment
                   (HS.Lambda HS.noLoc [HS.PTuple HS.Boxed patVars] texp)) -- the predicate
           (HS.Lambda HS.noLoc [HS.PTuple HS.Boxed patVars] -- the loop block
                  (HS.Do $ ts ++ [HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Tuple HS.Boxed expVars))])))]
                       
tStmt (ABS.SDec typ ident@(ABS.Ident var)) = 
    if isInterface typ
    then do -- just rewrites it to Interface x = null; 
      
      tStmt (ABS.SDecAss typ ident (ABS.ExpP $ ABS.ELit $ ABS.LNull)) -- adds also to current scope
    else error (var ++ "is ADT and has to be initialized")
    -- TODO: remove the ident from the class attributes to check

tStmt (ABS.SDecAss typ ident texp) = do
  addToScope ident typ          -- adds to scope
  tass <-  tStmt (ABS.SAss ident texp)
  return (tass)

tStmt (ABS.SAss ident@(ABS.Ident var) (ABS.ExpP pexp)) = do
  fscope <- funScope
  (cscope,_,_) <- ask
  case M.lookup ident fscope of
      Just t -> do
        texp <- tPureExpWrap pexp
        return [HS.Generator HS.noLoc 
                       -- lhs
                       (case t of
                          ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                          ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (tType ptyp))
                       -- rhs
                       texp]
      Nothing -> 
        case M.lookup ident cscope of -- maybe it is in the class scope
                                                -- normalize it to a field ass
          Just _t -> tStmt (ABS.SFieldAss ident (ABS.ExpP pexp))
          Nothing -> error (var ++ " not in scope")
                                                                             

tStmt (ABS.SAss ident@(ABS.Ident var) (ABS.ExpE eexp)) = do
  fscope <- funScope
  (cscope,_,_) <- ask
  case M.lookup ident fscope of
    Just t -> do
      texp <- liftInterf ident eexp `ap` tEffExpWrap eexp
      return [HS.Generator HS.noLoc
                               -- lhs
                               (case t of
                                  ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                  ptyp -> HS.PatTypeSig HS.noLoc (HS.PVar $ HS.Ident var)  (tType ptyp))
                               -- rhs
                               texp]
    Nothing -> 
          case M.lookup ident cscope of -- maybe it is in the class scope
            -- normalize it to a field ass
            Just _t -> tStmt (ABS.SFieldAss ident (ABS.ExpE eexp))
            Nothing -> error (var ++ " not in scope")

                        

tStmt (ABS.SFieldAss (ABS.Ident ident) (ABS.ExpP pexp)) = do
  (_, _, cls) <- ask
  texp <- tPureExpWrap pexp
  return [HS.Qualifier (HS.Paren $ HS.InfixApp 
                          (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ ident)
                          (HS.QVarOp $ symbolI "=<<")
                          (HS.Paren texp))] -- paren are necessary here


tStmt (ABS.SFieldAss ident@(ABS.Ident var) (ABS.ExpE eexp)) = do
  (_,_,cls) <- ask
  texp <- liftInterf ident eexp `ap` tEffExpWrap eexp
  return [HS.Qualifier (HS.Paren $ HS.InfixApp 
                                (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ var)
                                (HS.QVarOp $ symbolI "=<<")
                          texp)]


tStmt (ABS.STryCatchFinally try_stm cbranches mfinally) = do
  tfin <- case mfinally of
           ABS.NoFinally -> return id
           ABS.JustFinally fstm -> do
                         tblock <- tBlock [fstm] False 
                         return $ \ try_catch -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "finally")  try_catch) tblock
                                   
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
                                  ABS.PUnderscore -> (HS.App (HS.App (HS.Var $ identI "liftM") (HS.Con $ HS.UnQual $ HS.Ident "Just")) (HS.Paren $ tcstm))
                                  _ -> HS.Case (HS.Var $ HS.UnQual $ HS.Ident "__0")
                                      [HS.Alt HS.noLoc (tFunPat pat)
                                       -- wrap the normal returned expression in a just
                                       (HS.UnGuardedAlt (HS.App (HS.App (HS.Var $ identI "liftM") (HS.Con $ HS.UnQual $ HS.Ident "Just")) (HS.Paren $ tcstm))) (HS.BDecls []),
                                       -- pattern match fail, return nothing
                                       HS.Alt HS.noLoc HS.PWildCard (HS.UnGuardedAlt $ (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Con $ HS.UnQual $ HS.Ident "Nothing"))) (HS.BDecls [])])))
              cbranches

  return [HS.Qualifier
             -- (optionally) wrap the try-catch in a finally block
             (tfin
              (HS.App
                     (HS.App (HS.Var (HS.UnQual $ HS.Ident "catches"))
                        ttry)
                     (HS.List tbranches)))]



liftInterf ident exp@(ABS.New _ _) = liftInterf' ident exp
liftInterf ident exp@(ABS.NewLocal _ _) = liftInterf' ident exp
liftInterf ident exp = return id
liftInterf' ident@(ABS.Ident var) exp =  do
  fscope <- funScope
  (cscope, _, _)<- ask
  return $ case M.lookup ident (M.union fscope cscope) of
      Nothing -> error $ "Identifier " ++ var ++ " cannot be resolved from scope"
      Just (ABS.TUnderscore) -> error $ "Cannot infer interface type for variable" ++ var
      Just (ABS.TSimple (ABS.QType qids)) -> HS.App (HS.App (HS.Var $ identI "liftM") (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QTypeSegment (ABS.TypeIdent iid)) -> iid) (last qids)))
      Just _ -> error $ var ++ " not of interface type"


-- this scope is the oo-scope: it does not allow re-declaration
-- pure-scope is done with lambdas, so it allows re-declaration
addToScope :: ABS.Ident -> ABS.Type -> StmtM ()
addToScope var@(ABS.Ident pid) typ = do
  (topscope:restscopes) <- lift get
  if (any (\ scope -> var `M.member` scope) restscopes)
    then error $ pid ++ " already defined in an outer scope"
    else lift $ put $ M.insertWith (const $ const $ error $ pid ++ " already defined in this scope") var typ topscope  : restscopes


funScope :: StmtM ScopeTable
funScope = do
  scopes <- lift get
  return $ M.unions scopes

eReturnUnit :: HS.Exp
eReturnUnit = (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                     (HS.Con $ HS.Special $ HS.UnitCon)) -- return ()



-- tPureExpWrap is a pure expression in the statement world
-- it does 3 things:
-- 1) if a sub-expression is pure it wraps it in a return
-- 2) if a sub-expression reads local-variables it wraps them in readIORef
-- 3) if a sub-expression reads this fields it wraps them in readThis
tPureExpWrap :: (?moduleTable::ModuleTable) => ABS.PureExp -> StmtM HS.Exp
tPureExpWrap pexp = do
  (_,_,cls) <- ask
  runExpr $ do
      vcscope <- visible_cscope
      let thisFields = collectThisVars pexp vcscope
      texp <- tPureExp pexp []
      -- translate the pure expression with no typevars since stateful code cannot contain type variables (par polymorphism)
      -- TODO: no type variables, has to be changed for polymorphic methods
      return $ if null thisFields
               then HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") texp   --  rhs  
               else HS.Paren $ HS.InfixApp 
                      (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                      (HS.QVarOp $ symbolI ">>=")
                      (HS.Lambda HS.noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                            map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                         (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))
                                          ] (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") texp))

-- | tEffExpWrap is a wrapper arround tEffExp that adds a single read to the object pointer to collect the necessary fields
-- it is supposed to be an optimization compared to reading each time the field at the place it is accessed
tEffExpWrap :: (?moduleTable::ModuleTable) => ABS.EffExp -> StmtM HS.Exp
tEffExpWrap eexp = do
  (_,_,cls) <- ask
  runExpr $ do
      vcscope <- visible_cscope
      let argsExps = case eexp of
                         ABS.Get pexp -> [pexp]
                         ABS.New _ pexps  -> pexps
                         ABS.NewLocal _ pexps -> pexps
                         ABS.SyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                         ABS.ThisSyncMethCall _ pexps -> pexps
                         ABS.AsyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                         ABS.ThisAsyncMethCall _ pexps -> pexps
      let thisFields = concatMap ((flip collectThisVars) vcscope) argsExps
      texp <- tEffExp eexp
      return $ if null thisFields
               then texp
               else -- readObject this >>= \ Class1 { record bindings   } ->
                   HS.Paren $ HS.InfixApp 
                   (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                   (HS.QVarOp $ symbolI ">>=")
                   (HS.Lambda HS.noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                         map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                      (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))]
                    texp)

runExpr :: ExprM a -> StmtM a
runExpr e = do
  fscope <- funScope
  (cscope,interf,_) <- ask
  return $ runReader e (fscope,cscope,interf)

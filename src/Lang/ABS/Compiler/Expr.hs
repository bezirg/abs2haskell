module Lang.ABS.Compiler.Expr
    (tType
    ,tTypeOrTyVar
    ,tPattern
    ,tPureExp
    ,tBody
    ,joinSub
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import Control.Monad.Trans.Reader (ask, local, runReader)

import qualified Data.Map as M
import Data.Foldable (foldlM)

tBody :: (?moduleTable::ModuleTable) => ABS.FunBody -> [ABS.TypeIdent] -> [ABS.Param] -> HS.Exp
tBody ABS.BuiltinFunBody _tyvars _params = HS.Var $ identI "undefined" -- builtin turned to undefined
tBody (ABS.NormalFunBody pexp) tyvars params = runReader (tPureExp pexp tyvars) 
                (M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params -- initial function scope is the formal params
                ,error "no class context") -- no class scope
        

tPureExp :: (?moduleTable::ModuleTable) =>
            ABS.PureExp 
          -> [ABS.TypeIdent] -- ^ TypeVarsInScope
          -> ExprM HS.Exp

tPureExp (ABS.If predE thenE elseE) tyvars = do
  tpred <- tPureExp predE tyvars
  tthen <- tPureExp thenE tyvars
  telse <- tPureExp elseE tyvars
  return $ HS.If tpred tthen telse

-- | translate it into a lambda exp
tPureExp (ABS.Let (ABS.Par ptyp pid@(ABS.Ident var)) eqE inE) tyvars = do
  tin <- local (\ (fscope, interf) -> (M.insert pid ptyp fscope, -- add to function scope of the "in"-expression
                                            interf)) $ 
        tPureExp inE tyvars
  teq <- tPureExp eqE tyvars
  let pat = HS.PVar $ HS.Ident var
  return $ HS.App -- apply the created lamdba to the equality expr
             (HS.Lambda HS.noLoc
                [if ptyp == ABS.TUnderscore
                 then pat -- infer the parameter type
                 else HS.PatTypeSig HS.noLoc pat (tTypeOrTyVar tyvars ptyp)] -- wrap with an explicit type annotation -- bound variable
                tin) teq

tPureExp (ABS.Case matchE branches) tyvars = do
  tmatch <- tPureExp matchE tyvars
  (scope,_) <- ask
  case matchE of
    ABS.EVar ident | M.lookup ident scope == Just (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Exception")])) -> tCaseException tmatch branches
    ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen tid]) | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen tid]) _ | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    _ -> do
        talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
                        let new_vars = collectPatVars pat
                        texp <- local (\ (fscope, interf) -> 
                                          (M.fromList (zip new_vars (repeat ABS.TUnderscore)) `M.union` fscope, interf)) -- TODO: tunderscore acts as a placeholder since the types of the new vars in the matchE are not taken into account
                                                                              (tPureExp pexp tyvars)
                        return $ HS.Alt HS.noLoc (tPattern pat) (HS.UnGuardedAlt texp) (HS.BDecls []))
                branches
        return $ HS.Case tmatch talts

  where
    tCaseException tmatch brs = do
      talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
             let new_vars = collectPatVars pat
             texp <- local (\ (fscope, interf) -> 
                                          (M.fromList (zip new_vars (repeat ABS.TUnderscore)) `M.union` fscope, interf)) (tPureExp pexp tyvars)
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


tPureExp (ABS.EFunCall (ABS.Ident cid) args) tyvars = foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp nextArg tyvars
                                               return $ HS.App acc targ)
                                            (HS.Var $ HS.UnQual $ HS.Ident cid)
                                            args
tPureExp (ABS.EQualFunCall (ABS.TTyp tsegs) (ABS.Ident cid) args) tyvars = foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp nextArg tyvars
                                               return $ HS.App acc targ)
                                            (HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident cid)
                                            args

tPureExp (ABS.ENaryFunCall (ABS.Ident cid) args) tyvars = do
  targs <- mapM (\ arg -> tPureExp arg tyvars) args
  return $ HS.App 
             (HS.Var $ HS.UnQual $ HS.Ident cid)
             (HS.List targs)

tPureExp (ABS.ENaryQualFunCall (ABS.TTyp tsegs) (ABS.Ident cid) args) tyvars = do
  targs <- mapM (\ arg -> tPureExp arg tyvars) args
  return $ HS.App 
             (HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident cid)
             (HS.List targs)

-- constants
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LNull)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp (ABS.EEq (ABS.ELit ABS.LThis) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "False"

tPureExp (ABS.EEq pnull@(ABS.ELit ABS.LNull) pvar@(ABS.EVar ident@(ABS.Ident str))) _tyvars = do
  tnull <- tPureExp pnull _tyvars
  tvar <- tPureExp pvar _tyvars
  (fscope, _) <- ask
  case M.lookup ident fscope of -- check the type of the right var
    Just t -> if isInterface t
             then return $ HS.Paren $ HS.InfixApp
                      (HS.ExpTypeSig HS.noLoc tnull (tType t))
                       (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                       (HS.ExpTypeSig HS.noLoc tvar (tType t))
             else error "cannot equate datatype to null"
    Nothing -> error $ str ++ " not in scope"

-- commutative
tPureExp (ABS.EEq pexp pnull@(ABS.ELit (ABS.LNull))) _tyvars = tPureExp (ABS.EEq pnull pexp) _tyvars

tPureExp (ABS.EEq pvar1@(ABS.EVar ident1@(ABS.Ident str1)) pvar2@(ABS.EVar ident2@(ABS.Ident str2))) _tyvars = do
  tvar1 <- tPureExp pvar1 _tyvars
  tvar2 <- tPureExp pvar2 _tyvars
  (fscope, _) <- ask
  case M.lookup ident1 fscope of -- check the type of the right var
    Just t1 -> case M.lookup ident2 fscope of
                Just t2 -> if isInterface t1 && isInterface t2
                          then case joinSub t1 t2 of
                              Just t ->
                                return $ HS.Paren $ HS.InfixApp
                                  (HS.ExpTypeSig HS.noLoc tvar1 (tType t))
                                  (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                                  (HS.ExpTypeSig HS.noLoc tvar2 (tType t))
                              Nothing -> error "cannot unify the two interfaces"
                          else return $ HS.Paren $ HS.InfixApp  -- treat them as both datatypes and let haskell figure out if there is a type mismatch
                                          tvar1
                                          (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                                          tvar2
                Nothing -> error $ str2 ++ " not in scope"
    Nothing -> error $ str1 ++ " not in scope"

-- tPureExp (ABS.EEq pfun1@(ABS.EFunCall _ _) pfun2@(ABS.EFunCall _ _)) _tyvars = error "equality on function expressions not implemented yet"
-- tPureExp (ABS.EEq pfun1@(ABS.ENaryFunCall _ _) pfun2@(ABS.ENaryFunCall _ _)) _tyvars = error "equality on function expressions not implemented yet"
-- tPureExp (ABS.EEq pfun@(ABS.EFunCall _ _) _)  _tyvars = error "equality on function expressions not implemented yet"
-- tPureExp (ABS.EEq pfun@(ABS.ENaryFunCall _ _) _) _tyvars = error "equality on function expressions not implemented yet"
-- tPureExp (ABS.EEq pexp1 pfun@(ABS.EFunCall _ _)) _tyvars = tPureExp (ABS.EEq pfun pexp1) _tyvars -- commutative
-- tPureExp (ABS.EEq pexp1 pfun@(ABS.ENaryFunCall _ _)) _tyvars = tPureExp (ABS.EEq pfun pexp1) _tyvars -- commutative

-- a catch-all for literals,constructors maybe coupled with vars
tPureExp (ABS.EEq pexp1 pexp2) _tyvars = do
  texp1 <- tPureExp pexp1 _tyvars
  texp2 <- tPureExp pexp2 _tyvars
  return $ HS.Paren $ HS.InfixApp  
         texp1
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
         texp2


-- normalizess to not . ==
tPureExp (ABS.ENeq left right) tyvars = tPureExp (ABS.ELogNeg $ ABS.EEq left right) tyvars

-- be careful to parenthesize infix apps
tPureExp (ABS.EOr left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual $ HS.Symbol "||")
         tright

tPureExp (ABS.EAnd left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")
         tright

tPureExp (ABS.ELt left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<")  
         tright

tPureExp (ABS.ELe left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<=")  
         tright


tPureExp (ABS.EGt left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">")  
         tright

tPureExp (ABS.EGe left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">=")  
         tright

tPureExp (ABS.EAdd left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "+")  
         tright

tPureExp (ABS.ESub left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "-")  
         tright

tPureExp (ABS.EMul left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "*")  
         tright

tPureExp (ABS.EDiv left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/")  
         tright

tPureExp (ABS.EMod left right) tyvars = do
  tleft <- tPureExp left tyvars
  tright <- tPureExp right tyvars
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol "%")  
         tright

tPureExp (ABS.ELogNeg pexp) tyvars = do
  texp <- tPureExp pexp tyvars
  return $ HS.Paren $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") texp

tPureExp (ABS.EIntNeg pexp) tyvars = do
  texp <- tPureExp pexp tyvars
  return $ HS.Paren $ HS.NegApp texp

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Unit")])) _ = 
    return $ HS.Con $ HS.Special HS.UnitCon -- for the translation to ()

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Nil")])) _ = 
    return $ HS.Con $ HS.Special HS.ListCon -- for the translation to []

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "EmptyMap")])) _ = 
    return $ HS.Var $ HS.UnQual $ HS.Ident "empty" -- for the translation to Data.Map

tPureExp (ABS.ESinglConstr (ABS.QTyp qids)) _ = return $
  let mids = init qids
      tid@(ABS.TypeIdent sid) = (\ (ABS.QTypeSegmen cid) -> cid) (last qids)
  in if tid `elem` concatMap exceptions ?moduleTable
     -- if is an exception constructor, replace it with its smart constructor
     then HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ headToLower sid  
     else
         HS.Con $ (if null mids 
                   then HS.UnQual 
                   else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                  ) $ HS.Ident sid

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Triple")]) pexps) tyvars =   
    if length pexps == 3
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else error "wrong number of arguments to Triple"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Pair")]) pexps) tyvars = 
    if length pexps == 2
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else error "wrong number of arguments to Pair"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Cons")]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.Paren (HS.InfixApp 
                       texp1
                       (HS.QConOp $ HS.Special $ HS.Cons)
                       texp2)
tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "Cons")]) _) _ = error "wrong number of arguments to Cons"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "InsertAssoc")]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") texp1) texp2
tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.TypeIdent "InsertAssoc")]) _) _ = error "wrong number of arguments to InsertAssoc"

tPureExp (ABS.EParamConstr qids args) tyvars = do
    tcon <- tPureExp (ABS.ESinglConstr qids) tyvars -- first translate the constructor
    foldlM (\ acc nextArg -> do
              targ <- tPureExp nextArg tyvars 
              return $ HS.App acc targ) tcon args

tPureExp (ABS.EVar var@(ABS.Ident pid)) _tyvars = do
    (fscope, _) <- ask
    return $ case M.lookup var fscope of
      Nothing ->  error $ pid ++ " not in scope"
        -- if it of an int type, upcast it
      Just (ABS.TSimple (ABS.QTyp ([ABS.QTypeSegmen (ABS.TypeIdent "Int")]))) -> HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident pid)
      Just t -> HS.Paren $ (if isInterface t
                           then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                           else id) 
               (HS.Var $ HS.UnQual $ HS.Ident pid)

tPureExp (ABS.EQualVar (ABS.TTyp tsegs) (ABS.Ident pid)) _tyvars = -- todo: we cannot use any scope, so we have to lookup the other modules, to check if it is of an interface type (upcast it) or int (fromIntegral) 
    return $ HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident pid
    -- we tread it as pure for now

tPureExp (ABS.ELit lit) _ = return $ case lit of
                                    ABS.LStr str ->  HS.Lit $ HS.String str
                                    ABS.LInt i ->  HS.Lit $ HS.Int i
                                    ABS.LThis -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                    ABS.LNull -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "null")


tPureExp (ABS.EThis (ABS.Ident _)) _ = return $ error "Cannot compile object accesses in mathematically pure expressions"

tPattern :: ABS.Pattern -> HS.Pat
tPattern (ABS.PIdent (ABS.Ident pid)) = HS.PVar $ HS.Ident $ pid
tPattern (ABS.PSinglConstr (ABS.TypeIdent "Nil")) = HS.PList []
tPattern (ABS.PSinglConstr (ABS.TypeIdent "Unit")) = HS.PTuple HS.Boxed [] -- () unit constructor
tPattern (ABS.PSinglConstr (ABS.TypeIdent tid)) = HS.PApp (HS.UnQual $ HS.Ident tid) []
tPattern (ABS.PParamConstr (ABS.TypeIdent "Triple") subpats) | length subpats == 3 = HS.PTuple HS.Boxed (map tPattern subpats)
                                                               | otherwise = error "wrong number of arguments to Triple"
tPattern (ABS.PParamConstr (ABS.TypeIdent "Pair") subpats) | length subpats == 2 = HS.PTuple HS.Boxed (map tPattern subpats)
                                                             | otherwise = error "wrong number of arguments to Pair"
tPattern (ABS.PParamConstr (ABS.TypeIdent "Cons") [subpat1, subpat2]) = HS.PParen (HS.PInfixApp 
                                                                          (tPattern subpat1)
                                                                          (HS.Special $ HS.Cons)
                                                                          (tPattern subpat2))
tPattern (ABS.PParamConstr (ABS.TypeIdent "Cons") _) = error "wrong number of arguments to Cons"
tPattern (ABS.PParamConstr (ABS.TypeIdent "InsertAssoc") _) = error "InsertAssoc is unsafe, you should avoid it."
tPattern (ABS.PParamConstr (ABS.TypeIdent tid) subpats) = HS.PApp (HS.UnQual $ HS.Ident tid) (map tPattern subpats)
tPattern ABS.PUnderscore = HS.PWildCard
tPattern (ABS.PLit lit) = HS.PLit $ case lit of
                                         (ABS.LStr str) ->  HS.String str
                                         (ABS.LInt i) ->  HS.Int i
                                         _ -> error "this or null are not allowed in pattern syntax"


-- | translate an ABS Type or a TypeVar to HS type
tTypeOrTyVar :: [ABS.TypeIdent] -> ABS.Type -> HS.Type
tTypeOrTyVar tyvars (ABS.TSimple (ABS.QTyp qtids))  = 
       let joinedTid = joinQualTypeIds qtids    
       in if (ABS.TypeIdent joinedTid) `elem` tyvars -- type variable
          then HS.TyVar $ HS.Ident $ headToLower joinedTid
          else HS.TyCon $ if length qtids == 1 
                          then HS.UnQual $ HS.Ident joinedTid -- unqual
                          else HS.Qual (HS.ModuleName (joinQualTypeIds (init qtids))) -- qual
                                   (HS.Ident $ (\ (ABS.QTypeSegmen (ABS.TypeIdent tid)) -> tid) (last qtids))

tTypeOrTyVar tyvars (ABS.TGen qtyp tyargs) = foldl (\ tyacc tynext -> HS.TyApp tyacc (tTypeOrTyVar tyvars tynext)) (tType (ABS.TSimple qtyp)) tyargs

-- | shorthand for only translate ABS Types (no typevars) to HS types 
tType :: ABS.Type -> HS.Type
tType t = tTypeOrTyVar [] t     -- no type variables in scope

-- unify two interfaces, to their *Common interface*
-- will return Nothing if the interfaces are not unifiable
joinSub :: (?moduleTable::ModuleTable) => ABS.Type -> ABS.Type -> (Maybe ABS.Type)
joinSub t1 t2 | t1 == t2 = Just t1 -- same interface subtyping
joinSub t1@(ABS.TSimple interf1) t2@(ABS.TSimple interf2) | otherwise = 
  let 
    unionedST = (M.unions $ map hierarchy ?moduleTable) :: M.Map ABS.TypeIdent [ABS.QType]
    canReach :: ABS.QType -> ABS.QType -> Bool
    canReach (ABS.QTyp qids) principal = let ABS.QTypeSegmen sub = last qids in
                                             case M.lookup sub unionedST of
                                               Just sups -> if principal `elem` sups
                                                           then True
                                                           else any (\ sup -> canReach sup principal) sups
                                               Nothing -> False
                                             in
                                               if interf1 `canReach` interf2
                                               then Just t2
                                               else if interf2 `canReach` interf1
                                                    then Just t1
                                                    else Nothing
joinSub _ _ = error "joinSub"




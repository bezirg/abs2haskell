-- | Functions to translate pure-expressions (inside def functions)
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

-- | Translating the body of a pure function
tBody :: (?moduleTable::ModuleTable) => ABS.FunBody -> [ABS.UIdent] -> [ABS.Param] -> HS.Exp
tBody ABS.BuiltinFunBody _tyvars _params = HS.Var $ identI "undefined" -- builtin turned to undefined
tBody (ABS.NormalFunBody pexp) tyvars params = runReader (tPureExp pexp tyvars) 
                (M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params) -- initial function scope is the formal params
        

-- | Translating a pure expression
tPureExp :: (?moduleTable::ModuleTable) =>
            ABS.PureExp 
          -> [ABS.UIdent] -- ^ TypeVarsInScope
          -> ExprM HS.Exp

tPureExp (ABS.If predE thenE elseE) tyvars = do
  tpred <- tPureExp predE tyvars
  tthen <- tPureExp thenE tyvars
  telse <- tPureExp elseE tyvars
  return $ HS.If tpred tthen telse

-- translate it into a lambda exp
tPureExp (ABS.Let (ABS.Par ptyp pid@(ABS.LIdent (_,var))) eqE inE) tyvars = do
  tin <- local (\ fscope -> M.insert pid ptyp fscope -- add to function scope of the "in"-expression
                                            ) $ 
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
  scope <- ask
  case matchE of
    ABS.EVar ident | M.lookup ident scope == Just (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (undefined, "Exception"))])) -> tCaseException tmatch branches
    ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen tid]) | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen tid]) _ | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    _ -> do
        talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
                        let new_vars = collectPatVars pat
                        texp <- local (\ fscope -> 
                                          (M.fromList (zip new_vars (repeat ABS.TUnderscore)) `M.union` fscope)) -- TODO: tunderscore acts as a placeholder since the types of the new vars in the matchE are not taken into account
                                                                              (tPureExp pexp tyvars)
                        return $ HS.Alt HS.noLoc (tPattern pat) (HS.UnGuardedAlt texp) (HS.BDecls []))
                branches
        return $ HS.Case tmatch talts

  where
    tCaseException tmatch brs = do
      talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
             let new_vars = collectPatVars pat
             texp <- local (\ fscope -> 
                                          (M.fromList (zip new_vars (repeat ABS.TUnderscore)) `M.union` fscope)) (tPureExp pexp tyvars)
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


tPureExp (ABS.EFunCall (ABS.LIdent (_,cid)) args) tyvars = foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp nextArg tyvars
                                               return $ HS.App acc targ)
                                            (HS.Var $ HS.UnQual $ HS.Ident cid)
                                            args
tPureExp (ABS.EQualFunCall (ABS.TTyp tsegs) (ABS.LIdent (_,cid)) args) tyvars = foldlM
                                            (\ acc nextArg -> do
                                               targ <- tPureExp nextArg tyvars
                                               return $ HS.App acc targ)
                                            (HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident cid)
                                            args

tPureExp (ABS.ENaryFunCall (ABS.LIdent (_,cid)) args) tyvars = do
  targs <- mapM (\ arg -> tPureExp arg tyvars) args
  return $ HS.App 
             (HS.Var $ HS.UnQual $ HS.Ident cid)
             (HS.List targs)

tPureExp (ABS.ENaryQualFunCall (ABS.TTyp tsegs) (ABS.LIdent (_,cid)) args) tyvars = do
  targs <- mapM (\ arg -> tPureExp arg tyvars) args
  return $ HS.App 
             (HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident cid)
             (HS.List targs)

-- constants
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LNull)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp (ABS.EEq (ABS.ELit ABS.LThis) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "True"
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LThis)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "False"

tPureExp (ABS.EEq pnull@(ABS.ELit ABS.LNull) pvar@(ABS.EVar ident@(ABS.LIdent (p,str)))) _tyvars = do
  tnull <- tPureExp pnull _tyvars
  tvar <- tPureExp pvar _tyvars
  fscope <- ask
  case M.lookup ident fscope of -- check the type of the right var
    Just t -> if isInterface t
             then return $ HS.Paren $ HS.InfixApp
                      (HS.ExpTypeSig HS.noLoc tnull (tType t))
                       (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                       (HS.ExpTypeSig HS.noLoc tvar (tType t))
             else errorPos p "cannot equate datatype to null"
    Nothing -> errorPos p $ str ++ " not in scope"

-- commutative
tPureExp (ABS.EEq pexp pnull@(ABS.ELit (ABS.LNull))) _tyvars = tPureExp (ABS.EEq pnull pexp) _tyvars

tPureExp (ABS.EEq pvar1@(ABS.EVar ident1@(ABS.LIdent (p1,str1))) pvar2@(ABS.EVar ident2@(ABS.LIdent (p2,str2)))) _tyvars = do
  tvar1 <- tPureExp pvar1 _tyvars
  tvar2 <- tPureExp pvar2 _tyvars
  fscope <- ask
  case M.lookup ident1 fscope of -- check the type of the right var
    Just t1 -> case M.lookup ident2 fscope of
                Just t2 -> if isInterface t1 && isInterface t2
                          then case joinSub t1 t2 of
                              Just t ->
                                return $ HS.Paren $ HS.InfixApp
                                  (HS.ExpTypeSig HS.noLoc tvar1 (tType t))
                                  (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                                  (HS.ExpTypeSig HS.noLoc tvar2 (tType t))
                              Nothing -> error ("cannot unify the interface " ++ str1 
                                               ++ " at position " ++ showPos p1 ++ " with interface " ++ str2 ++ " at position " ++ showPos p2)
                          else return $ HS.Paren $ HS.InfixApp  -- treat them as both datatypes and let haskell figure out if there is a type mismatch
                                          tvar1
                                          (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==")
                                          tvar2
                Nothing -> errorPos p2 $ str2 ++ " not in scope"
    Nothing -> errorPos p1 $ str1 ++ " not in scope"

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

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Unit"))])) _ = 
    return $ HS.Con $ HS.Special HS.UnitCon -- for the translation to ()

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Nil"))])) _ = 
    return $ HS.Con $ HS.Special HS.ListCon -- for the translation to []

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"EmptyMap"))])) _ = 
    return $ HS.Var $ HS.UnQual $ HS.Ident "_emptyMap" -- for the translation to Data.Map

tPureExp (ABS.ESinglConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"EmptySet"))])) _ = 
    return $ HS.Var $ HS.UnQual $ HS.Ident "_emptySet" -- for the translation to Data.Map

tPureExp (ABS.ESinglConstr (ABS.QTyp qids)) _ = return $
  let mids = init qids
      tid@(ABS.UIdent (_,sid)) = (\ (ABS.QTypeSegmen cid) -> cid) (last qids)
  in if tid `elem` concatMap exceptions ?moduleTable
     -- if is an exception constructor, replace it with its smart constructor
     then HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ headToLower sid  
     else
         HS.Con $ (if null mids 
                   then HS.UnQual 
                   else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                  ) $ HS.Ident sid

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Triple"))]) pexps) tyvars =   
    if length pexps == 3
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else errorPos p "wrong number of arguments to Triple"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Pair"))]) pexps) tyvars = 
    if length pexps == 2
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else errorPos p "wrong number of arguments to Pair"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Cons"))]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.Paren (HS.InfixApp 
                       texp1
                       (HS.QConOp $ HS.Special $ HS.Cons)
                       texp2)
tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"Cons"))]) _) _ = errorPos p "wrong number of arguments to Cons"

tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"InsertAssoc"))]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") texp1) texp2
tPureExp (ABS.EParamConstr (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (p,"InsertAssoc"))]) _) _ = errorPos p "wrong number of arguments to InsertAssoc"

tPureExp (ABS.EParamConstr qids args) tyvars = do
    tcon <- tPureExp (ABS.ESinglConstr qids) tyvars -- first translate the constructor
    foldlM (\ acc nextArg -> do
              targ <- tPureExp nextArg tyvars 
              return $ HS.App acc targ) tcon args

tPureExp (ABS.EVar var@(ABS.LIdent (p,pid))) _tyvars = do
    fscope <- ask
    return $ case M.lookup var fscope of
      Nothing ->  errorPos p $ pid ++ " not in scope"
        -- if it of an int type, upcast it
      Just (ABS.TSimple (ABS.QTyp ([ABS.QTypeSegmen (ABS.UIdent (_,"Int"))]))) -> HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident pid)
      Just t -> HS.Paren $ (if isInterface t
                           then HS.App (HS.Var $ identI "up") -- upcasting if it is of a class type
                           else id) 
               (HS.Var $ HS.UnQual $ HS.Ident pid)

tPureExp (ABS.EQualVar (ABS.TTyp tsegs) (ABS.LIdent (_,pid))) _tyvars = -- todo: we cannot use any scope, so we have to lookup the other modules, to check if it is of an interface type (upcast it) or int (fromIntegral) 
    return $ HS.Var $ HS.Qual (HS.ModuleName $ joinTTypeIds tsegs) $ HS.Ident pid
    -- we tread it as pure for now

tPureExp (ABS.ELit lit) _ = return $ case lit of
                                    ABS.LStr str ->  HS.Lit $ HS.String str
                                    ABS.LInt i ->  HS.Lit $ HS.Int i
                                    ABS.LThis -> HS.App (HS.Var $ identI "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                    ABS.LNull -> HS.App (HS.Var $ identI "up") (HS.Var $ HS.UnQual $ HS.Ident "null")
                                    ABS.LThisDC -> HS.Var $ HS.UnQual $ HS.Ident "thisDC"

tPureExp (ABS.EThis (ABS.LIdent (p, _))) _ = return $ errorPos p "Cannot compile object accesses in mathematically pure expressions"

-- | Translating a pattern of an ABS case-branch in pure case-pattern-matching
tPattern :: ABS.Pattern -> HS.Pat
tPattern (ABS.PIdent (ABS.LIdent (_,pid))) = HS.PVar $ HS.Ident $ pid
tPattern (ABS.PSinglConstr (ABS.UIdent (_,"Nil"))) = HS.PList []
tPattern (ABS.PSinglConstr (ABS.UIdent (_,"Unit"))) = HS.PTuple HS.Boxed [] -- () unit constructor
tPattern (ABS.PSinglConstr (ABS.UIdent (_, tid))) = HS.PApp (HS.UnQual $ HS.Ident tid) []
tPattern (ABS.PParamConstr (ABS.UIdent (p,"Triple")) subpats) | length subpats == 3 = HS.PTuple HS.Boxed (map tPattern subpats)
                                                               | otherwise = errorPos p "wrong number of arguments to Triple"
tPattern (ABS.PParamConstr (ABS.UIdent (p,"Pair")) subpats) | length subpats == 2 = HS.PTuple HS.Boxed (map tPattern subpats)
                                                             | otherwise = errorPos p "wrong number of arguments to Pair"
tPattern (ABS.PParamConstr (ABS.UIdent (_,"Cons")) [subpat1, subpat2]) = HS.PParen (HS.PInfixApp 
                                                                          (tPattern subpat1)
                                                                          (HS.Special $ HS.Cons)
                                                                          (tPattern subpat2))
tPattern (ABS.PParamConstr (ABS.UIdent (p,"Cons")) _) = errorPos p "wrong number of arguments to Cons"
tPattern (ABS.PParamConstr (ABS.UIdent (p,"InsertAssoc")) _) = errorPos p "InsertAssoc is unsafe, you should avoid it."
tPattern (ABS.PParamConstr (ABS.UIdent (_,tid)) subpats) = HS.PApp (HS.UnQual $ HS.Ident tid) (map tPattern subpats)
tPattern ABS.PUnderscore = HS.PWildCard
tPattern (ABS.PLit lit) = HS.PLit $ case lit of
                                         (ABS.LStr str) ->  HS.String str
                                         (ABS.LInt i) ->  HS.Int i
                                         _ -> error "this or null are not allowed in pattern syntax"


-- | Translating an ABS type or an ABS type-variable to a Haskell type
tTypeOrTyVar :: [ABS.UIdent] -> ABS.Type -> HS.Type
tTypeOrTyVar tyvars (ABS.TSimple (ABS.QTyp qtids))  = 
       let joinedTid = joinQualTypeIds qtids    
       in if ABS.UIdent (undefined, joinedTid) `elem` tyvars -- type variable
          then HS.TyVar $ HS.Ident $ headToLower joinedTid
          else HS.TyCon $ if length qtids == 1 
                          then HS.UnQual $ HS.Ident joinedTid -- unqual
                          else HS.Qual (HS.ModuleName (joinQualTypeIds (init qtids))) -- qual
                                   (HS.Ident $ (\ (ABS.QTypeSegmen (ABS.UIdent (_,tid))) -> tid) (last qtids))
tTypeOrTyVar tyvars (ABS.TGen qtyp tyargs) = foldl (\ tyacc tynext -> HS.TyApp tyacc (tTypeOrTyVar tyvars tynext)) (tType (ABS.TSimple qtyp)) tyargs

-- | Shorthand for only translate ABS Types (no type-variables) to haskell types 
tType :: ABS.Type -> HS.Type
tType t = tTypeOrTyVar [] t     -- no type variables in scope

-- | unify two interfaces, to their _common super-interface_
--
-- Will return Nothing if the interfaces are not unifiable.
-- Used during object-equality
joinSub :: (?moduleTable::ModuleTable) => ABS.Type -> ABS.Type -> (Maybe ABS.Type)
joinSub t1 t2 | t1 == t2 = Just t1 -- same interface subtyping
joinSub t1@(ABS.TSimple interf1) t2@(ABS.TSimple interf2) | otherwise = 
  let 
    unionedST = (M.unions $ map hierarchy ?moduleTable) :: M.Map ABS.UIdent [ABS.QType]
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
joinSub _ _ = error "joinSub is not tuned to work yet for qualified interfaces"




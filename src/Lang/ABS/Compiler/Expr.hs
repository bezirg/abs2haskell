module Lang.ABS.Compiler.Expr
    (tType
    ,tTypeOrTyVar
    ,tFunPat
    ,tPureExp
    ,tEffExp
    ,tAwaitGuard
    ,tBody
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import Control.Monad.Trans.Reader (ask, local, runReader)

import Data.List (findIndices)
import qualified Data.Map as M
import Data.Foldable (foldlM)

tBody :: (?moduleTable::ModuleTable) => ABS.FunBody -> [ABS.TypeIdent] -> [ABS.Param] -> HS.Exp
tBody ABS.BuiltinFunBody _tyvars _params = HS.Var $ identI "undefined" -- builtin turned to undefined
tBody (ABS.NormalFunBody pexp) tyvars params = runReader (tPureExp pexp tyvars) 
                (M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params -- initial function scope is the formal params
                ,M.empty
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
  tin <- local (\ (fscope, cscope,interf) -> (M.insert pid ptyp fscope, -- add to function scope of the "in"-expression
                                            cscope,interf)) $ 
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
  (fscope,cscope,_) <- ask
  let scope = fscope `M.union` cscope
  case matchE of
    ABS.EVar ident | M.lookup ident scope == Just (ABS.TSimple (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Exception")])) -> tCaseException tmatch branches
    ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment tid]) | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    ABS.EParamConstr (ABS.QType [ABS.QTypeSegment tid]) _ | tid `elem` concatMap exceptions ?moduleTable -> tCaseException tmatch branches
    _ -> do
        talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
                        let new_vars = collectPatVars pat
                        texp <- local (\ (fscope, cscope, interf) -> 
                                          (M.fromList (zip new_vars (repeat ABS.TUnderscore)) `M.union` fscope, cscope, interf)) -- TODO: tunderscore acts as a placeholder since the types of the new vars in the matchE are not taken into account
                                                                              (tPureExp pexp tyvars)
                        return $ HS.Alt HS.noLoc (tFunPat pat) (HS.UnGuardedAlt texp) (HS.BDecls []))
                branches
        return $ HS.Case tmatch talts

  where
    tCaseException tmatch brs = do
      talts <- mapM (\ (ABS.CaseBranc pat pexp) -> do
             texp <- tPureExp pexp tyvars
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
                             HS.Alt HS.noLoc (tFunPat pat) (HS.UnGuardedAlt (HS.App (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.Paren texp))) (HS.BDecls [])
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

tPureExp (ABS.ENaryFunCall (ABS.Ident cid) args) tyvars = do
  targs <- mapM (\ arg -> tPureExp arg tyvars) args
  return $ HS.App 
             (HS.Var $ HS.UnQual $ HS.Ident cid)
             (HS.List targs)

-- Equality handler
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LNull)) _tyvars = return $ HS.Con $ HS.UnQual $ HS.Ident "True"

-- normalize
tPureExp (ABS.EEq (ABS.ELit ABS.LNull) right) tyvars = tPureExp (ABS.EEq right (ABS.ELit ABS.LNull)) tyvars

-- right is null, so check left if null
tPureExp (ABS.EEq left (ABS.ELit ABS.LNull)) tyvars = check =<< tPureExp left tyvars
    where
                check pexp = case pexp of
                  HS.Paren pexp' -> check pexp'
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) o -> check o -- unwrap the "up"
                  HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
                  -- it is this object
                  hvar@(HS.Var (HS.UnQual (HS.Ident "this"))) -> do
                      i <- interf
                      return $HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ i) 
                                        hvar)
                                 (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null")

                  -- it is unqualified non-this object
                  hvar@(HS.Var (HS.UnQual (HS.Ident v))) -> do
                      (fscope, cscope,_) <- ask
                      let vtyp@(ABS.TSimple (ABS.QType qtids)) = maybe (error "incomparable types") id $ 
                                                                           M.lookup (ABS.Ident v) $ 
                                                                            M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope)
                      return $ if isInterface vtyp
                               then HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) 
                                        hvar)
                                  (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null")
                               else error "incomparable types"

                  -- it is qualified non-this object
                  hvar@(HS.Var (HS.Qual _m (HS.Ident v))) -> do
                      (fscope, cscope,_) <- ask
                      let vtyp@(ABS.TSimple (ABS.QType qtids)) = maybe (error "incomparable types") id $  
                                                                           M.lookup (ABS.Ident v) $ 
                                                                            M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope)
                      return $ if isInterface vtyp
                               then HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) 
                                        hvar)
                                  (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null")
                               else error "incomparable types"
                  HS.Con _ -> error "cannot compare ADT constructor with null"
                  HS.InfixApp _ _ _ -> error "cannot compare pure value with null"
                  HS.NegApp _ -> error "cannot compare pure value with null"
                  HS.Lit _ -> error "cannot compare pure value with null"
                  HS.Tuple _ _ -> error "cannot compare tuple with null"

tPureExp (ABS.EEq (ABS.ELit (ABS.LThis)) (ABS.ELit (ABS.LThis))) _ = do
  i <- interf
  if length i > 0 -- hack to ensure it is not in a main block
    then return $ HS.Con $ HS.UnQual $ HS.Ident "True"
    else error "this not allowed in main block"

-- normalize, associativity
tPureExp (ABS.EEq left@(ABS.ELit (ABS.LThis)) right) tyvars = tPureExp (ABS.EEq right left) tyvars
    
tPureExp (ABS.EEq left (ABS.ELit (ABS.LThis))) tyvars = check =<< tPureExp left tyvars
              where
                check pexp = case pexp of
                  HS.Paren pexp' -> check pexp'
                  -- it is a qual non-this object
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.Qual _m (HS.Ident v))) ->  do
                       (fscope,cscope,i) <- ask                
                       let vtyp@(ABS.TSimple qtyp) = maybe (error "incomparable types") id $  
                                                               M.lookup (ABS.Ident v) $ 
                                                                M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope)
                       return $ if isInterface vtyp
                                then case joinSub qtyp (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent i)]) of
                                 Just (ABS.QType qtids) -> 
                                     HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) hvar) $ 
                                               HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "this"
                                 Nothing -> error "incomparable types"
                                else error "incomparable types"
                  -- it is an unqual non-this object
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.UnQual (HS.Ident v))) -> do
                      (fscope,cscope,i) <- ask                 
                      let vtyp@(ABS.TSimple qtyp) = maybe (error "incomparable types") id $ 
                                                               M.lookup (ABS.Ident v) $ 
                                                                M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope)
                      return $ if isInterface vtyp
                               then case joinSub qtyp (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent i)]) of
                                Just (ABS.QType qtids) -> HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) hvar) $ 
                                                         HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "this"
                                Nothing -> error "incomparable types"
                               else (error "incomparable types")
                  HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
                  HS.Con _ -> error "cannot compare ADT constructor with null"
                  HS.InfixApp _ _ _ -> error "cannot compare pure value with null"
                  HS.NegApp _ -> error "cannot compare pure value with null"
                  HS.Lit _ -> error "cannot compare pure value with null"
                  HS.Tuple _ _ -> error "cannot compare tuple with null"

tPureExp (ABS.EEq left right) tyvars = do
  tLeft <- tPureExp left tyvars
  tRight <-tPureExp right tyvars
  check tLeft tRight 
    where
           check (HS.Paren lexp) rexp = check lexp rexp -- eliminate parentheses
           check lexp (HS.Paren rexp) = check lexp rexp -- eliminate parentheses
           check leftapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident leftVarName)))) rexp = do
               (fscope,cscope,_) <- ask                                 
               return $ case rexp of 
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                     in 
                       case joinSub qtypLeft qtypRight of
                         Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                         Nothing -> error "incomparable types"
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                     in case joinSub qtypLeft qtypRight of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
           check leftapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident leftVarName)))) rexp = do
               (fscope,cscope,_) <- ask                                 
               return $ case rexp of 
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                     in case joinSub qtypLeft qtypRight of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) cscope))
                     in case joinSub qtypLeft qtypRight of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
           check (HS.App _ _) (HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) _)   = error "equality coupled with function calls not implemented yet" -- TODO
           -- then it should be an equality between pure expressions
           check tLeft tRight = return $ HS.Paren $ HS.InfixApp tLeft (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==") tRight
         
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

tPureExp (ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Nil")])) _ = 
    return $ HS.Con $ HS.Special HS.ListCon -- for the translation to []

tPureExp (ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "EmptyMap")])) _ = 
    return $ HS.Var $ HS.UnQual $ HS.Ident "empty" -- for the translation to Data.Map

tPureExp (ABS.ESinglConstr (ABS.QType qids)) _ = return $
  let mids = init qids
      tid@(ABS.TypeIdent sid) = (\ (ABS.QTypeSegment cid) -> cid) (last qids)
  in if tid `elem` concatMap exceptions ?moduleTable
     -- if is an exception constructor, replace it with its smart constructor
     then HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ headToLower sid  
     else
         HS.Con $ (if null mids 
                   then HS.UnQual 
                   else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                  ) $ HS.Ident sid

tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Triple")]) pexps) tyvars =   
    if length pexps == 3
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else error "wrong number of arguments to Triple"

tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Pair")]) pexps) tyvars = 
    if length pexps == 2
    then do
      texps <- mapM (\ pexp -> tPureExp pexp tyvars) pexps
      return $ HS.Tuple HS.Boxed texps
    else error "wrong number of arguments to Pair"

tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Cons")]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.Paren (HS.InfixApp 
                       texp1
                       (HS.QConOp $ HS.Special $ HS.Cons)
                       texp2)
tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Cons")]) _) _ = error "wrong number of arguments to Cons"

tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "InsertAssoc")]) [pexp1, pexp2]) tyvars = do
  texp1 <- tPureExp pexp1 tyvars
  texp2 <- tPureExp pexp2 tyvars
  return $ HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") texp1) texp2
tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "InsertAssoc")]) _) _ = error "wrong number of arguments to InsertAssoc"

tPureExp (ABS.EParamConstr qids args) tyvars = do
    tcon <- tPureExp (ABS.ESinglConstr qids) tyvars -- first translate the constructor
    foldlM (\ acc nextArg -> do
              targ <- tPureExp nextArg tyvars 
              return $ HS.App acc targ) tcon args

tPureExp (ABS.EVar var@(ABS.Ident pid)) _tyvars = do
    (fscope, cscope,_) <- ask
    return $ case M.lookup var fscope of
      Nothing -> case M.lookup var cscope of -- lookup in the cscope
                  -- if it of an int type, upcast it
                  Just (ABS.TSimple (ABS.QType ([ABS.QTypeSegment (ABS.TypeIdent "Int")]))) -> 
                      HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid)
                  Just t -> HS.Paren $ (if isInterface t
                                       then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                                       else id)
                           (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid)
                  Nothing -> error $ pid ++ " not in scope" -- return $ HS.Var $ HS.UnQual $ HS.Ident pid -- TODO: this should be turned into warning
        -- if it of an int type, upcast it
      Just (ABS.TSimple (ABS.QType ([ABS.QTypeSegment (ABS.TypeIdent "Int")]))) -> HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident pid)
      Just t -> HS.Paren $ (if isInterface t
                           then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                           else id) 
               (HS.Var $ HS.UnQual $ HS.Ident pid)

tPureExp (ABS.ELit lit) _ = return $ case lit of
                                    ABS.LStr str ->  HS.Lit $ HS.String str
                                    ABS.LInt i ->  HS.Lit $ HS.Int i
                                    ABS.LThis -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                    ABS.LNull -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "null")


-- this is a trick for sync_call and async_call TODO: error "Cannot compile object accesses in mathematically pure expressions"
-- translate this.field
tPureExp (ABS.EThis (ABS.Ident ident)) _ = return $ HS.Var $ HS.UnQual $ HS.Ident ("__" ++ ident)


tEffExp :: (?moduleTable::ModuleTable) => ABS.EffExp -> ExprM HS.Exp
tEffExp (ABS.New (ABS.TSimple (ABS.QType qtids)) pexps) = tNewOrNewLocal "new" qtids pexps 
tEffExp (ABS.New _ _) = error "Not valid class name"
tEffExp (ABS.NewLocal (ABS.TSimple (ABS.QType qtids)) pexps) = tNewOrNewLocal "new_local" qtids pexps
tEffExp (ABS.NewLocal _ _) = error "Not valid class name"


tEffExp (ABS.SyncMethCall pexp (ABS.Ident method) args) = tSyncOrAsync "sync" pexp method args
tEffExp (ABS.AsyncMethCall pexp (ABS.Ident method) args) = tSyncOrAsync "async" pexp method args

-- normalize
tEffExp (ABS.ThisSyncMethCall method args) = tEffExp (ABS.SyncMethCall (ABS.ELit $ ABS.LThis) method args)
-- normalize
tEffExp (ABS.ThisAsyncMethCall method args) = tEffExp (ABS.AsyncMethCall (ABS.ELit $ ABS.LThis) method args)

tEffExp (ABS.Get pexp) = do
  texp <- tPureExp pexp []
  return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "get") texp

-- | shorthand generator, because new and new local are similar
tNewOrNewLocal :: (?moduleTable::ModuleTable) => String -> [ABS.QualTypeSegment] -> [ABS.PureExp] -> ExprM HS.Exp
tNewOrNewLocal newOrNewLocal qtids pexps = do 
  texps <- foldlM
           (\ acc pexp -> do
              texp <- tPureExp pexp []
              return $ HS.App acc texp)
           (HS.Var  
                  ((let mids = init qtids
                    in
                      if null mids
                      then HS.UnQual
                      else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                   (HS.Ident $ "__" ++ headToLower ( (\ (ABS.QTypeSegment (ABS.TypeIdent cid)) -> cid) (last qtids))))) pexps
  return $ HS.App (HS.Var $ HS.UnQual $ HS.Ident newOrNewLocal) texps

-- | shorthand generator, because sync and async are similar
tSyncOrAsync :: (?moduleTable::ModuleTable) => String -> ABS.PureExp -> String -> [ABS.PureExp] -> ExprM HS.Exp
tSyncOrAsync syncOrAsync pexp method args = do
  texp <- tPureExp pexp [] -- the callee object
  targs <- foldlM                                -- the method's arguments
         (\ acc arg -> do
            targ <- tPureExp arg []
            return $ HS.App acc targ)
         (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_" ++ syncOrAsync)
         args
  return $ HS.Paren $ HS.App targs texp


tFunPat :: ABS.Pattern -> HS.Pat
tFunPat (ABS.PIdent (ABS.Ident pid)) = HS.PVar $ HS.Ident $ pid
tFunPat (ABS.PSinglConstr (ABS.TypeIdent "Nil")) = HS.PList []
tFunPat (ABS.PSinglConstr (ABS.TypeIdent tid)) = HS.PApp (HS.UnQual $ HS.Ident tid) []
tFunPat (ABS.PParamConstr (ABS.TypeIdent "Triple") subpats) | length subpats == 3 = HS.PTuple HS.Boxed (map tFunPat subpats)
                                                               | otherwise = error "wrong number of arguments to Triple"
tFunPat (ABS.PParamConstr (ABS.TypeIdent "Pair") subpats) | length subpats == 2 = HS.PTuple HS.Boxed (map tFunPat subpats)
                                                             | otherwise = error "wrong number of arguments to Pair"
tFunPat (ABS.PParamConstr (ABS.TypeIdent "Cons") [subpat1, subpat2]) = HS.PParen (HS.PInfixApp 
                                                                          (tFunPat subpat1)
                                                                          (HS.Special $ HS.Cons)
                                                                          (tFunPat subpat2))
tFunPat (ABS.PParamConstr (ABS.TypeIdent "Cons") _) = error "wrong number of arguments to Cons"
tFunPat (ABS.PParamConstr (ABS.TypeIdent "InsertAssoc") _) = error "InsertAssoc is unsafe, you should avoid it."
tFunPat (ABS.PParamConstr (ABS.TypeIdent tid) subpats) = HS.PApp (HS.UnQual $ HS.Ident tid) (map tFunPat subpats)
tFunPat ABS.PUnderscore = HS.PWildCard
tFunPat (ABS.PLit lit) = HS.PLit $ case lit of
                                         (ABS.LStr str) ->  HS.String str
                                         (ABS.LInt i) ->  HS.Int i
                                         _ -> error "this or null are not allowed in pattern syntax"



-- | translate an ABS Type or a TypeVar to HS type
tTypeOrTyVar :: [ABS.TypeIdent] -> ABS.Type -> HS.Type
tTypeOrTyVar tyvars (ABS.TSimple (ABS.QType qtids))  = 
       let joinedTid = joinQualTypeIds qtids    
       in if (ABS.TypeIdent joinedTid) `elem` tyvars -- type variable
          then HS.TyVar $ HS.Ident $ headToLower joinedTid
          else HS.TyCon $ if length qtids == 1 
                          then HS.UnQual $ HS.Ident joinedTid -- unqual
                          else HS.Qual (HS.ModuleName (joinQualTypeIds (init qtids))) -- qual
                                   (HS.Ident $ (\ (ABS.QTypeSegment (ABS.TypeIdent tid)) -> tid) (last qtids))

tTypeOrTyVar tyvars (ABS.TGen qtyp tyargs) = foldl (\ tyacc tynext -> HS.TyApp tyacc (tTypeOrTyVar tyvars tynext)) (tType (ABS.TSimple qtyp)) tyargs

-- | shorthand for only translate ABS Types (no typevars) to HS types 
tType :: ABS.Type -> HS.Type
tType t = tTypeOrTyVar [] t     -- no type variables in scope

-- unify two interfaces, to their *Common interface*
-- will return Nothing if the interfaces are not unifiable
joinSub :: (?moduleTable::ModuleTable) => ABS.QualType -> ABS.QualType -> (Maybe ABS.QualType)
joinSub interf1 interf2 | interf1 == interf2 = Just interf1 -- same interface subtyping
joinSub interf1 interf2 | otherwise = 
  let 
    unionedST = (M.unions $ map hierarchy ?moduleTable) :: M.Map ABS.TypeIdent [ABS.QualType]
    canReach :: ABS.QualType -> ABS.QualType -> Bool
    canReach (ABS.QType qids) principal = let ABS.QTypeSegment sub = last qids in
                                             case M.lookup sub unionedST of
                                               Just sups -> if principal `elem` sups
                                                           then True
                                                           else any (\ sup -> canReach sup principal) sups
                                               Nothing -> False
                                             in
                                               if interf1 `canReach` interf2
                                               then Just interf2
                                               else if interf2 `canReach` interf1
                                                    then Just interf1
                                                    else Nothing


tAwaitGuard :: (?moduleTable::ModuleTable) => ABS.Guard -> String -> ExprM HS.Exp
tAwaitGuard (ABS.VarGuard (ABS.Ident ident)) _cls = return $ HS.App
                                                          (HS.Con $ HS.UnQual $ HS.Ident "FutureGuard")
                                                          (HS.Var $ HS.UnQual $ HS.Ident ident)
tAwaitGuard (ABS.ExpGuard pexp) _cls = do
  (_,cscope,_) <- ask
  vcscope <- visible_cscope
  let awaitFields = collectThisVars pexp vcscope
  texp <- tPureExp pexp []
  return $ HS.App (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ThisGuard") 
                   (HS.List (map (HS.Lit . HS.Int . toInteger) (findIndices ((\ (ABS.Ident field) -> field `elem` awaitFields)) (M.keys cscope)))))
                                             texp

tAwaitGuard (ABS.AndGuard gl gr) cls = do
  tleft <- tAwaitGuard gl cls 
  tright <- tAwaitGuard gr cls
  return $ HS.Paren $ HS.InfixApp 
         tleft
         (HS.QVarOp $ HS.UnQual  $ HS.Symbol ":&:")
         tright

tAwaitGuard (ABS.FieldGuard (ABS.Ident ident)) cls = error "Not implemented yet, take Cosimo's consideration into account"

interf :: ExprM String
interf = do 
  (_,_,i) <- ask
  return i

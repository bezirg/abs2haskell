{-# LANGUAGE ImplicitParams #-}
module Lang.ABS.Compiler.Utils
    (tModuleName
    ,joinQualTypeIds
    ,identI
    ,symbolI
    ,headToLower
    ,collectThisVars
    ,collectPatVars
    ,collectAssigns
    ,isInterface
    ,visible_cscope
    ,typOfConstrType
    ) where

import Lang.ABS.Compiler.Base
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS

import Control.Monad (when, liftM)
import Data.List (intersperse, nub, findIndices, (\\), elemIndices, mapAccumL)
import Data.Char (toLower)
import qualified Data.Map as M (member, unions, insertWith, (\\))
import Control.Monad.Trans.Reader (ask)

-- generate haskell code - helper functions

tModuleName :: ABS.QualType -> HS.ModuleName
tModuleName (ABS.QType qtis) = HS.ModuleName $ joinQualTypeIds qtis

joinQualTypeIds :: [ABS.QualTypeSegment] -> String
joinQualTypeIds qtids = concat $ intersperse "." $ map (\ (ABS.QTypeSegment (ABS.TypeIdent str)) -> str) qtids


-- | create Include-qualified Haskell *identifiers* for the generated stub code to *not clash* with ABS user-written code
identI :: String -> HS.QName
identI str = HS.Qual (HS.ModuleName "I__") (HS.Ident str)

-- | create Include-qualified Haskell *symbols* for the generated stub code to *not clash* with ABS user-written code
symbolI :: String -> HS.QName
symbolI sym = HS.Qual (HS.ModuleName "I__") (HS.Symbol sym)

-- | Used for turning an ABS type variable (e.g. A,B,DgFx) to HS type variable (a,b,dgFx)
headToLower :: String -> String
headToLower (x:xs) = toLower x : xs


-- | Querying an expression AST
-- collects pure variables and class attributes
-- TODO: use syb
collectThisVars                              :: ABS.PureExp -- ^ the exp to scan
                                     -> ScopeTable  -- the current class scope
                                     -> [String]    -- the names of the occuring vars
collectThisVars (ABS.Let _ pexp1 pexp2) ccs      = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.If pexp1 pexp2 pexp3) ccs   = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs ++ collectThisVars pexp3 ccs
collectThisVars (ABS.Case pexp cbranches) ccs    = collectThisVars pexp ccs ++ concatMap (\ (ABS.CaseBranc _ pexp') -> collectThisVars pexp' ccs) cbranches
collectThisVars (ABS.EOr pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EAnd pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EEq pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.ENeq pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.ELt pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.ELe pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EGt pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EGe pexp1 pexp2) ccs        = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EAdd pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.ESub pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EMul pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EDiv pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.EMod pexp1 pexp2) ccs       = collectThisVars pexp1 ccs ++ collectThisVars pexp2 ccs
collectThisVars (ABS.ELogNeg pexp) ccs           = collectThisVars pexp ccs
collectThisVars (ABS.EIntNeg pexp) ccs           = collectThisVars pexp ccs
collectThisVars (ABS.EFunCall _ pexps) ccs          = concatMap ((flip collectThisVars) ccs) pexps
collectThisVars (ABS.ENaryFunCall _ pexps) ccs      = concatMap ((flip collectThisVars) ccs) pexps
collectThisVars (ABS.EParamConstr _ pexps) ccs    = concatMap ((flip collectThisVars) ccs) pexps
collectThisVars (ABS.EThis (ABS.Ident attr)) _ccs = [attr] -- qualify it
collectThisVars (ABS.EVar ident@(ABS.Ident var)) ccs = if ident `M.member` ccs -- currentClassScope
                                               then [var]
                                               else []
collectThisVars _ _ = []

-- | collects all vars from a case pattern
collectPatVars :: ABS.Pattern -> [ABS.Ident]
collectPatVars (ABS.PIdent i) = [i]
collectPatVars (ABS.PParamConstr _ ps) = concatMap collectPatVars ps
collectPatVars _ = []

collectAssigns :: ABS.Stm -> ScopeTable -> [String]
collectAssigns (ABS.SBlock stmts) fscope = concatMap ((flip collectAssigns) fscope) stmts
collectAssigns (ABS.SWhile _ stmt) fscope = collectAssigns stmt fscope
collectAssigns (ABS.SIf _ stmt) fscope = collectAssigns stmt fscope
collectAssigns (ABS.SIfElse _ stmt1 stmt2) fscope = collectAssigns stmt1 fscope ++ collectAssigns stmt2 fscope
-- old changed variables
collectAssigns (ABS.SAss ident@(ABS.Ident var) _) fscope = if ident `M.member` fscope
                                                           then [var]
                                                           else []
-- and newly introduced variables
-- ignore fieldass, since they are iorefs
collectAssigns (ABS.SDec _ (ABS.Ident var)) _ = [var]
collectAssigns _ _ = []

typOfConstrType :: ABS.ConstrType -> ABS.Type
typOfConstrType (ABS.EmptyConstrType typ) = typ
typOfConstrType (ABS.RecordConstrType typ _) = typ


-- state queries

isInterface :: (?moduleTable :: ModuleTable) => ABS.Type -> Bool
isInterface (ABS.TSimple (ABS.QType [ABS.QTypeSegment iid])) =  iid `M.member` (M.unions (map methods ?moduleTable))
isInterface _ = False

-- helpers
visible_cscope :: ExprM ScopeTable
visible_cscope = do
 (fscope, cscope, _) <- ask
 return $ cscope M.\\ fscope


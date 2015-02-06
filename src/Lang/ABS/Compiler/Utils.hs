{-# LANGUAGE ImplicitParams #-}
module Lang.ABS.Compiler.Utils
    (joinQualTypeIds
    ,joinTTypeIds
    ,identI
    ,symbolI
    ,headToLower
    ,collectVars
    ,collectPatVars
    ,collectAssigns
    ,isInterface
    ,typOfConstrType
    ,funScope
    ,errorPos, showPos
    ) where

import Lang.ABS.Compiler.Base
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS

import Data.List (intersperse)
import Data.Char (toLower)
import qualified Data.Map as M (member, unions, (\\))
import Control.Monad.Trans.Reader (ask, runReader)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)

-- generate haskell code - helper functions

joinQualTypeIds :: [ABS.QTypeSegment] -> String
joinQualTypeIds qtids = concat $ intersperse "." $ map (\ (ABS.QTypeSegmen (ABS.UIdent (_,str))) -> str) qtids

joinTTypeIds :: [ABS.TTypeSegment] -> String
joinTTypeIds qtids = concat $ intersperse "." $ map (\ (ABS.TTypeSegmen (ABS.UIdent (_,str))) -> str) qtids


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
collectVars                              :: ABS.PureExp -- ^ the exp to scan
                                     -> ScopeTable  -- the current class scope
                                     -> [String]    -- the names of the occuring vars
collectVars (ABS.Let _ pexp1 pexp2) ccs      = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.If pexp1 pexp2 pexp3) ccs   = collectVars pexp1 ccs ++ collectVars pexp2 ccs ++ collectVars pexp3 ccs
collectVars (ABS.Case pexp cbranches) ccs    = collectVars pexp ccs ++ concatMap (\ (ABS.CaseBranc _ pexp') -> collectVars pexp' ccs) cbranches
collectVars (ABS.EOr pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EAnd pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EEq pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.ENeq pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.ELt pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.ELe pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EGt pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EGe pexp1 pexp2) ccs        = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EAdd pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.ESub pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EMul pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EDiv pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.EMod pexp1 pexp2) ccs       = collectVars pexp1 ccs ++ collectVars pexp2 ccs
collectVars (ABS.ELogNeg pexp) ccs           = collectVars pexp ccs
collectVars (ABS.EIntNeg pexp) ccs           = collectVars pexp ccs
collectVars (ABS.EFunCall _ pexps) ccs          = concatMap ((flip collectVars) ccs) pexps
collectVars (ABS.ENaryFunCall _ pexps) ccs      = concatMap ((flip collectVars) ccs) pexps
collectVars (ABS.EParamConstr _ pexps) ccs    = concatMap ((flip collectVars) ccs) pexps
collectVars (ABS.EThis (ABS.LIdent (_,attr))) _ccs = [attr] -- qualify it
collectVars (ABS.EVar ident@(ABS.LIdent (_,var))) ccs = if ident `M.member` ccs -- currentClassScope
                                                        then [var]
                                                        else []
collectVars _ _ = []

-- | collects all vars from a case pattern
collectPatVars :: ABS.Pattern -> [ABS.LIdent]
collectPatVars (ABS.PIdent i) = [i]
collectPatVars (ABS.PParamConstr _ ps) = concatMap collectPatVars ps
collectPatVars _ = []

collectAssigns :: ABS.Stm -> ScopeTable -> [String]
collectAssigns (ABS.SBlock stmts) fscope = concatMap ((flip collectAssigns) fscope) stmts
collectAssigns (ABS.SWhile _ stmt) fscope = collectAssigns stmt fscope
collectAssigns (ABS.SIf _ stmt) fscope = collectAssigns stmt fscope
collectAssigns (ABS.SIfElse _ stmt1 stmt2) fscope = collectAssigns stmt1 fscope ++ collectAssigns stmt2 fscope
-- old changed variables
collectAssigns (ABS.SAss ident@(ABS.LIdent (_,var)) _) fscope = if ident `M.member` fscope
                                                                then [var]
                                                                else []
-- and newly introduced variables
-- ignore fieldass, since they are iorefs
collectAssigns (ABS.SDec _ (ABS.LIdent (_,var))) _ = [var]
collectAssigns _ _ = []

typOfConstrType :: ABS.ConstrType -> ABS.Type
typOfConstrType (ABS.EmptyConstrType typ) = typ
typOfConstrType (ABS.RecordConstrType typ _) = typ


-- state queries

isInterface :: (?moduleTable :: ModuleTable) => ABS.Type -> Bool
isInterface (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen iid])) =  iid `M.member` (M.unions (map methods ?moduleTable))
isInterface _ = False

-- helpers

funScope :: StmtM ScopeTable
funScope = do
  scopes <- lift get
  return $ M.unions scopes

errorPos :: (Int, Int) -> String -> a
errorPos pos msg = error ("[error #" ++ showPos pos ++ "]" ++  msg)

showPos :: (Int, Int) -> String
showPos (row,col) = show row ++ ":" ++ show col

module Lang.ABS.Compiler.Top
    (tProg
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Utils
import Lang.ABS.Compiler.Stmt (tBlockWithReturn, tInitBlockWithReturn)
import Lang.ABS.Compiler.Expr (tPureExp,tBody, tType, tTypeOrTyVar)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import qualified Data.Map as M
import Data.List ((\\), mapAccumL, nub, find)
import Data.Maybe
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (runReader)

-- | Takes the name of the ABS source file and its parsed AST
--
-- Returns  a list of haskell ASTs, because 1 ABS program AST may correspond to more than 1 haskell module files/asts
tProg :: (?moduleTable::ModuleTable) => ABS.Program -> [HS.Module]
tProg (ABS.Prog moduls) = map tModul moduls

tModul :: (?moduleTable::ModuleTable) => ABS.Module -> HS.Module
tModul (ABS.Modul mname@(ABS.QTyp qsegs) exports imports decls maybeMain) = let strModuleName = joinQualTypeIds qsegs in
               HS.Module HS.noLoc (HS.ModuleName strModuleName) 
                     [HS.LanguagePragma HS.noLoc [
                                              HS.Ident "NoImplicitPrelude" -- for not importing haskell's prelude
                                              ,HS.Ident "ExistentialQuantification" -- for heterogenous collections
                                              ,HS.Ident "MultiParamTypeClasses" -- for subtyping
                                              ,HS.Ident "PatternSignatures" -- for inlining type annotations
                                              ,HS.Ident "FlexibleContexts" -- for some type inference of methods
                                              ,HS.Ident "DeriveDataTypeable" -- for defining ABS exceptions (exceptions are dynamically typed in haskell)
                                              ,HS.Ident "DeriveGeneric" -- for deriving Binary instances for object records
                                              ]
                     , HS.OptionsPragma HS.noLoc (Just HS.GHC) "-w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts"
                     ] 
                     Nothing 
                     (Just $ (case maybeMain of
                                ABS.JustBlock _ _ -> ((HS.EVar $ HS.UnQual $ HS.Ident "main") :)
                                ABS.NoBlock -> id) $ concatMap (tExport mname) exports)
                     -- IMPORT HEADER for the generated haskell module
                     ([HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Runtime.Base", 
                                     HS.importSrc = False, 
                                     HS.importQualified = True,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Just $ HS.ModuleName "I__",
                                     HS.importSpecs = Nothing
                                    }
                      ,HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Runtime.Core", 
                                     HS.importSrc = False, 
                                     HS.importQualified = True,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Just $ HS.ModuleName "I__",
                                     HS.importSpecs = Nothing
                                    }
                     ,HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Compiler.Include", 
                                     HS.importSrc = False, 
                                     HS.importQualified = True,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Just (HS.ModuleName "I__"),
                                     HS.importSpecs = Nothing
                                    }
                      ,HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Runtime.Prim", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    }
                     ,HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.StdLib", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    } 
                     ,HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Data.Binary", 
                                     HS.importSrc = False, 
                                     HS.importQualified = True,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Just (HS.ModuleName "B__"),
                                     HS.importSpecs = Nothing
                                    } 
                     ]
                      ++        -- the translated ABS imports
                      map tImport imports
                     ) (let ?moduleName = mname in tDecls decls ++ tMain maybeMain)


tExport :: (?moduleTable::ModuleTable) => ABS.QType -> ABS.Export -> [HS.ExportSpec]
tExport (ABS.QTyp qsegs) ABS.StarExport = [HS.EModuleContents (HS.ModuleName $ joinQualTypeIds qsegs)]
tExport _ (ABS.StarFromExport (ABS.QTyp qsegs)) = [HS.EModuleContents (HS.ModuleName $ joinQualTypeIds qsegs)]
tExport m (ABS.AnyExport es) = concatMap tExport' es
    where tExport' (ABS.AnyIden (ABS.LIdent (_,var))) = [HS.EVar (HS.UnQual $ HS.Ident var)]
          tExport' (ABS.AnyTyIden ident@(ABS.UIdent (_,var))) = case find (\ mi -> moduleName mi == m) ?moduleTable of
                                                                Just (ModuleInfo _ _ _ _ exs _ _ _) -> if ident `elem` exs
                                                                                             then [HS.EThingAll (HS.UnQual $ HS.Ident var), -- MyException
                                                                                                   -- __myException smart constructor
                                                                                                   HS.EVar (HS.UnQual $ HS.Ident $  "__" ++ headToLower var)]
                                                                                             else [HS.EThingAll (HS.UnQual $ HS.Ident var)] -- just datatype
                                                                Nothing -> [HS.EThingAll (HS.UnQual $ HS.Ident var)] -- here we wrongly export all its constructors, but it better fits ABS, since ABS module-system cannot distinquish between type constructors and data constructors and exports both if the names are the same, e.g.:  data A = A;
tExport _ _ = error "we cannot translate that export yet, bcs ABS' module-system has problems"

tImport :: (?moduleTable::ModuleTable) => ABS.Import -> HS.ImportDecl
tImport (ABS.StarFromImport _ityp (ABS.QTyp qsegs)) = HS.ImportDecl HS.noLoc (HS.ModuleName (joinQualTypeIds qsegs)) False False Nothing Nothing (Just (True, [HS.IVar $ HS.Ident "main"])) -- hiding main
tImport (ABS.AnyImport _ityp (ABS.TTyp qsegs) aid) = HS.ImportDecl HS.noLoc (HS.ModuleName (joinTTypeIds qsegs)) True False Nothing Nothing (Just (False, tImport' (joinTTypeIds qsegs) aid))
tImport (ABS.AnyFromImport _ityp aids (ABS.QTyp qsegs)) = HS.ImportDecl HS.noLoc (HS.ModuleName (joinQualTypeIds qsegs)) False False Nothing Nothing (Just (False, concatMap (tImport' (joinQualTypeIds qsegs)) aids))

tImport' :: (?moduleTable::ModuleTable) => String -> ABS.AnyIdent -> [HS.ImportSpec]
tImport' _ (ABS.AnyIden (ABS.LIdent (_,ident))) = [HS.IVar $ HS.Ident ident]
tImport' m (ABS.AnyTyIden ident@(ABS.UIdent (_,var))) = case find (\ (ModuleInfo _ (ABS.QTyp qsegs) _ _ _ _ _ _) -> joinQualTypeIds qsegs == m) ?moduleTable of
                                                     Just (ModuleInfo _ _ _ _ exs _ _ _) -> if ident `elem` exs
                                                                                       then [HS.IThingAll $ HS.Ident var, -- MyException
                                                                                             -- __myException smart constructor
                                                                                             HS.IVar (HS.Ident $  "__" ++ headToLower var)]
                                                                                       else [HS.IThingAll $ HS.Ident var] -- compromise since ABS cannot distinguish type constructors to data constructors
                                                     Nothing -> [HS.IThingAll $ HS.Ident var]

-- | Creates the mainABS wrapper i.e. main = main_is mainABS
-- only if the module has a main block and is declared as main source file in the conf
tMain :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => ABS.MaybeBlock -> [HS.Decl]
tMain ABS.NoBlock = []
tMain (ABS.JustBlock _ (ABS.Bloc block)) = 
       -- main can only return with: return Unit;
       HS.FunBind [HS.Match HS.noLoc (HS.Ident "mainABS") [HS.PVar $ HS.Ident "this"] Nothing (HS.UnGuardedRhs $ tBlockWithReturn block
                                                                      ("Top") -- class-name
                                                                      M.empty -- class-scope -- (error "No context for this")
                                                                      M.empty -- empty method params
                                                                      [] -- [scopetable]
                                                                      (error "no class context") -- interface-name
                                                                      []
                                                               ) (HS.BDecls [])]
                                      :
                                      [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "main")) Nothing 
                                             (HS.UnGuardedRhs (HS.App 
                                                                     (HS.App (HS.Var (identI "main_is"))
                                                                            (HS.Var (HS.UnQual (HS.Ident "mainABS")) ))
                                                                      (HS.Var (identI "initRemoteTable") ))) (HS.BDecls [])]

tDecls :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => [ABS.AnnotDecl] -> [HS.Decl]
tDecls = concatMap (\ (ABS.AnnDec _ d) -> tDecl d)

-- can return more than 1 decl, because of creating accessors for records
-- or putting type signatures
tDecl :: (?moduleTable::ModuleTable,?moduleName::ABS.QType) => ABS.Decl -> [HS.Decl]

tDecl (ABS.ExceptionDecl constr) = let ((_,cid), cargs) = case constr of
                                                            ABS.SinglConstrIdent (ABS.UIdent tid) -> (tid, [])
                                                            ABS.ParamConstrIdent (ABS.UIdent tid) args -> (tid, args)
                                       in
                                         -- 1) a data MyException = MyException(args)
                                         [HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident cid) [] 
                                               -- one sole constructor with the same name as the exception name
                                               [HS.QualConDecl HS.noLoc [] [] (HS.ConDecl (HS.Ident cid)
                                                                            (map (HS.UnBangedTy . tType . typOfConstrType) cargs))]
                                               -- three deriving for exception datatypes
                                               (map (\ der -> (identI der, [])) ["Eq","Show","Typeable"])
                                         -- 2) a instance Exception MyException
                                         ,HS.InstDecl HS.noLoc [] (identI "Exception") [HS.TyCon $ HS.UnQual $ HS.Ident cid] []
                                         -- 3) a __myException smartconstructor
                                         ,HS.FunBind [HS.Match HS.noLoc (HS.Ident $ "__" ++ headToLower cid)
                                                            (map (\ i -> HS.PVar $ HS.Ident $ "__" ++ show i )  [1..length cargs])
                                                            Nothing
                                                            (HS.UnGuardedRhs $ (HS.App (HS.Var $ identI "SomeException") 
                                                                                (HS.Paren
                                                                                 (foldl (\ acc i -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ show i))
                                                                                            (HS.Con $ HS.UnQual $ HS.Ident cid) [1..length cargs]))))
                                                            (HS.BDecls [])]
                                         ]
                                                    
    -- TODO pass the type variables env , change tType to tTypeOrTyVar
tDecl (ABS.TypeDecl (ABS.UIdent (_,tid)) typ) = [HS.TypeDecl HS.noLoc (HS.Ident tid) [{- no typevars -}] (tType typ)]

tDecl (ABS.TypeParDecl (ABS.UIdent (_,tid)) tyvars typ) = [HS.TypeDecl HS.noLoc (HS.Ident tid) 
                                                                 (map (\ (ABS.UIdent (_,varid)) -> HS.UnkindedVar $ HS.Ident $ headToLower $  varid) tyvars) (tTypeOrTyVar tyvars typ)]

tDecl (ABS.DataDecl tid constrs) =  tDecl (ABS.DataParDecl tid [] constrs) -- just parametric datatype with empty list of type variables

tDecl (ABS.DataParDecl (ABS.UIdent (_,tid)) tyvars constrs) = let
           -- check if there is an exception carried inside the constructors
           hasEx = or $ map (\case
                             ABS.SinglConstrIdent (ABS.UIdent _) -> False
                             ABS.ParamConstrIdent (ABS.UIdent _) args -> 
                                 "Exception" `elem` (map
                                                     (\case
                                                      ABS.EmptyConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,x))])) -> x
                                                      ABS.RecordConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,x))])) _ -> x
                                                      _ -> "") args)) constrs
           pConstr constr = HS.UnQual $ HS.Ident $ case constr of
                                                     ABS.SinglConstrIdent (ABS.UIdent (_,tid)) -> tid
                                                     ABS.ParamConstrIdent (ABS.UIdent (_,tid)) _ -> tid

           pConstrArgs :: ABS.ConstrIdent -> String -> [HS.Pat]
           pConstrArgs constr prefix = case constr of
                                  ABS.SinglConstrIdent (ABS.UIdent tid) -> []
                                  ABS.ParamConstrIdent (ABS.UIdent tid) args -> 
                                      snd (mapAccumL (\ acc arg -> (acc+1,
                                                     case arg of
                                                       ABS.EmptyConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Exception"))]))  -> 
                                                           HS.PApp (identI "SomeException") [HS.PVar $ HS.Ident $ prefix ++ show acc]
                                                       ABS.RecordConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Exception"))])) _ -> 
                                                           HS.PApp (identI "SomeException") [HS.PVar $ HS.Ident $ prefix ++ show acc]
                                                       _ -> HS.PVar $ HS.Ident $ prefix ++ show acc
                                                             )) 1 args)
      in
        -- create the data declaration
        HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident tid) (map (\ (ABS.UIdent (_,varid)) -> HS.UnkindedVar $ HS.Ident $ headToLower $  varid) tyvars)
           (map (\case
                 ABS.SinglConstrIdent (ABS.UIdent (_,cid)) -> HS.QualConDecl HS.noLoc [] [] (HS.ConDecl (HS.Ident cid) []) -- no constructor arguments
                 ABS.ParamConstrIdent (ABS.UIdent (_,cid)) args -> HS.QualConDecl HS.noLoc [] [] (HS.ConDecl (HS.Ident cid) (map (HS.UnBangedTy . tTypeOrTyVar tyvars . typOfConstrType) args))) constrs)
           (if hasEx then [(identI "Typeable",[]), (identI "Generic",[])] else [(identI "Eq", []), (identI "Show", []), (identI "Typeable",[]), (identI "Generic",[])])
         
        -- create an empty instance of Binary (derived by generic)
        :HS.InstDecl HS.noLoc (map (\ (ABS.UIdent (_,varid)) -> HS.ClassA (identB "Binary") [HS.TyVar $ HS.Ident $ headToLower varid]) tyvars) (identB "Binary") [HS.TyParen (foldl HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident $ tid) (map (\ (ABS.UIdent (_,varid)) -> HS.TyVar $ HS.Ident $ headToLower varid) tyvars))] []
        :
        if hasEx                
        -- then manual Eq instance
        -- TODO: Eq of Exceptions through the show instance, i.e. show ex1 == show ex2  (it is error-prone), change to MySomeException and renew Cloud.Exception API
        then [HS.InstDecl HS.noLoc (map (\ (ABS.UIdent (_,a)) -> HS.ClassA (identI "Eq") [HS.TyVar $ HS.Ident $ headToLower a]) tyvars) (identI "Eq") 
                    [foldl (\ acc (ABS.UIdent (_,a)) -> HS.TyApp acc $ HS.TyVar $ HS.Ident $ headToLower a) (HS.TyCon $ HS.UnQual $ HS.Ident tid) tyvars] [HS.InsDecl $ HS.FunBind
                ((map (\ constr ->  HS.Match HS.noLoc (HS.Symbol "==") [HS.PApp (pConstr constr) (pConstrArgs constr "__x"), 
                                                                          HS.PApp (pConstr constr) (pConstrArgs constr "__y")]
                      Nothing (HS.UnGuardedRhs $ case constr of
                                                   ABS.SinglConstrIdent (ABS.UIdent _) -> HS.Con $ HS.UnQual $ HS.Ident "True" -- no constr arguments then, True
                                                   ABS.ParamConstrIdent (ABS.UIdent _) args ->
                                                       snd (foldl (\ (c,exp) arg -> (c+1,
                                                                                          HS.InfixApp exp (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&") (
                                                         case arg of
                                                           ABS.EmptyConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Exception"))]))  -> 
                                                               HS.InfixApp (HS.App (HS.Var $ identI "show") (HS.Var $ HS.UnQual $ HS.Ident $ "__x" ++ show c))
                                                                  (HS.QVarOp $ HS.UnQual $ HS.Symbol "==")
                                                                  (HS.App (HS.Var $ identI "show") (HS.Var $ HS.UnQual $ HS.Ident $ "__y" ++ show c))
                                                           ABS.RecordConstrType (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Exception"))])) _ -> 
                                                               HS.InfixApp (HS.App (HS.Var $ identI "show") (HS.Var $ HS.UnQual $ HS.Ident $ "__x" ++ show c ))
                                                                     (HS.QVarOp $ HS.UnQual $ HS.Symbol "==")
                                                                     (HS.App (HS.Var $ identI "show") (HS.Var $ HS.UnQual $ HS.Ident $ "__y" ++ show c))
                                                           _ -> HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident $ "__x" ++ show c)
                                                               (HS.QVarOp $ HS.UnQual $ HS.Symbol "==")
                                                               (HS.Var $ HS.UnQual $ HS.Ident $ "__y" ++ show c)
                                                             ))) (1, HS.Con $ HS.UnQual $ HS.Ident "True")  args))
                                                        
                      (HS.BDecls [])) constrs)
                -- -- a catch-all case, for comparing different constructors
                ++ [HS.Match HS.noLoc (HS.Symbol "==") [HS.PWildCard, HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "False") (HS.BDecls [])]
             )]]
        else []                 -- derived
        ++
        -- create record accessors
        map (\ (ABS.LIdent (_,fname), consname, idx, len) ->  HS.FunBind [HS.Match HS.noLoc (HS.Ident fname) ([HS.PApp (HS.UnQual (HS.Ident consname)) (replicate idx HS.PWildCard ++ [HS.PVar (HS.Ident "a")] ++ replicate (len - idx - 1) HS.PWildCard)]) Nothing (HS.UnGuardedRhs (HS.Var (HS.UnQual (HS.Ident "a")))) (HS.BDecls [])]) (
             concatMap (\case
               ABS.SinglConstrIdent _ -> []
               ABS.ParamConstrIdent (ABS.UIdent (_,cid)) args -> -- taking the indices of fields
                                         let len = length args
                                         in
                                            foldl (\ acc (field, idx) ->  case field of
                                                                            ABS.EmptyConstrType _ -> acc
                                                                            ABS.RecordConstrType _ fid -> (fid, cid, idx, len):acc) [] (zip args [0..])
              ) constrs
                                                                                                                                                                                                                                                                                                                                  )

    -- empty interface extends automatically from Root superinterface
tDecl (ABS.InterfDecl tid@(ABS.UIdent (p,_)) ms) = tDecl (ABS.ExtendsDecl tid [ABS.QTyp $ [ABS.QTypeSegmen $ ABS.UIdent (p,"I__.Root")]]  ms) 

tDecl (ABS.ExtendsDecl iid@(ABS.UIdent (p,tname)) extends ms) = HS.ClassDecl 
                                                              HS.noLoc 
                                                              (map (\ (ABS.QTyp e) -> HS.ClassA (HS.UnQual $ HS.Ident $ joinQualTypeIds e ++ "_") [HS.TyVar (HS.Ident "a")]) extends)
                                                              (HS.Ident $ tname ++ "_") 
                                                              [HS.UnkindedVar (HS.Ident "a")]
                                                              [] -- no fundeps
                                                              (map (tMethSig tname) ms)
       : 
        -- type synonym for Objects typed by the interface
        -- data Interf1 = forall a. Interf1_ a => Interf1 (Obj a)

        HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident tname) [] [HS.QualConDecl HS.noLoc [HS.UnkindedVar $ HS.Ident "a"] [HS.ClassA (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyVar (HS.Ident "a")]] (HS.ConDecl (HS.Ident tname) [HS.UnBangedTy (HS.TyApp (HS.TyCon $ identI "Obj") (HS.TyVar $ HS.Ident "a"))])] []
                                                              
       : -- show instance
       HS.InstDecl HS.noLoc [] (identI "Show") [HS.TyCon $ HS.UnQual $ HS.Ident $ tname]
             [HS.InsDecl (HS.FunBind  [HS.Match HS.noLoc (HS.Ident "show") [HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Lit $ HS.String tname) (HS.BDecls [])])]
       -- Sub instances generation
       : generateSubSelf tname
       -- for lifting null to I, essentially null is a subtype of I
       : generateUnwrappedSub tname
       : generateSub tname (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent (p,"I__.Root")]) -- root class
       -- null class is an instance of any interface
       : HS.InstDecl HS.noLoc [] (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyCon $ identI "Null"] 
             (map (\ (ABS.AnnMethSig _ (ABS.MethSig _ (ABS.LIdent (_,mid)) _)) -> HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident mid) [] Nothing 
                                                                               (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "this should not happen. report the program to the compiler developers"))) (HS.BDecls [])]) ms)
       -- instance Eq I where (==) =   -- this is needed for ADTs deriving Eq
       : HS.InstDecl HS.noLoc [] (identI "Eq") [HS.TyCon $ HS.UnQual $ HS.Ident tname]
         [HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Symbol "==") 
                                   [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id1", HS.PVar $ HS.Ident "tid1"]],
                                      HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id2", HS.PVar $ HS.Ident "tid2"]]] Nothing (HS.UnGuardedRhs $  HS.InfixApp
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "id1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "id2")))
                        (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "tid1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "tid2")))) (HS.BDecls []),
                                   HS.Match HS.noLoc (HS.Symbol "==") (replicate 2 (HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "NullRef") []]))
                                     Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "True") (HS.BDecls []),                                   
                                   HS.Match HS.noLoc (HS.Symbol "==") [HS.PWildCard, HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "False") (HS.BDecls [])]]

       -- instance Ord I where compare = 
       : HS.InstDecl HS.noLoc [] (identI "Ord") [HS.TyCon $ HS.UnQual $ HS.Ident tname]
           [HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident "compare")
                                     [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id1", HS.PVar $ HS.Ident "tid1"]],
                                      HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id2", HS.PVar $ HS.Ident "tid2"]]] Nothing (HS.UnGuardedRhs $ (HS.App (HS.App (HS.Var $ identI "compare") (HS.Tuple HS.Boxed [HS.Var $ HS.UnQual $ HS.Ident "tid1", HS.Var $ HS.UnQual $ HS.Ident "id1"])) (HS.Tuple HS.Boxed [HS.Var $ HS.UnQual $ HS.Ident "tid2", HS.Var $ HS.UnQual $ HS.Ident "id2"])))
                                     (HS.BDecls []),
                                     HS.Match HS.noLoc (HS.Ident "compare")
                                     [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "NullRef") []],
                                      HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "NullRef") []]] Nothing (HS.UnGuardedRhs $ HS.Con $ identI "EQ") (HS.BDecls []),
                                     HS.Match HS.noLoc (HS.Ident "compare")
                                     [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (identI "NullRef") []],
                                      HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PWildCard]] Nothing (HS.UnGuardedRhs $ HS.Con $ identI "LT") (HS.BDecls []),
                                     HS.Match HS.noLoc (HS.Ident "compare")
                                     [HS.PWildCard, HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Con $ identI "GT") (HS.BDecls [])
                                    ]]
       -- instance B__.Binary I where
       : HS.InstDecl HS.noLoc [] (identB "Binary") [HS.TyCon $ HS.UnQual $ HS.Ident tname]
           [HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident "put")
                                       [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PVar $ HS.Ident "x"]] Nothing 
                                         (HS.UnGuardedRhs $ HS.Do [HS.Qualifier (HS.App (HS.Var $ identB "put") (HS.App (HS.Var $ identI "encodeFingerprint") (HS.App (HS.Var $ identI "fingerprint") (HS.Var $ HS.UnQual $ HS.Ident "x")))),
                                                                   HS.Qualifier (HS.App (HS.Var $ identB "put") (HS.Var $ HS.UnQual $ HS.Ident "x"))]) (HS.BDecls [])]
           ,HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident "get") [] Nothing
                                       (HS.UnGuardedRhs $ HS.Do [HS.Generator HS.noLoc (HS.PVar $ HS.Ident "fp") (HS.Var $ identB "get"),
                                                                 HS.Qualifier (HS.Case (HS.App (HS.App (HS.Var $ identI "lookup") (HS.App (HS.Var $ identI "decodeFingerprint") (HS.Var $ HS.UnQual $ HS.Ident "fp"))) (HS.Var $ HS.UnQual $ HS.Ident $ "stable_" ++ tname))
                                                                                 [HS.Alt HS.noLoc (HS.PApp (HS.UnQual $ HS.Ident "Just") [HS.PParen (HS.PApp (HS.UnQual $ HS.Ident $ "SomeGet_" ++ tname) [HS.PVar $ HS.Ident "someget"])]) (HS.UnGuardedAlt (HS.InfixApp (HS.Con $ HS.UnQual $ HS.Ident tname) (HS.QVarOp (HS.UnQual (HS.Symbol "<$!>"))) (HS.Var $ HS.UnQual $ HS.Ident "someget"))) (HS.BDecls [])
                                                                                 ,HS.Alt HS.noLoc (HS.PApp (HS.UnQual $ HS.Ident "Nothing") []) (HS.UnGuardedAlt (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String $ "Binary " ++ tname ++ ": fingerprint unknown"))) (HS.BDecls [])
                                                                                 ])]) (HS.BDecls [])]
           ]

       : HS.PatBind HS.noLoc (HS.PVar (HS.Ident $ "stable_" ++ tname)) Nothing (HS.UnGuardedRhs (HS.App (HS.Var (identI "fromList")) (HS.List $map (\ (ABS.UIdent (_,cname)) -> HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "mkSMapEntry"))) (HS.Paren (HS.ExpTypeSig HS.noLoc (HS.Var $ identI "undefined") (HS.TyApp (HS.TyCon (identI "Obj")) (HS.TyCon (HS.UnQual (HS.Ident cname)))))))) collectImplementingClasses))) (HS.BDecls [HS.TypeSig HS.noLoc [HS.Ident "mkSMapEntry"] (HS.TyForall (Just [HS.UnkindedVar (HS.Ident "a")]) [HS.ClassA (HS.UnQual (HS.Ident $ tname ++ "_")) [HS.TyVar (HS.Ident "a")],HS.ClassA (identI "Serializable") [HS.TyVar (HS.Ident "a")]] (HS.TyFun (HS.TyApp (HS.TyCon (identI "Obj")) (HS.TyVar (HS.Ident "a"))) (HS.TyTuple HS.Boxed [HS.TyCon (identI "Fingerprint"),HS.TyCon (HS.UnQual (HS.Ident $ "SomeGet_" ++ tname ))]))) , HS.FunBind [HS.Match HS.noLoc (HS.Ident "mkSMapEntry") [HS.PVar (HS.Ident "a")] Nothing (HS.UnGuardedRhs (HS.Tuple HS.Boxed [HS.App (HS.Var (identI "fingerprint")) (HS.Var (HS.UnQual (HS.Ident "a"))),HS.App (HS.Con (HS.UnQual (HS.Ident $ "SomeGet_" ++ tname))) (HS.Paren (HS.ExpTypeSig HS.noLoc (HS.Var (identB "get")) (HS.TyApp (HS.TyCon (identB "Get")) (HS.TyParen (HS.TyApp (HS.TyCon (identI "Obj")) (HS.TyVar (HS.Ident "a")))))))])) (HS.BDecls [])]])
       : HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident $ "SomeGet_" ++ tname) [] [HS.QualConDecl HS.noLoc [HS.UnkindedVar (HS.Ident "a")] [HS.ClassA (HS.UnQual (HS.Ident $ tname ++ "_")) [HS.TyVar (HS.Ident "a")],HS.ClassA (identI "Serializable") [HS.TyVar (HS.Ident "a")]] (HS.ConDecl (HS.Ident $ "SomeGet_" ++ tname) [HS.UnBangedTy $ HS.TyParen (HS.TyApp (HS.TyCon (identB "Get")) (HS.TyParen (HS.TyApp (HS.TyCon (identI "Obj")) (HS.TyVar (HS.Ident "a")))))])] []


       : generateSubs tname (filter (\ (ABS.QTyp qids) -> qids /= [ABS.QTypeSegmen $ ABS.UIdent (p,"I__.Root")])  extends) 



    where
    -- method_signature :: args -> Obj a (THIS) -> ABS result
    tMethSig :: String -> ABS.AnnotMethSignat -> HS.ClassDecl
    tMethSig ityp (ABS.AnnMethSig _ann (ABS.MethSig tReturn (ABS.LIdent (mpos,mname)) pars))  = 
        if mname == "run" && (tReturn /= ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((-1,-1), "Unit")]) || not (null pars))
        then errorPos mpos "run should have zero parameters and return type Unit"
        else HS.ClsDecl $ HS.TypeSig HS.noLoc [HS.Ident mname] $
               ((foldr  -- function application is right-associative
                 (\ tpar acc -> HS.TyFun tpar acc)
                 -- the this objectref passed as input to the method
                 (HS.TyApp ((HS.TyCon $ identI "ABS") 
                                   )
                  (tType tReturn))
                )
                (map (\ (ABS.Par typ _) -> tType typ) pars ++ [(HS.TyApp (HS.TyCon $ identI "Obj") (HS.TyVar $ HS.Ident "a"))]))

    collectImplementingClasses :: [ABS.UIdent]
    collectImplementingClasses = 
       M.foldlWithKey  (\ acc k v ->
                            if tname `elem` map (\ (ABS.QTyp qsegs) -> joinQualTypeIds qsegs) (collectSubs v)
                            then k:acc
                            else acc) [] (classes $ fromJust $ (find (\ m -> moduleName m == ?moduleName) ?moduleTable))

    -- normalize
tDecl (ABS.FunDecl fReturnTyp fid params body) = tDecl (ABS.FunParDecl fReturnTyp fid [] params body) -- no type variables

tDecl (ABS.FunParDecl fReturnTyp (ABS.LIdent (_,fid)) tyvars params body) = 
       [
        HS.FunBind [HS.Match HS.noLoc (HS.Ident fid) (map (\ (ABS.Par ptyp (ABS.LIdent (_,pid))) -> 
                                                            (\ pat -> if ptyp == ABS.TUnderscore
                                                                     then pat -- infer the parameter type
                                                                     else HS.PatTypeSig HS.noLoc pat (tTypeOrTyVar tyvars ptyp) -- wrap with an explicit type annotation
                                                            ) (HS.PVar (HS.Ident pid))) params)
                          Nothing (HS.UnGuardedRhs $  -- we don't support guards in ABS language
                                         (\ exp -> if fReturnTyp == ABS.TUnderscore 
                                                  then exp -- infer the return type
                                                  else HS.ExpTypeSig HS.noLoc exp (tTypeOrTyVar tyvars fReturnTyp)) -- wrap the return exp with an explicit type annotation
                                         (tBody body tyvars params)
                                   )  (HS.BDecls [])]
       ]

-- normalizing class declarations
tDecl (ABS.ClassDecl tident fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] [] fdecls maybeBlock mdecls)
tDecl (ABS.ClassParamDecl tident params fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident params [] fdecls maybeBlock mdecls)
tDecl (ABS.ClassImplements tident imps fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] imps fdecls maybeBlock mdecls)
tDecl (ABS.ClassParamImplements (ABS.UIdent (pos,clsName)) params imps ldecls maybeBlock rdecls) = -- TODO add check for imps, if a method is not implemented
       -- the record-ADT of the ABS class
        HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident clsName) [] 
              [HS.QualConDecl HS.noLoc [] [] $ HS.RecDecl (HS.Ident clsName) (map (\ ((ABS.LIdent (_,i)), t) -> ([HS.Ident $ headToLower clsName ++ "_" ++ i], 
                                                                                                                let t' = tType t
                                                                                                                in case t' of
                                                                                                                     HS.TyCon (HS.UnQual (HS.Ident ident)) -> (if ident `elem` ["Int", "Bool", "Rational", "Unit"] then HS.BangedTy else HS.UnBangedTy) (tType t)
                                                                                                                -- TODO: unpack the pairs+triples
                                                                                                                     _ -> HS.UnBangedTy (tType t)
                                                                                                               )) (M.toAscList allFields))]  [(identI "Typeable",[]), (identI "Generic", [])]
        :

        -- the smart constructor
        HS.FunBind [HS.Match HS.noLoc (HS.Ident $ "__" ++ headToLower clsName)
                    (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) params) Nothing 
                    (HS.UnGuardedRhs $ let is = fimports $ fromJust $ (find (\ m -> moduleName m == ?moduleName) ?moduleTable) in
                        foldr (\ fdecl acc -> case fdecl of
                           ABS.FieldAssignClassBody _t (ABS.LIdent (_, fid)) pexp -> do
                                 HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc)
                                       (runReader (tPureExp pexp []) allFields)
                           ABS.FieldClassBody t (ABS.LIdent (p,fid)) ->  
                               if isInterface t 
                               then HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc)
                                         (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.TSimple (ABS.QTyp qsegs)) -> joinQualTypeIds qsegs) t) $ (HS.Var $ HS.UnQual $ HS.Ident "null"))
                               else case t of
                                      -- it is an unitialized future (abs allows this)
                                      ABS.TGen (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Fut"))])  _ -> 
                                          HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc) (HS.Var $ identI "NullFutureRef")
                                      -- it is an unitialized promise (abs allows this)
                                      ABS.TGen (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Promise"))])  _ -> 
                                                -- TODO: this will not work fix later
                                          HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc) (HS.Var $ identI "NullFutureRef")
                                            -- maybe it is a foreign polymorphic datatype, 
                                            -- so it can be left uninitialized, because its creation may be monadic
                                      ABS.TGen (ABS.QTyp qsegs) _ -> case find (\case
                                                              ABS.AnyTyIden (ABS.UIdent (_, uid)) -> uid == joinQualTypeIds qsegs
                                                              _ -> False) is of
                                                                              Just _ -> HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc) (HS.Var $ identI "undefined")
                                                                              Nothing -> errorPos p "A field must be initialised if it is not of a reference type"
                                      -- maybe it is a foreign simple datatype, 
                                      ABS.TSimple (ABS.QTyp qsegs) -> case find (\case
                                                              ABS.AnyTyIden (ABS.UIdent (_, uid)) -> uid == joinQualTypeIds qsegs
                                                              _ -> False) is of
                                                                              Just _ -> HS.App (HS.Lambda HS.noLoc [HS.PVar $ HS.Ident fid] acc) (HS.Var $ identI "undefined")
                                                                              Nothing -> errorPos p "A field must be initialised if it is not of a reference type"
                                      _ -> errorPos p "A field must be initialised if it is not of a reference type"
                           ABS.MethClassBody _ _ _ _ -> case maybeBlock of
                                                          ABS.NoBlock -> acc
                                                          ABS.JustBlock _ _->  error "Second parsing error: Syntactic error, no method declaration accepted here")  (HS.RecConstr (HS.UnQual $ HS.Ident clsName) (map (\ (ABS.LIdent (_,pid)) -> HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ pid) (HS.Var $ HS.UnQual $ HS.Ident pid)) (M.keys allFields))) ldecls)
                    (HS.BDecls [])
                   ]

       -- show instance for object turned off, because we rely on show instance of interface wrapper-datatype
       -- a Show instance for object
       -- HS.InstDecl HS.noLoc [] (identI "Show") [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName]
       --       [HS.InsDecl (HS.FunBind  [HS.Match HS.noLoc (HS.Ident "show") [HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Lit $ HS.String clsName) (HS.BDecls [])])]
       :
       -- the Root instance
       HS.InstDecl HS.noLoc [{- empty context for now, may need to fix later -}] 
              (identI "Root_") -- interface name
              [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName]
              (
               -- the init method (optional)
               -- normalize to a method decl with name __init
               (case maybeBlock of
                  ABS.JustBlock _ b -> [tInitDecl "I__.Root" $ ABS.MethClassBody (error "compiler implementation") (ABS.LIdent (pos,"__init")) [] b]
                  ABS.NoBlock -> if isJust mRun -- it should still create init, because __init calls run
                                then [tInitDecl "I__.Root" $ ABS.MethClassBody (error "compiler implementation") (ABS.LIdent (pos,"__init")) [] (ABS.Bloc [])]
                                else []
               ) 
              )
       -- the empty Binary instance (deriving binary through generic)
       : HS.InstDecl HS.noLoc [] (identB "Binary") [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName] []

       :
       -- create the typeclass-instances
       map (\ (ABS.UIdent (_,interf), imdecls) -> 
                HS.InstDecl HS.noLoc [{- empty context for now, may need to fix later -}] 
                      (HS.UnQual $ HS.Ident $ interf ++ "_") -- interface name
                      [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName] -- the haskell instance / abs class name
                      (map (tMethDecl interf) imdecls)
           )
                 (M.toList scanInterfs)
       where
         mRun = find (\case 
                      ABS.MethClassBody _ (ABS.LIdent (_, "run")) _ _ -> True
                      _  -> False
                     ) mdecls

         -- all methods declared (interface methods and non-methods)
         mdecls = case maybeBlock of
                    ABS.NoBlock ->  ldecls
                    ABS.JustBlock _ _ -> rdecls

         nonMethods = filter (\case ABS.MethClassBody _ _ _ _ -> True
                                    _ -> False) $ mdecls \\ (concat (M.elems scanInterfs))

         -- treat it as a simple method 
         tNonMethDecl interfName (ABS.MethClassBody tReturn (ABS.LIdent (mpos,mident)) mparams (ABS.Bloc block)) = 
             if mident == "run" && (tReturn /= ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((-1,-1), "Unit")]) || not (null mparams))
             then errorPos mpos "run should have zero parameters and return type Unit"
             else  -- the underline non-method implementation
                 HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) ((map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams) ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName methsList)  (HS.BDecls [])]

         tNonMethDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"

         allFields :: ScopeTable -- order matters, because the fields are indexed
         allFields = M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params ++ mapMaybe (\case
                                                                       ABS.FieldClassBody t i -> Just (i,t)
                                                                       ABS.FieldAssignClassBody t i _ -> Just (i,t)
                                                                       ABS.MethClassBody _ _ _ _ -> Nothing
                                                                      ) ldecls

         -- treat it as a special instance method, since it disallows await and synchronous calls
         tInitDecl interfName (ABS.MethClassBody _ (ABS.LIdent (_,mident)) mparams (ABS.Bloc block)) = 
             let HS.Do tinit = tInitBlockWithReturn block clsName allFields (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName methsList
             in HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"]) Nothing (HS.UnGuardedRhs $ HS.Do $ if isJust mRun then tinit++ [HS.Qualifier (HS.App (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "this") (HS.QVarOp (HS.UnQual (HS.Symbol "^!!")))  (HS.Var $ HS.UnQual $ HS.Ident "this"))) (HS.Var $ HS.UnQual $ HS.Ident "run"))] else tinit)  (HS.BDecls (map (tNonMethDecl interfName) nonMethods))]

         tInitDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"


         tMethDecl interfName (ABS.MethClassBody tReturn (ABS.LIdent (mpos,mident)) mparams (ABS.Bloc block)) = 
             if mident == "run" && (tReturn /= ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent ((-1,-1), "Unit")]) || not (null mparams))
             then errorPos mpos "run should have zero parameters and return type Unit"
             else  -- the underline non-method implementation
                 HS.InsDecl $ 
                      HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar (HS.Ident "this")] )
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName methsList)  (HS.BDecls (map (tNonMethDecl interfName) nonMethods) --turned off non-meth decls
                                                                                                                                                                                                      )] -- 
         tMethDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"
         -- TODO, can be optimized
         methsList :: [ABS.LIdent]
         methsList = map (\ (ABS.MethClassBody _ mname _ _ )-> mname) $ concat $ M.elems scanInterfs
         scanInterfs :: M.Map ABS.UIdent [ABS.ClassBody] -- assoc list of interfaces to methods
         scanInterfs = M.map (\ mnames -> filter (\case
                                                 ABS.MethClassBody _ mname _ _ -> mname `elem` mnames
                                                 _ -> False
                                                ) mdecls)
                              $ M.filterWithKey (\ interfName _ -> interfName `elem` scanInterfs') (M.unions $ map methods ?moduleTable) -- filtered methods symboltable
             where
               scanInterfs' = scan imps
               unionedST = (M.unions $ map hierarchy ?moduleTable)
               scan :: [ABS.QType] -> [ABS.UIdent] -- gathers all interfaces that must be implemented
               scan imps = M.foldlWithKey (\ acc k extends ->  if ABS.QTyp [ABS.QTypeSegmen k] `elem` imps
                                                              then if null extends
                                                                   then k:acc
                                                                   else k:(scan extends ++ acc)
                                                              else acc)
                           [] unionedST

-- Utils


generateSubSelf :: String -> HS.Decl
generateSubSelf iname = HS.InstDecl HS.noLoc [] 
                            (identI "Sub")
                            [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident iname] -- instance Sub Interf1 Interf1
                            [   -- the upcasting method
                                -- is id in this case
                                HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [HS.PVar $ HS.Ident "x"] Nothing 
                                                           (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "x") (HS.BDecls [])]
                            ]

generateSubs :: (?moduleTable :: ModuleTable) =>String -> [ABS.QType] -> [HS.Decl]
generateSubs iname extends = map (generateSub iname) (nub $ collectSubs extends)

generateSub iname (ABS.QTyp sup) =  HS.InstDecl HS.noLoc [] 
                              (identI "Sub")
                              [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sub Interf2 Interf1
                              [   -- the upcasting method
                                  -- is unwrapping and wrapping the data constructors
                                  HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [HS.PApp (HS.UnQual $ HS.Ident iname) [HS.PVar $ HS.Ident "a"]] Nothing 
                                                                             (HS.UnGuardedRhs $ HS.App (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "a")) (HS.BDecls [])]
                                        ]


generateUnwrappedSub iname =  HS.InstDecl HS.noLoc [HS.ClassA (HS.UnQual $ HS.Ident (iname ++ "_")) [HS.TyVar $ HS.Ident "a"]] 
                              (identI "Sub")
                              [HS.TyApp (HS.TyCon $ identI "Obj") (HS.TyVar $ HS.Ident "a"), 
                                 HS.TyCon $ HS.UnQual $ HS.Ident $ iname] -- instance (Interf1_ a) => Sub (Obj a) Interf1
                              [   -- the upcasting method
                                  -- is wrapping with the constructor
                                  HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [] Nothing 
                                                                             (HS.UnGuardedRhs $ (HS.Con $ HS.UnQual $ HS.Ident iname)
                                                                                    ) (HS.BDecls [])]]



collectSubs :: (?moduleTable :: ModuleTable) => [ABS.QType] -> [ABS.QType]
collectSubs extends = extends ++ concat (mapMaybe (\ (ABS.QTyp eqids) -> liftM collectSubs (M.lookup (case last eqids of
                                                                                                            ABS.QTypeSegmen eid ->  eid
                                                                                                         )
                                                                                                interfMap
                                                                                               )) extends)
        where
          interfMap = M.unions (map hierarchy ?moduleTable)

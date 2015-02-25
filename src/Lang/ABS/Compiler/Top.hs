module Lang.ABS.Compiler.Top
    (tProg
    ) where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Conf
import Lang.ABS.Compiler.Utils
import Lang.ABS.Compiler.Stmt (tBlockWithReturn, tInitBlockWithReturn)
import Lang.ABS.Compiler.Expr (tPureExp,tBody, tType, tTypeOrTyVar)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.SrcLoc as HS (noLoc)
import qualified Data.Map as M
import Data.List ((\\), mapAccumL, nub, find)
import Data.Maybe (mapMaybe, isNothing)
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (runReader)

-- | Takes the name of the ABS source file and its parsed AST
-- Returns  a list of haskell ASTs, because 1 ABS program AST may correspond to more than 1 haskell module files/asts
tProg :: (?moduleTable::ModuleTable) => ABS.Program -> [HS.Module]
tProg (ABS.Prog moduls) = map tModul moduls

tModul :: (?moduleTable::ModuleTable) => ABS.Module -> HS.Module
tModul (ABS.Modul mname@(ABS.QTyp qsegs) exports imports decls maybeMain) = let strModuleName = joinQualTypeIds qsegs in
               HS.Module HS.noLoc (HS.ModuleName strModuleName) 
                     [HS.LanguagePragma HS.noLoc [HS.Ident "Rank2Types"
                                              ,HS.Ident "NoImplicitPrelude" -- for not importing haskell's prelude
                                              ,HS.Ident "FlexibleInstances" -- for subtype null to any interface
                                              ,HS.Ident "ExistentialQuantification" -- for heterogenous collections
                                              ,HS.Ident "MultiParamTypeClasses" -- for subtyping
                                              ,HS.Ident "ScopedTypeVariables" -- for inlining type annotations
                                              ,HS.Ident "DeriveDataTypeable" -- for defining ABS exceptions (exceptions are dynamically typed in haskell)
                                              ]
                     , HS.OptionsPragma HS.noLoc (Just HS.GHC) "-w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts"
                     ] 
                     Nothing 
                     (Just $ (case maybeMain of
                                ABS.JustBlock _ -> ((HS.EVar $ HS.UnQual $ HS.Ident "main") :)
                                ABS.NoBlock -> id) $ concatMap (tExport mname) exports)
                     -- IMPORT HEADER for the generated haskell module
                     ([HS.ImportDecl {HS.importLoc = HS.noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Runtime", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
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
                                     HS.importModule = HS.ModuleName "Lang.ABS.StdLib", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    } 
                     ]
                      ++        -- the translated ABS imports
                      map tImport imports
                     ) (tDecls decls ++ tMain maybeMain)


tExport :: (?moduleTable::ModuleTable) => ABS.QType -> ABS.Export -> [HS.ExportSpec]
tExport (ABS.QTyp qsegs) ABS.StarExport = [HS.EModuleContents (HS.ModuleName $ joinQualTypeIds qsegs)]
tExport _ (ABS.StarFromExport (ABS.QTyp qsegs)) = [HS.EModuleContents (HS.ModuleName $ joinQualTypeIds qsegs)]
tExport m (ABS.AnyExport es) = concatMap tExport' es
    where tExport' (ABS.AnyIden (ABS.LIdent (_,var))) = [HS.EVar (HS.UnQual $ HS.Ident var)]
          tExport' (ABS.AnyTyIden ident@(ABS.UIdent (_,var))) = case find (\ mi -> moduleName mi == m) ?moduleTable of
                                                                Just (ModuleInfo _ _ _ _ exs) -> if ident `elem` exs
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
tImport' m (ABS.AnyTyIden ident@(ABS.UIdent (_,var))) = case find (\ (ModuleInfo _ (ABS.QTyp qsegs) _ _ _ ) -> joinQualTypeIds qsegs == m) ?moduleTable of
                                                     Just (ModuleInfo _ _ _ _ exs) -> if ident `elem` exs
                                                                                       then [HS.IThingAll $ HS.Ident var, -- MyException
                                                                                             -- __myException smart constructor
                                                                                             HS.IVar (HS.Ident $  "__" ++ headToLower var)]
                                                                                       else [HS.IThingAll $ HS.Ident var] -- compromise since ABS cannot distinguish type constructors to data constructors
                                                     Nothing -> [HS.IThingAll $ HS.Ident var]

-- | Creates the mainABS wrapper i.e. main = main_is mainABS
-- only if the module has a main block and is declared as main source file in the conf
tMain :: (?moduleTable::ModuleTable) => ABS.MaybeBlock -> [HS.Decl]
tMain ABS.NoBlock = []
tMain (ABS.JustBlock (ABS.Bloc block)) = 
       -- main can only return with: return Unit;
       HS.PatBind HS.noLoc (HS.PVar (HS.Ident "mainABS")) Nothing (HS.UnGuardedRhs $ tBlockWithReturn block
                                                                      ("Top") -- class-name
                                                                      M.empty -- class-scope -- (error "No context for this")
                                                                      M.empty -- empty method params
                                                                      [] -- [scopetable]
                                                                      (error "no class context") -- interface-name
                                                               ) (HS.BDecls [])
                                      :
                                      [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "main")) Nothing 
                                             (HS.UnGuardedRhs (HS.App (HS.Var (HS.UnQual (HS.Ident "main_is")))
                                                                      (HS.Var (HS.UnQual (HS.Ident "mainABS"))))) (HS.BDecls [])]


tDecls :: (?moduleTable::ModuleTable) => [ABS.Decl] -> [HS.Decl]
tDecls = concatMap tDecl

-- can return more than 1 decl, because of creating accessors for records
-- or putting type signatures
tDecl :: (?moduleTable::ModuleTable) => ABS.Decl -> [HS.Decl]

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
           (if hasEx then [] else [(identI "Eq", [])])
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

    -- empty interface extends automatically from Object
tDecl (ABS.InterfDecl tid@(ABS.UIdent (p,_)) ms) = tDecl (ABS.ExtendsDecl tid [ABS.QTyp $ [ABS.QTypeSegmen $ ABS.UIdent (p,"Object_")]]  ms) 

tDecl (ABS.ExtendsDecl (ABS.UIdent (p,tname)) extends ms) = HS.ClassDecl 
                                                              HS.noLoc 
                                                              (map (\ (ABS.QTyp e) -> HS.ClassA (HS.UnQual $ HS.Ident $ joinQualTypeIds e ++ "_") [HS.TyVar (HS.Ident "a")]) extends)
                                                              (HS.Ident $ tname ++ "_") 
                                                              [HS.UnkindedVar (HS.Ident "a")]
                                                              [] -- no fundeps
                                                              (map (tMethSig tname) ms)
       : 
        -- type synonym for Objects typed by the interface
        -- data Interf1 = forall a. Interf1_ a => Interf1 (ObjectRef a)

        HS.DataDecl HS.noLoc HS.DataType [] (HS.Ident tname) [] [HS.QualConDecl HS.noLoc [HS.UnkindedVar $ HS.Ident "a"] [HS.ClassA (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyVar (HS.Ident "a")]] (HS.ConDecl (HS.Ident tname) [HS.UnBangedTy (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a"))])] []
                                                              
       -- Sub instances generation
       : generateSubSelf tname
       -- for lifting null to I, essentially null is a subtype of I
       : generateSubNull tname
       : generateSub tname (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent (p,"AnyObject")]) -- root class
       -- null class is an instance of any interface
       : HS.InstDecl HS.noLoc [] (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyCon $ HS.UnQual $ HS.Ident "Null"] 
             (map (\ (ABS.MethSig _ (ABS.LIdent (_,mid)) _) -> HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Ident mid) [] Nothing 
                                                                               (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "this should not happen. report the program to the compiler developers"))) (HS.BDecls [])]) ms)
       -- generate the equality smart function
       -- __eqI :: I -> I -> Bool
       : HS.FunBind [
       --__eqI (I NullRef) (I NullRef) = True
       HS.Match HS.noLoc (HS.Ident $ "__eq" ++ tname) (replicate 2 (HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "NullRef") []]))
         Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "True") (HS.BDecls []),
       --__eqI (I (ObjectRef _ id1 tid1)) (I (ObjectRef _ id2 tid2)) = id1 == id2 && tid1 == tid2
       HS.Match HS.noLoc (HS.Ident $ "__eq" ++ tname) 
             [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id1", HS.PVar $ HS.Ident "tid1"]],
              HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id2", HS.PVar $ HS.Ident "tid2"]]]
             Nothing (HS.UnGuardedRhs $ HS.InfixApp
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "id1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "id2")))
                        (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "tid1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "tid2"))))
                     (HS.BDecls []),
       -- __eqI _ _ = False
       HS.Match HS.noLoc (HS.Ident $ "__eq" ++ tname) [HS.PWildCard, HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "False") (HS.BDecls [])
             ]

       -- instance Eq I where (==) = __eqI   -- this is needed for ADTs deriving Eq
       : HS.InstDecl HS.noLoc [] (identI "Eq") [HS.TyCon $ HS.UnQual $ HS.Ident tname]
         [HS.InsDecl $ HS.FunBind [HS.Match HS.noLoc (HS.Symbol "==") [] Nothing (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ tname) (HS.BDecls [])]]
       

       : generateSubs tname (filter (\ (ABS.QTyp qids) -> qids /= [ABS.QTypeSegmen $ ABS.UIdent (p,"Object_")])  extends) 



       ++ (concatMap (\ (ABS.MethSig _ (ABS.LIdent (_,mid)) pars) -> let 
                          parspvars = map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar $ HS.Ident pid) pars
                          parsvars = map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.Var $ HS.UnQual $ HS.Ident pid) pars
                     in
                                                                 [
                -- the sync call for each method: method1_sync
                HS.FunBind [HS.Match HS.noLoc (HS.Ident $ mid ++ "_sync" ) (parspvars ++ [HS.PParen (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))]))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator HS.noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var $ identI "readRef") (HS.Var (HS.UnQual (HS.Ident "__ioref")))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "otherCOG")) (HS.App (HS.Var (HS.UnQual (HS.Ident "__cog"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Qualifier (HS.App (HS.App (HS.Var $ identI "when") (HS.Paren (HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__hereCOG"))) (HS.QVarOp (HS.UnQual (HS.Symbol "=="))) (HS.Var (HS.UnQual (HS.Ident "otherCOG"))))))) (HS.Paren (HS.App (HS.Var $ identI "error") (HS.Lit (HS.String "Sync Call on a different COG detected"))))),HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var (identI "withReaderT")) (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))])) (HS.BDecls [])
                           , HS.Match HS.noLoc (HS.Ident $ mid ++ "_sync") (replicate (length pars) HS.PWildCard ++ [HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PApp (HS.UnQual (HS.Ident "NullRef")) []])]) Nothing (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "sync call to null"))) (HS.BDecls [])
                           ]
                -- the async call for each method: method1_async
              , HS.FunBind [HS.Match HS.noLoc (HS.Ident $ headToLower mid ++ "_async") (parspvars ++ [HS.PParen (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))]))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator HS.noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var $ identI "readRef") (HS.Var (HS.UnQual (HS.Ident "__ioref")))),HS.Generator HS.noLoc (HS.PTuple HS.Boxed [(HS.PVar (HS.Ident "__chan")), HS.PWildCard]) (HS.App (HS.Var (HS.UnQual (HS.Ident "__cog"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "liftIO") (HS.Var (identI "newEmptyMVar"))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))), HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__hereCOG")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "liftIO") (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Var (HS.UnQual (HS.Ident "__f"))))])) (HS.BDecls [])
                           , HS.Match HS.noLoc (HS.Ident $ mid ++ "_async") (replicate (length pars) HS.PWildCard ++ [HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PApp (HS.UnQual (HS.Ident "NullRef")) []])]) Nothing (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "async call to null"))) (HS.BDecls [])
                           ]
                      ]
       ) ms)
    where
    tMethSig :: String -> ABS.MethSignat -> HS.ClassDecl
    tMethSig ityp (ABS.MethSig tReturn (ABS.LIdent (_,mname)) pars)  = HS.ClsDecl $
       HS.TypeSig HS.noLoc [HS.Ident mname] (foldr  -- function application is right-associative
                                     (\ tpar acc -> HS.TyFun (tType tpar) acc)
                                     -- the this objectref passed as input to the method
                                     (HS.TyFun 
                                      (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a"))
                                      (HS.TyApp ((HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ABS") 
                                                              (HS.TyVar $ HS.Ident "a")) )
                                                (tType tReturn))
                                     )
                                     (map (\ (ABS.Par typ _) -> typ) pars))


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
              [HS.QualConDecl HS.noLoc [] [] $ HS.RecDecl (HS.Ident clsName) (([HS.Ident $ headToLower clsName ++ "_loc"],
                                                                            -- maybe it should be banged for the fields of the class
                                                                           HS.UnBangedTy (HS.TyForall Nothing [HS.ClassA (HS.UnQual (HS.Ident "Object__")) [HS.TyVar (HS.Ident "o")]] (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident "o")))  (HS.TyCon (HS.UnQual (HS.Ident "COG")))))
                                                                           ): map (\ ((ABS.LIdent (_,i)), t) -> ([HS.Ident $ headToLower clsName ++ "_" ++ i], HS.UnBangedTy (tType t)))  (M.toAscList allFields))]  []
        :

        -- the smart constructor
        HS.FunBind [HS.Match HS.noLoc (HS.Ident $ "__" ++ headToLower clsName)
                    (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) params) Nothing 
                    (HS.UnGuardedRhs $ HS.RecConstr (HS.UnQual $ HS.Ident clsName) 
                           (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> 
                                     HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ pid) (HS.Var $ HS.UnQual $ HS.Ident pid)) params)
                    )
                    (HS.BDecls [])
                   ]
       :
        -- the Object instance
        HS.InstDecl HS.noLoc [{- empty context for now, may need to fix later -}] 
              (HS.UnQual $ HS.Ident "Object__") -- interface name
              [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName]
              (
               -- the new method
               HS.InsDecl (HS.FunBind [HS.Match HS.noLoc (HS.Ident "new") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                        -- chan <- lift $ lift $ newChan
                                        (HS.Generator HS.noLoc (HS.PVar $ HS.Ident "__chan") $ HS.App (HS.Var $ identI "liftIO") (HS.Var $ identI "newChan"))
                                        :
                                        -- __new_tid <- lift $ lift $ spawnCOG __chan
                                        (HS.Generator HS.noLoc (HS.PVar $ HS.Ident "__new_tid") $ HS.App (HS.Var $ identI "liftIO")
                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "spawnCOG") (HS.Var $ HS.UnQual $ HS.Ident "__chan")))
                                        :
                                        -- let __field = initialized_value
                                        fieldInits
                                        ++
                                        -- update the passed class ADT
                                        -- let __c = cont { class1_field1 = __field1, ..., class1_loc = (return (__chan, __new_tid)) }
                                        [HS.LetStmt $ HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\  fdecl acc -> (case fdecl of
                                                                                              ABS.FieldAssignClassBody _t (ABS.LIdent (_,fid)) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident fid) : acc
                                                                                              ABS.FieldClassBody _t (ABS.LIdent (_,fid)) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident fid) : acc
                                                                                              ABS.MethClassBody _ _ _ _ ->  (case maybeBlock of
                                                                                                                         ABS.NoBlock -> acc
                                                                                                                         ABS.JustBlock _ ->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                                                                             )) 
                                                                       -- class1_loc = (return (__chan, __new_tid))
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                                             (HS.Tuple HS.Boxed [(HS.Var $ HS.UnQual $ HS.Ident "__chan"), HS.Var $ HS.UnQual $ HS.Ident "__new_tid"]))]
                                                                       ldecls)) (HS.BDecls [])]
                                         -- __ioref <- lift $ lift $ newIORef __c
                                        , HS.Generator HS.noLoc (HS.PVar $ HS.Ident "__ioref") $ 
                                                           (HS.App (HS.Var $ identI "liftIO")
                                                                  (HS.App (HS.Var $ identI "newIORef")
                                                                         (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                        -- let __obj = ObjectRef __ioref 0 __new_tid
                                        , HS.LetStmt $ HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App (HS.App 
                                                                                                  (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                   (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                            (HS.Lit $ HS.Int 0))
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "__new_tid"))
                                                                  ) (HS.BDecls [])]
                                        -- init_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator HS.noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "liftIO") (HS.Var (identI "newEmptyMVar"))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))),HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__hereCOG")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "liftIO") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))))))])

                                        -- run_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator HS.noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "liftIO") (HS.Var (identI "newEmptyMVar"))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))),HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__hereCOG")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "liftIO") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))))))])

                                        -- return $ __obj
                                        , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Var $ HS.UnQual $ HS.Ident "__obj")

                                        ])) (HS.BDecls [])])

               :
               -- the new local method
               HS.InsDecl (HS.FunBind [HS.Match HS.noLoc (HS.Ident "new_local") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                         fieldInits
                                         ++

                                         [
                                         -- __thisCOG@(_, __tid) <- thisCOG
                                         HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "__thisCOG") $ HS.PTuple HS.Boxed [HS.PWildCard, HS.PVar $ HS.Ident "__tid"]) 
                                               (HS.Var $ HS.UnQual $ HS.Ident "thisCOG"),
                                         -- let __c = cont { class1_field1 = __field1, ..., class1_loc = __thisCOG }
                                         HS.LetStmt $ HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\ fdecl acc -> (case fdecl of
                                                                                              ABS.FieldAssignClassBody _t (ABS.LIdent (_,fid)) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident fid) : acc
                                                                                              ABS.FieldClassBody _t (ABS.LIdent (_,fid)) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident fid) : acc

                                                                                              ABS.MethClassBody _ _ _ _ -> (case maybeBlock of
                                                                                                                         ABS.NoBlock -> acc
                                                                                                                         ABS.JustBlock _ ->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                                                                             )) 
                                                                       -- class1_loc = __thisCOG)
                                                                       [HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                                                          (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Var $ HS.UnQual $ HS.Ident "__thisCOG"))
                                                                             ]
                                                                       ldecls)) (HS.BDecls [])]                                          
                                         -- __ioref <- lift $ lift $ newIORef __c
                                        , HS.Generator HS.noLoc (HS.PVar $ HS.Ident "__ioref") $ (HS.App (HS.Var $ identI "liftIO")
                                                                                                                      (HS.App (HS.Var $ identI "newIORef")
                                                                                                                             (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                         --       __astate@(AState {aCounter = __counter})  <- lift $ RWS.get
                                         , HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "__astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get")))

                                         -- lift $ RWS.put (__astate {aCounter = __counter + 1})
                                         , HS.Qualifier $ HS.App (HS.Var $ identI "lift") $ (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "__astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))])))
                                         -- let __obj = ObjectRef __ioref __counter __tid
                                         , HS.LetStmt $ HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App (HS.App 
                                                                                          (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                 (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                           (HS.Var $ HS.UnQual $ HS.Ident "__counter"))
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "__tid"))
                                                                                   ) (HS.BDecls [])]

                                         -- __init_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var (identI "withReaderT")) (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))

                                         -- __run_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var $ identI "withReaderT") (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))
                                        
                                         -- return $ __obj
                                         , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                            (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                         ])) (HS.BDecls [])])
                                        
               :
               -- the __cog method
               HS.InsDecl (HS.FunBind [HS.Match HS.noLoc (HS.Ident "__cog") [] Nothing
                                             (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                             (HS.BDecls [])])
               :
               -- the init method (optional)
               -- normalize to a method decl with name __init
               (case maybeBlock of
                  ABS.JustBlock b -> [tInitDecl "AnyObject" $ ABS.MethClassBody (error "compiler implementation") (ABS.LIdent (pos,"__init")) [] b]
                  ABS.NoBlock -> []
               ) 
               ++
               if isNothing $ find (\case ABS.MethClassBody _ (ABS.LIdent (_, "run")) _ _ -> True
                                          _ -> False) (concat (M.elems scanInterfs))
               then             -- that means there is no interface-declared run method
                   (case find (\case 
                               (ABS.MethClassBody _ (ABS.LIdent (_,"run")) [] _) -> True
                               _ -> False) mdecls of
                      Just (ABS.MethClassBody (ABS.TSimple (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Unit"))])) _ [] b) -> 
                          -- then it's the special RUN method
                          [tMethDecl "AnyObject" $ ABS.MethClassBody (error "compiler implementation") (ABS.LIdent (pos, "__run")) [] b]
                      _ -> [])
               else []
              )
       :

       (concatMap (\ ((ABS.LIdent (_,i), t), fieldNumber) ->
                 [
                  -- adds an explicit type signature for setters
                  HS.TypeSig HS.noLoc [HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i]
                                      (HS.TyFun (tType t)
                                         (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual $ HS.Ident "ABS"))
                                                        (HS.TyCon $ HS.UnQual $ HS.Ident clsName))
                                          (HS.TyCon $ HS.Special $ HS.UnitCon)))

                  ,
                  HS.FunBind [HS.Match HS.noLoc (HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i) [HS.PVar $ HS.Ident "v" ] Nothing
                                   (HS.UnGuardedRhs $ HS.Do
                                    [
                                     -- (AConf (ObjectRef ioref oid _) (thisChan, _) _) <- lift $ RWS.ask
                                     HS.Generator HS.noLoc (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AConf")) [HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "ioref"),HS.PVar (HS.Ident "oid"), HS.PWildCard]), HS.PTuple HS.Boxed [HS.PVar (HS.Ident "thisChan"), HS.PWildCard]])) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask")))
                                     -- astate@(AState _ om) <- lift $ RWS.get
                                    ,HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AState")) [HS.PWildCard,HS.PVar (HS.Ident "om"), HS.PWildCard]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get")))
                                     -- lift $ lift $ modifyIORef' ioref (\ c -> c {class1_p1 = v})      -- update the field value
                                    ,HS.Qualifier (HS.App (HS.Var $ identI "liftIO") (HS.App (HS.App (HS.Var (identI "modifyIORef'")) (HS.Var (HS.UnQual (HS.Ident "ioref")))) (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "c")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "c"))) [HS.FieldUpdate (HS.UnQual (HS.Ident $ headToLower clsName ++ "_" ++ i )) (HS.Var (HS.UnQual (HS.Ident "v")))])))))
                                     -- let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (oid, 0) om
                                     ,HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PTuple HS.Boxed [HS.PVar (HS.Ident "maybeWoken"),HS.PVar (HS.Ident "om'")]) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Var (identI "updateLookupWithKey")) (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "k"),HS.PVar (HS.Ident "v")] (HS.Con (HS.UnQual (HS.Ident "Nothing")))))) (HS.Tuple HS.Boxed [HS.Var (HS.UnQual (HS.Ident "oid")),HS.Lit (HS.Int fieldNumber)])) (HS.Var (HS.UnQual (HS.Ident "om"))))) (HS.BDecls [])])

                                     -- maybe (return ()) (\ woken -> lift $ lift $ writeList2Chan thisChan woken) maybeWoken
                                     ,HS.Qualifier (HS.App (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "maybe"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Con (HS.Special HS.UnitCon))))) (HS.Paren (HS.Lambda HS.noLoc [HS.PVar (HS.Ident "woken")] (HS.App (HS.Var $ identI "liftIO") (HS.App (HS.App (HS.Var (identI "writeList2Chan")) (HS.Var (HS.UnQual (HS.Ident "thisChan")))) (HS.Var (HS.UnQual (HS.Ident "woken")))))))) (HS.Var (HS.UnQual (HS.Ident "maybeWoken"))))

                                     -- lift $ RWS.put $ astate {aSleepingO = om'}
                                     ,HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.App (HS.Var (identI "put")) (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aSleepingO")) (HS.Var (HS.UnQual (HS.Ident "om'")))])))
                                    ]
                                   )
                                   (HS.BDecls [])]])
            ) (zip (M.assocs allFields) [0..])

       ++

       generateClsSub clsName (ABS.QTyp [ABS.QTypeSegmen $ ABS.UIdent (pos,"AnyObject")]) -- root class
       : generateClsSubs clsName imps

       ++
       -- create the typeclass-instances
       map (\ (ABS.UIdent (_,interf), imdecls) -> 
                HS.InstDecl HS.noLoc [{- empty context for now, may need to fix later -}] 
                      (HS.UnQual $ HS.Ident $ interf ++ "_") -- interface name
                      [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName] -- the haskell instance / abs class name
                      (map (tMethDecl interf) imdecls)
           )
                 (M.toList scanInterfs)
       where
         -- all methods declared (interface methods and non-methods)
         mdecls = case maybeBlock of
                    ABS.NoBlock ->  ldecls
                    ABS.JustBlock _ -> rdecls

         nonMethods = (filter (\case
                               ABS.MethClassBody _ _ _ _ -> True
                               _ -> False) mdecls) \\ concat (M.elems scanInterfs)

         -- treat it as a simple method 
         tNonMethDecl interfName (ABS.MethClassBody _ (ABS.LIdent (_,mident)) mparams (ABS.Bloc block)) = 
             -- the underline non-method implementation
             HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams) -- does not take this as param
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName)  (HS.BDecls [])]
             -- the sync wrapper
           : HS.FunBind [HS.Match HS.noLoc (HS.Ident $ mident ++ "_sync") (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams 
                                                                                ++ [HS.PParen (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AnyObject")) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))]))])
                                    Nothing (HS.UnGuardedRhs (foldl (\ acc (ABS.Par _ (ABS.LIdent (_,pident))) -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident pident)) (HS.Var $ HS.UnQual $ HS.Ident mident) mparams))  (HS.BDecls [])]
             -- the async wrapper
           : [HS.FunBind [HS.Match HS.noLoc (HS.Ident $ mident ++ "_async") (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams 
                                                                                ++ [HS.PParen (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AnyObject")) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))]))])
                                    Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator HS.noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PAsPat (HS.Ident "__cog") (HS.PTuple HS.Boxed [HS.PVar (HS.Ident "__chan"),HS.PWildCard])),HS.PFieldPat (HS.UnQual (HS.Ident "aThis")) (HS.PVar (HS.Ident "__obj"))]) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask"))),HS.Generator HS.noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.Generator HS.noLoc (HS.PVar (HS.Ident "__mvar"))  (HS.App (HS.Var $ identI "liftIO") (HS.Var (identI "newEmptyMVar"))),HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "liftIO") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (foldl (\ acc (ABS.Par _ (ABS.LIdent (_, pident))) -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident pident)) (HS.Var $ HS.UnQual $ HS.Ident mident) mparams))))))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Var (HS.UnQual (HS.Ident "__f"))))])) (HS.BDecls [])]]

         tNonMethDecl _ _ = error "non method declaration error"



         allFields :: ScopeTable -- order matters, because the fields are indexed
         allFields = M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params ++ mapMaybe (\case
                                                                       ABS.FieldClassBody t i -> Just (i,t)
                                                                       ABS.FieldAssignClassBody t i _ -> Just (i,t)
                                                                       ABS.MethClassBody _ _ _ _ -> Nothing
                                                                      ) ldecls
         fieldInits :: [HS.Stmt]
         fieldInits = foldr (\ fdecl acc -> (case fdecl of
                                                ABS.FieldAssignClassBody _t (ABS.LIdent (_,fid)) pexp -> 
                                                    HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident fid) Nothing 
                                                                                   (HS.UnGuardedRhs $ runReader (tPureExp pexp []) (allFields,"AnyObject")) (HS.BDecls [])])
                                                    : acc
                                                ABS.FieldClassBody t (ABS.LIdent (p,fid)) ->  
                                                    if isInterface t 
                                                    then HS.LetStmt (HS.BDecls [HS.PatBind HS.noLoc (HS.PVar $ HS.Ident fid) Nothing
                                                                                 (HS.UnGuardedRhs $ runReader (tPureExp (ABS.ELit ABS.LNull) []) (allFields,"AnyObject")) (HS.BDecls [])]) : acc
                                                    else case t of
                                                           -- it is an unitialized future (abs allows this)
                                                           ABS.TGen (ABS.QTyp [ABS.QTypeSegmen (ABS.UIdent (_,"Fut"))])  _ -> 
                                                               HS.Generator HS.noLoc (HS.PVar $ HS.Ident fid) (HS.Var $ identI "empty_fut")
                                                                                       : acc
                                                           _ -> errorPos p "A field must be initialised if it is not of a reference type"
                                                ABS.MethClassBody _ _ _ _ -> (case maybeBlock of
                                                                               ABS.NoBlock -> acc
                                                                               ABS.JustBlock _->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                               )) [] ldecls

         -- treat it as a special instance method, since it disallows await and synchronous calls
         tInitDecl interfName (ABS.MethClassBody _ (ABS.LIdent (_,mident)) mparams (ABS.Bloc block)) = HS.InsDecl $ 
                      HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tInitBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName)  (HS.BDecls [] -- (concatMap (tNonMethDecl interfName) nonMethods) --turned off non-meth decls
                                                                                                                                                                                                      )] -- 
         tInitDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"


         tMethDecl interfName (ABS.MethClassBody _ (ABS.LIdent (_,mident)) mparams (ABS.Bloc block)) = HS.InsDecl $ 
                      HS.FunBind [HS.Match HS.noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.LIdent (_,pid))) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             (foldl (\ acc (ABS.Par ptyp pident@(ABS.LIdent (p,_))) -> 
                                                       M.insertWith (const $ const $ errorPos p $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams) [] interfName)  (HS.BDecls [] -- (concatMap (tNonMethDecl interfName) nonMethods) --turned off non-meth decls
                                                                                                                                                                                                      )] -- 
         tMethDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"
         -- TODO, can be optimized
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
                            (HS.UnQual $ HS.Ident "Sub")
                            [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident iname] -- instance Sup Interf1 Interf1
                            [   -- the upcasting method
                                -- is id in this case
                                HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [HS.PVar $ HS.Ident "x"] Nothing 
                                                           (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "x") (HS.BDecls [])]
                            ]

generateSubNull :: String -> HS.Decl
generateSubNull iname = HS.InstDecl HS.noLoc [] 
                            (HS.UnQual $ HS.Ident "Sub")
                            -- instance Sub (ObjectRef Null) Interf1
                            [HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyCon $ HS.UnQual $ HS.Ident "Null"), HS.TyCon $ HS.UnQual $ HS.Ident iname]
                            [   -- the upcasting method
                                HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [] Nothing 
                                                           -- up = Interf1
                                                           (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident iname) (HS.BDecls [])]
                            ]
                                                      

generateSubs :: (?moduleTable :: ModuleTable) =>String -> [ABS.QType] -> [HS.Decl]
generateSubs iname extends = map (generateSub iname) (nub $ collectSubs extends)

generateSub iname (ABS.QTyp sup) =  HS.InstDecl HS.noLoc [] 
                              (HS.UnQual $ HS.Ident "Sub")
                              [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sup Interf1 Interf1
                              [   -- the upcasting method
                                  -- is id in this case
                                  HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [HS.PApp (HS.UnQual $ HS.Ident iname) [HS.PVar $ HS.Ident "a"]] Nothing 
                                                                             (HS.UnGuardedRhs $ HS.App (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "a")) (HS.BDecls [])]
                                        ]


generateClsSubs :: (?moduleTable :: ModuleTable) => String -> [ABS.QType] -> [HS.Decl]
generateClsSubs clsName impls = map (generateClsSub clsName) (nub $ collectSubs impls)

generateClsSub clsName (ABS.QTyp sup) =  HS.InstDecl HS.noLoc [] 
                              (HS.UnQual $ HS.Ident "Sub")
                              [HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyCon $ HS.UnQual $ HS.Ident clsName), 
                                 HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sup (ObjectRef Cls) Interf1
                              [   -- the upcasting method
                                  -- is id in this case
                                  HS.InsDecl $ HS.FunBind $ [HS.Match HS.noLoc (HS.Ident "up") [] Nothing 
                                                                             (HS.UnGuardedRhs $ (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    ) (HS.BDecls [])]]



collectSubs :: (?moduleTable :: ModuleTable) => [ABS.QType] -> [ABS.QType]
collectSubs extends = extends ++ concat (mapMaybe (\ (ABS.QTyp eqids) -> liftM collectSubs (M.lookup (case last eqids of
                                                                                                            ABS.QTypeSegmen eid ->  eid
                                                                                                         )
                                                                                                interfMap
                                                                                               )) extends)
        where
          interfMap = M.unions (map hierarchy ?moduleTable)

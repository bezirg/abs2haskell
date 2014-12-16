{-# LANGUAGE LambdaCase #-}

module Main where

import Lang.ABS.Compiler.Conf
import Lang.ABS.Compiler.Utils (parseABSFiles, ppTranslatedHaskell)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import Language.Haskell.Exts.SrcLoc (noLoc)
import Control.Monad (liftM)
import Data.List (intersperse, nub, findIndices, (\\))
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

-- Represents the SymbolTable, which is a list of all the modules info
data ModuleST = ModuleST {
      filePath :: FilePath,
      moduleName :: ABS.QualType,
      hierarchy :: M.Map ABS.TypeIdent [ABS.QualType], -- Interface -> Extends
      methods :: M.Map ABS.TypeIdent [ABS.Ident],      -- Interface -> Methods
      exceptions :: [ABS.TypeIdent]                    -- names of exceptions, needed for code generating smart-exception-constructors
    } deriving (Show)


main :: IO ()
main = do
  let Conf {files = inputFilesOrDirs} = conf
  asts <- liftM concat $ mapM parseABSFiles inputFilesOrDirs
  let 
    symbolTable :: [ModuleST]
    symbolTable = map collectInterfs asts

    collectInterfs :: (FilePath, ABS.Program) -> ModuleST
    collectInterfs (fp , (ABS.Prog [ABS.Modul mName _ _ decls _])) = ModuleST {
                                                                        filePath = fp,
                                                                        moduleName = mName,
                                                                        hierarchy = foldl insertInterfs M.empty decls,
                                                                        methods = foldl insertMethods M.empty decls,
                                                                        exceptions = foldl (\ acc decl -> 
                                                                                             case decl of
                                                                                               ABS.ExceptionDecl cident -> (case cident of
                                                                                                                            ABS.SinglConstrIdent tid -> tid
                                                                                                                            ABS.ParamConstrIdent tid _ -> tid) : acc
                                                                                               _ -> acc) [] decls
                                                                     }
                where 
                  insertInterfs :: M.Map ABS.TypeIdent [ABS.QualType] -> ABS.Decl -> M.Map ABS.TypeIdent [ABS.QualType]
                  insertInterfs acc (ABS.InterfDecl tident _msigs) = M.insertWith (const $ const $ error "duplicate interface declaration") tident [] acc
                  insertInterfs acc (ABS.ExtendsDecl tident extends _msigs) = M.insertWith (const $ const $ error "duplicate interface declaration") tident extends acc
                  insertInterfs acc _ = acc

                  insertMethods :: M.Map ABS.TypeIdent [ABS.Ident] -> ABS.Decl -> M.Map ABS.TypeIdent [ABS.Ident]
                  insertMethods acc (ABS.InterfDecl tident msigs) = insertMethods acc (ABS.ExtendsDecl tident [] msigs)  -- normalization
                  insertMethods acc (ABS.ExtendsDecl tident extends msigs) = 
                      {- TODO it could generate a compilation error because of duplicate method declaration -}
                      M.insertWith (const $ const $ error "duplicate interface declaration") tident (collectMethods msigs) acc
                  insertMethods acc _ = acc
                  collectMethods :: [ABS.MethSignat] -> [ABS.Ident]
                  collectMethods = map (\ (ABS.MethSig _ ident _) -> ident)

    tProgram :: (FilePath, ABS.Program) -> (FilePath, HS.Module)
    tProgram (absFilePath, (ABS.Prog [ABS.Modul moduleName exports imports decls maybeMain])) = (absFilePath,
               HS.Module noLoc (if (absFilePath == main_is conf)
                                then HS.ModuleName "Main"
                                else tModuleName moduleName) 
                     [HS.LanguagePragma noLoc [HS.Ident "Rank2Types"
                                              ,HS.Ident "NoImplicitPrelude"
                                               -- ,HS.Ident "ImpredicativeTypes"
                                               -- ,HS.Ident "LiberalTypeSynonyms"
                                              ,HS.Ident "FlexibleInstances" -- for subtype null to any interface
                                              ,HS.Ident "ExistentialQuantification" -- for heterogenous collections
                                              ,HS.Ident "MultiParamTypeClasses" -- for subtyping
                                              ,HS.Ident "ScopedTypeVariables" -- for inlining type annotations
                                              ,HS.Ident "DeriveDataTypeable" -- for defining ABS exceptions (exceptions are dynamically typed in haskell)
                                              ]
                     , HS.OptionsPragma noLoc (Just HS.GHC) "-w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts"
                     ] 
                     Nothing 
                     Nothing 
                     -- IMPORT HEADER for the generated haskell module
                     [
                      HS.ImportDecl {HS.importLoc = noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Runtime", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    },
                      HS.ImportDecl {HS.importLoc = noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.Compiler.Include", 
                                     HS.importSrc = False, 
                                     HS.importQualified = True,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Just (HS.ModuleName "I__"),
                                     HS.importSpecs = Nothing
                                    },
                      HS.ImportDecl {HS.importLoc = noLoc, 
                                     HS.importModule = HS.ModuleName "Lang.ABS.StdLib", 
                                     HS.importSrc = False, 
                                     HS.importQualified = False,
                                     HS.importPkg = Nothing,
                                     HS.importAs = Nothing,
                                     HS.importSpecs = Nothing
                                    }] 
                     (tDecls decls ++ (tMain maybeMain))
                                                                                                     )
    tDecls :: [ABS.Decl] -> [HS.Decl]
    tDecls = concatMap tDecl

    -- can return more than 1 decl, because of creating accessors for records
    -- or putting type signatures
    tDecl :: ABS.Decl -> [HS.Decl]

    tDecl (ABS.ExceptionDecl constr) = let (cid, cargs) = case constr of
                                                            ABS.SinglConstrIdent (ABS.TypeIdent tid) -> (tid, [])
                                                            ABS.ParamConstrIdent (ABS.TypeIdent tid) args -> (tid, args)
                                       in
                                         -- 1) a data MyException = MyException(args)
                                         [HS.DataDecl noLoc HS.DataType [] (HS.Ident cid) [] 
                                               -- one sole constructor with the same name as the exception name
                                               [HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid)
                                                                            (map (HS.UnBangedTy . tType . typOfConstrType) cargs))]
                                               -- three deriving for exception datatypes
                                               (map (\ der -> (identI der, [])) ["Eq","Show","Typeable"])
                                         -- 2) a instance Exception MyException
                                         ,HS.InstDecl noLoc [] (identI "Exception") [HS.TyCon $ HS.UnQual $ HS.Ident cid] []
                                         -- 3) a __myException smartconstructor
                                         ,HS.FunBind [HS.Match noLoc (HS.Ident $ "__" ++ headToLower cid)
                                                            (map (\ i -> HS.PVar $ HS.Ident $ "__" ++ show i )  [1..length cargs])
                                                            Nothing
                                                            (HS.UnGuardedRhs $ (HS.App (HS.Var $ identI "SomeException") 
                                                                                (HS.Paren
                                                                                 (foldl (\ acc i -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ show i))
                                                                                            (HS.Con $ HS.UnQual $ HS.Ident cid) [1..length cargs]))))
                                                            (HS.BDecls [])]
                                         ]
                                                    
    -- TODO pass the type variables env , change tType to tTypeOrTyVar
    tDecl (ABS.TypeDecl (ABS.TypeIdent tid) typ) = [HS.TypeDecl noLoc (HS.Ident tid) [{- TODO: type variables lhs -}] (tType typ)]

    tDecl (ABS.DataDecl tid constrs) =  tDecl (ABS.DataParDecl tid [] constrs) -- just parametric datatype with empty list of type variables

    tDecl (ABS.DataParDecl (ABS.TypeIdent tid) tyvars constrs) =
        -- create the data declaration
        HS.DataDecl noLoc HS.DataType [] (HS.Ident tid) (map (\ (ABS.TypeIdent varid) -> HS.UnkindedVar $ HS.Ident $ headToLower $  varid) tyvars)
           (map (\case
                 ABS.SinglConstrIdent (ABS.TypeIdent cid) -> HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid) []) -- no constructor arguments
                 ABS.ParamConstrIdent (ABS.TypeIdent cid) args -> HS.QualConDecl noLoc [] [] (HS.ConDecl (HS.Ident cid) (map (HS.UnBangedTy . tTypeOrTyVar tyvars . typOfConstrType) args))) constrs)
           [(identI "Eq", [])]
           :
        -- create record accessors
        map (\ (ABS.Ident fname, consname, idx, len) ->  HS.FunBind [HS.Match noLoc (HS.Ident fname) ([HS.PApp (HS.UnQual (HS.Ident consname)) (replicate idx HS.PWildCard ++ [HS.PVar (HS.Ident "a")] ++ replicate (len - idx - 1) HS.PWildCard)]) Nothing (HS.UnGuardedRhs (HS.Var (HS.UnQual (HS.Ident "a")))) (HS.BDecls [])]) (
             concatMap (\case
               ABS.SinglConstrIdent _ -> []
               ABS.ParamConstrIdent (ABS.TypeIdent cid) args -> -- taking the indices of fields
                                         let len = length args
                                         in
                                            foldl (\ acc (field, idx) ->  case field of
                                                                            ABS.EmptyConstrType _ -> acc
                                                                            ABS.RecordConstrType _ fid -> (fid, cid, idx, len):acc) [] (zip args [0..])
              ) constrs
                                                                                                                                                                                                                                                                                                                                  )

    -- empty interface extends automatically from Object
    tDecl (ABS.InterfDecl tid ms) = tDecl (ABS.ExtendsDecl tid [ABS.QType $ [ABS.QTypeSegment $ ABS.TypeIdent "Object_"]]  ms) 

    tDecl (ABS.ExtendsDecl (ABS.TypeIdent tname) extends ms) = HS.ClassDecl 
                                                              noLoc 
                                                              (map (\ (ABS.QType e) -> HS.ClassA (HS.UnQual $ HS.Ident $ joinQualTypeIds e ++ "_") [HS.TyVar (HS.Ident "a")]) extends)
                                                              (HS.Ident $ tname ++ "_") 
                                                              [HS.UnkindedVar (HS.Ident "a")]
                                                              [] -- no fundeps
                                                              (map (tMethodSig tname) ms)
       : 
        -- type synonym for Objects typed by the interface
        -- data Interf1 = forall a. Interf1_ a => Interf1 (ObjectRef a)

        HS.DataDecl noLoc HS.DataType [] (HS.Ident tname) [] [HS.QualConDecl noLoc [HS.UnkindedVar $ HS.Ident "a"] [HS.ClassA (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyVar (HS.Ident "a")]] (HS.ConDecl (HS.Ident tname) [HS.UnBangedTy (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a"))])] []
                                                              
       -- Sub instances generation
       : generateSubSelf tname
       -- for lifting null to I, essentially null is a subtype of I
       : generateSubNull tname
       : generateSub tname (ABS.QType [ABS.QTypeSegment $ ABS.TypeIdent "AnyObject"]) -- root class
       -- null class is an instance of any interface
       : HS.InstDecl noLoc [] (HS.UnQual $ HS.Ident $ tname ++ "_") [HS.TyCon $ HS.UnQual $ HS.Ident "Null"] 
             (map (\ (ABS.MethSig _ (ABS.Ident mid) _) -> HS.InsDecl $ HS.FunBind [HS.Match noLoc (HS.Ident mid) [] Nothing 
                                                                               (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "this should not happen. report the program to the compiler developers"))) (HS.BDecls [])]) ms)
       -- generate the equality smart function
       -- __eqI :: I -> I -> Bool
       : HS.FunBind [
       --__eqI (I NullRef) (I NullRef) = True
       HS.Match noLoc (HS.Ident $ "__eq" ++ tname) (replicate 2 (HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "NullRef") []]))
         Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "True") (HS.BDecls []),
       --__eqI (I (ObjectRef _ id1 tid1)) (I (ObjectRef _ id2 tid2)) = id1 == id2 && tid1 == tid2
       HS.Match noLoc (HS.Ident $ "__eq" ++ tname) 
             [HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id1", HS.PVar $ HS.Ident "tid1"]],
              HS.PApp (HS.UnQual $ HS.Ident tname) [HS.PApp (HS.UnQual $ HS.Ident "ObjectRef") [HS.PWildCard, HS.PVar $ HS.Ident "id2", HS.PVar $ HS.Ident "tid2"]]]
             Nothing (HS.UnGuardedRhs $ HS.InfixApp
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "id1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "id2")))
                        (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")
                        (HS.Paren (HS.InfixApp (HS.Var $ HS.UnQual $ HS.Ident "tid1") (HS.QVarOp $ HS.UnQual $ HS.Symbol "==") (HS.Var $ HS.UnQual $ HS.Ident "tid2"))))
                     (HS.BDecls []),
       -- __eqI _ _ = False
       HS.Match noLoc (HS.Ident $ "__eq" ++ tname) [HS.PWildCard, HS.PWildCard] Nothing (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident "False") (HS.BDecls [])
             ]

       -- instance Eq I where (==) = __eqI   -- this is needed for ADTs deriving Eq
       : HS.InstDecl noLoc [] (identI "Eq") [HS.TyCon $ HS.UnQual $ HS.Ident tname]
         [HS.InsDecl $ HS.FunBind [HS.Match noLoc (HS.Symbol "==") [] Nothing (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ tname) (HS.BDecls [])]]
       

       : generateSubs tname (filter (\ (ABS.QType qids) -> qids /= [ABS.QTypeSegment $ ABS.TypeIdent "Object_"])  extends) 



       ++ (concatMap (\ (ABS.MethSig _ (ABS.Ident mid) pars) -> let 
                          parspvars = map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar $ HS.Ident pid) pars
                          parsvars = map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.Var $ HS.UnQual $ HS.Ident pid) pars
                     in
                                                                 [
                -- the sync call for each method: method1_sync
                HS.FunBind [HS.Match noLoc (HS.Ident $ mid ++ "_sync" ) (parspvars ++ [HS.PParen (HS.PAsPat (HS.Ident "__wrapped") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))])))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__hereCOG")) (HS.Var (HS.UnQual (HS.Ident "thisCOG"))),HS.Generator noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "readIORef") (HS.Var (HS.UnQual (HS.Ident "__ioref")))))))),HS.Generator noLoc (HS.PVar (HS.Ident "otherCOG")) (HS.App (HS.Var (HS.UnQual (HS.Ident "__cog"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Qualifier (HS.App (HS.App (HS.Var $ identI "when") (HS.Paren (HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__hereCOG"))) (HS.QVarOp (HS.UnQual (HS.Symbol "=="))) (HS.Var (HS.UnQual (HS.Ident "otherCOG"))))))) (HS.Paren (HS.App (HS.Var $ identI "error") (HS.Lit (HS.String "Sync Call on a different COG detected"))))),HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var (identI "withReaderT")) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__obj"))))))])) (HS.BDecls [])
                           , HS.Match noLoc (HS.Ident $ mid ++ "_sync") (replicate (length pars) HS.PWildCard ++ [HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PApp (HS.UnQual (HS.Ident "NullRef")) []])]) Nothing (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "sync call to null"))) (HS.BDecls [])
                           ]
                -- the async call for each method: method1_async
              , HS.FunBind [HS.Match noLoc (HS.Ident $ headToLower mid ++ "_async") (parspvars ++ [HS.PParen (HS.PAsPat (HS.Ident "__wrapped") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PAsPat (HS.Ident "__obj") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "__ioref"),HS.PWildCard, HS.PWildCard]))])))]) Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__obj1")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "readIORef") (HS.Var (HS.UnQual (HS.Ident "__ioref")))))))),HS.Generator noLoc (HS.PTuple HS.Boxed [(HS.PVar (HS.Ident "__chan")), HS.PWildCard]) (HS.App (HS.Var (HS.UnQual (HS.Ident "__cog"))) (HS.Var (HS.UnQual (HS.Ident "__obj1")))),HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Var (identI "newEmptyMVar"))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "__cog"))]) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask"))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (foldl HS.App (HS.Var (HS.UnQual (HS.Ident mid))) parsvars) (HS.Var (HS.UnQual (HS.Ident "__obj")))))))))))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Var (HS.UnQual (HS.Ident "__f"))))])) (HS.BDecls [])
                           , HS.Match noLoc (HS.Ident $ mid ++ "_async") (replicate (length pars) HS.PWildCard ++ [HS.PParen (HS.PApp (HS.UnQual (HS.Ident tname)) [HS.PApp (HS.UnQual (HS.Ident "NullRef")) []])]) Nothing (HS.UnGuardedRhs (HS.App (HS.Var $ identI "error") (HS.Lit $ HS.String "async call to null"))) (HS.BDecls [])
                           ]
                      ]
       ) ms)


    -- normalize
    tDecl (ABS.FunDecl fReturnTyp fid params body) = tDecl (ABS.FunParDecl fReturnTyp fid [] params body) -- no type variables

    tDecl (ABS.FunParDecl fReturnTyp (ABS.Ident fid) tyvars params body) = 
       [
        HS.FunBind [HS.Match noLoc (HS.Ident fid) (map (\ (ABS.Par ptyp (ABS.Ident pid)) -> 
                                                            (\ pat -> if ptyp == ABS.TUnderscore
                                                                     then pat -- infer the parameter type
                                                                     else HS.PatTypeSig noLoc pat (tTypeOrTyVar tyvars ptyp) -- wrap with an explicit type annotation
                                                            ) (HS.PVar (HS.Ident pid))) params)
                          Nothing (HS.UnGuardedRhs $  -- we don't support guards in ABS language
                                         (\ exp -> if fReturnTyp == ABS.TUnderscore 
                                                  then exp -- infer the return type
                                                  else HS.ExpTypeSig noLoc exp (tTypeOrTyVar tyvars fReturnTyp)) -- wrap the return exp with an explicit type annotation
                                         (tBody body tyvars params)
                                   )  (HS.BDecls [])]
       ]

    -- normalizing class declarations
    tDecl (ABS.ClassDecl tident fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamDecl tident params fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident params [] fdecls maybeBlock mdecls)
    tDecl (ABS.ClassImplements tident imps fdecls maybeBlock mdecls) = tDecl (ABS.ClassParamImplements tident [] imps fdecls maybeBlock mdecls)
    tDecl (ABS.ClassParamImplements (ABS.TypeIdent clsName) params imps ldecls maybeBlock rdecls) = -- TODO add check for imps, if a method is not implemented
       -- the record-ADT of the ABS class
        HS.DataDecl noLoc HS.DataType [] (HS.Ident clsName) [] 
              [HS.QualConDecl noLoc [] [] $ HS.RecDecl (HS.Ident clsName) (([HS.Ident $ headToLower clsName ++ "_loc"],
                                                                            -- maybe it should be banged for the fields of the class
                                                                           HS.UnBangedTy (HS.TyForall Nothing [HS.ClassA (HS.UnQual (HS.Ident "Object__")) [HS.TyVar (HS.Ident "o")]] (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual (HS.Ident "ABS"))) (HS.TyVar (HS.Ident "o")))  (HS.TyCon (HS.UnQual (HS.Ident "COG")))))
                                                                           ): map (\ ((ABS.Ident i), t) -> ([HS.Ident $ headToLower clsName ++ "_" ++ i], HS.UnBangedTy (tType t)))  (M.toAscList allFields))]  []
        :

        -- the smart constructor
        HS.FunBind [HS.Match noLoc (HS.Ident $ "__" ++ headToLower clsName)
                    (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) params) Nothing 
                    (HS.UnGuardedRhs $ HS.RecConstr (HS.UnQual $ HS.Ident clsName) 
                           (map (\ (ABS.Par _ (ABS.Ident pid)) -> 
                                     HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ pid) (HS.Var $ HS.UnQual $ HS.Ident pid)) params)
                    )
                    (HS.BDecls [])
                   ]
       :
        -- the Object instance
        HS.InstDecl noLoc [{- empty context for now, may need to fix later -}] 
              (HS.UnQual $ HS.Ident "Object__") -- interface name
              [HS.TyCon $ HS.UnQual $ HS.Ident $ clsName]
              (
               -- the new method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "new") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                        -- chan <- lift $ lift $ newChan
                                        (HS.Generator noLoc (HS.PVar $ HS.Ident "__chan") $ HS.App (HS.Var $ identI "lift") $ HS.App (HS.Var $ identI "lift") (HS.Var $ identI "newChan"))
                                        :
                                        -- __new_tid <- lift $ lift $ spawnCOG __chan
                                        (HS.Generator noLoc (HS.PVar $ HS.Ident "__new_tid") $ HS.App (HS.Var $ identI "lift") $ HS.App (HS.Var $ identI "lift")
                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "spawnCOG") (HS.Var $ HS.UnQual $ HS.Ident "__chan")))
                                        :
                                        -- let __field = initialized_value
                                        fieldInits
                                        ++
                                        -- update the passed class ADT
                                        -- let __c = cont { class1_field1 = __field1, ..., class1_loc = (return (__chan, __new_tid)) }
                                        [HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\  fdecl acc -> (case fdecl of
                                                                                              ABS.FieldAssignClassBody _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldClassBody _t (ABS.Ident fid) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
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
                                        , HS.Generator noLoc (HS.PVar $ HS.Ident "__ioref") $ HS.App (HS.Var $ identI "lift") $
                                                           (HS.App (HS.Var $ identI "lift")
                                                                  (HS.App (HS.Var $ identI "newIORef")
                                                                         (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                        -- let __obj = ObjectRef __ioref 0 __new_tid
                                        , HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App (HS.App 
                                                                                                  (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                   (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                            (HS.Lit $ HS.Int 0))
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "__new_tid"))
                                                                  ) (HS.BDecls [])]
                                        -- init_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Var (identI "newEmptyMVar"))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "__cog"))]) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask"))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))))))))])

                                        -- run_async __obj
                                        , HS.Qualifier (HS.Do [HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Var (identI "newEmptyMVar"))))),HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PVar (HS.Ident "__cog"))]) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask"))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))))))))])

                                        -- return $ __obj
                                        , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Var $ HS.UnQual $ HS.Ident "__obj")

                                        ])) (HS.BDecls [])])

               :
               -- the new local method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "new_local") [HS.PVar $ HS.Ident "__cont"] Nothing
                                       (HS.UnGuardedRhs $ HS.Do (
                                         fieldInits
                                         ++

                                         [
                                         -- __thisCOG@(_, __tid) <- thisCOG
                                         HS.Generator noLoc (HS.PAsPat (HS.Ident "__thisCOG") $ HS.PTuple HS.Boxed [HS.PWildCard, HS.PVar $ HS.Ident "__tid"]) 
                                               (HS.Var $ HS.UnQual $ HS.Ident "thisCOG"),
                                         -- let __c = cont { class1_field1 = __field1, ..., class1_loc = __thisCOG }
                                         HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__c") Nothing 
                                                                   (HS.UnGuardedRhs $ HS.RecUpdate (HS.Var $ HS.UnQual $ HS.Ident "__cont")
                                                                      (foldr (\ fdecl acc -> (case fdecl of
                                                                                              ABS.FieldAssignClassBody _t (ABS.Ident fid) _pexp -> 
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc
                                                                                              ABS.FieldClassBody _t (ABS.Ident fid) ->  
                                                                                                  HS.FieldUpdate (HS.UnQual $ HS.Ident $ headToLower clsName ++ "_" ++ fid) (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ fid) : acc

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
                                        , HS.Generator noLoc (HS.PVar $ HS.Ident "__ioref") $ HS.App (HS.Var $ identI "lift") $ (HS.App (HS.Var $ identI "lift")
                                                                                                                      (HS.App (HS.Var $ identI "newIORef")
                                                                                                                             (HS.Var $ HS.UnQual $ HS.Ident "__c")))
                                         --       __astate@(AState {aCounter = __counter})  <- lift $ RWS.get
                                         , HS.Generator noLoc (HS.PAsPat (HS.Ident "__astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get")))

                                         -- lift $ RWS.put (__astate {aCounter = __counter + 1})
                                         , HS.Qualifier $ HS.App (HS.Var $ identI "lift") $ (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "__astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))])))
                                         -- let __obj = ObjectRef __ioref __counter __tid
                                         , HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident "__obj") Nothing 
                                                                  (HS.UnGuardedRhs (HS.App (HS.App 
                                                                                          (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ObjectRef")
                                                                                                 (HS.Var $ HS.UnQual $ HS.Ident "__ioref"))
                                                                                           (HS.Var $ HS.UnQual $ HS.Ident "__counter"))
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "__tid"))
                                                                                   ) (HS.BDecls [])]

                                         -- __init_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var (identI "withReaderT")) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__init"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))

                                         -- __run_sync __obj
                                         , HS.Qualifier (HS.App (HS.App (HS.Var $ identI "mapMonad") (HS.Paren (HS.App (HS.Var $ identI "withReaderT") (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "aconf")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "aconf"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aThis")) (HS.Var (HS.UnQual (HS.Ident "__obj")))])))))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "__run"))) (HS.App (HS.Con $ HS.UnQual $ HS.Ident "AnyObject") (HS.Var (HS.UnQual (HS.Ident "__obj")))))))
                                        
                                         -- return $ __obj
                                         , HS.Qualifier $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                                                            (HS.Var $ HS.UnQual $ HS.Ident "__obj")
                                         ])) (HS.BDecls [])])
                                        
               :
               -- the __cog method
               HS.InsDecl (HS.FunBind [HS.Match noLoc (HS.Ident "__cog") [] Nothing
                                             (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident $ headToLower clsName ++ "_loc")
                                             (HS.BDecls [])])
               :
               -- the init method (optional)
               -- normalize to a method decl with name __init
               (case maybeBlock of
                  ABS.JustBlock b -> [tMethDecl "AnyObject" $ ABS.MethClassBody (error "compiler implementation") (ABS.Ident "__init") [] b]
                  ABS.NoBlock -> []
               ) 
               -- the run method does not need a special case, since it is generated as a normal method
              )

       :

       (concatMap (\ ((ABS.Ident i, t), fieldNumber) ->
                 [
                  -- adds an explicit type signature for setters
                  HS.TypeSig noLoc [HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i]
                                      (HS.TyFun (tType t)
                                         (HS.TyApp (HS.TyApp (HS.TyCon (HS.UnQual $ HS.Ident "ABS"))
                                                        (HS.TyCon $ HS.UnQual $ HS.Ident clsName))
                                          (HS.TyCon $ HS.Special $ HS.UnitCon)))

                  ,
                  HS.FunBind [HS.Match noLoc (HS.Ident $ "set_" ++ headToLower clsName ++ "_" ++ i) [HS.PVar $ HS.Ident "v" ] Nothing
                                   (HS.UnGuardedRhs $ HS.Do
                                    [
                                     -- (AConf this@(ObjectRef ioref _) (thisChan, _) _) <- lift $ RWS.ask
                                     HS.Generator noLoc (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AConf")) [HS.PAsPat (HS.Ident "this") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "ObjectRef")) [HS.PVar (HS.Ident "ioref"),HS.PWildCard, HS.PWildCard])), HS.PTuple HS.Boxed [HS.PVar (HS.Ident "thisChan"), HS.PWildCard]])) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask")))
                                     -- astate@(AState _ om) <- lift $ RWS.get
                                    ,HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PApp (HS.UnQual (HS.Ident "AState")) [HS.PWildCard,HS.PVar (HS.Ident "om")]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get")))
                                     -- lift $ lift $ modifyIORef' ioref (\ c -> c {class1_p1 = v})      -- update the field value
                                    ,HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.App (HS.Var $ identI "lift") (HS.App (HS.App (HS.Var (identI "modifyIORef'")) (HS.Var (HS.UnQual (HS.Ident "ioref")))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "c")] (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "c"))) [HS.FieldUpdate (HS.UnQual (HS.Ident $ headToLower clsName ++ "_" ++ i )) (HS.Var (HS.UnQual (HS.Ident "v")))]))))))
                                     -- let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (Object this, 0) om
                                     ,HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PTuple HS.Boxed [HS.PVar (HS.Ident "maybeWoken"),HS.PVar (HS.Ident "om'")]) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Var (identI "updateLookupWithKey")) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "k"),HS.PVar (HS.Ident "v")] (HS.Con (HS.UnQual (HS.Ident "Nothing")))))) (HS.Tuple HS.Boxed [HS.App (HS.Con (HS.UnQual (HS.Ident "AnyObject"))) (HS.Var (HS.UnQual (HS.Ident "this"))),HS.Lit (HS.Int fieldNumber)])) (HS.Var (HS.UnQual (HS.Ident "om"))))) (HS.BDecls [])])

                                     -- maybe (return ()) (\ woken -> lift $ lift $ writeList2Chan thisChan woken) maybeWoken
                                     ,HS.Qualifier (HS.App (HS.App (HS.App (HS.Var (HS.UnQual (HS.Ident "maybe"))) (HS.Paren (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Con (HS.Special HS.UnitCon))))) (HS.Paren (HS.Lambda noLoc [HS.PVar (HS.Ident "woken")] (HS.App (HS.Var $ identI "lift") (HS.App (HS.Var $ identI "lift") (HS.App (HS.App (HS.Var (identI "writeList2Chan")) (HS.Var (HS.UnQual (HS.Ident "thisChan")))) (HS.Var (HS.UnQual (HS.Ident "woken"))))))))) (HS.Var (HS.UnQual (HS.Ident "maybeWoken"))))

                                     -- lift $ RWS.put $ astate {aSleepingO = om'}
                                     ,HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.App (HS.Var (identI "put")) (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aSleepingO")) (HS.Var (HS.UnQual (HS.Ident "om'")))])))
                                    ]
                                   )
                                   (HS.BDecls [])]])
            ) (zip (M.assocs allFields) [0..])

       ++

       generateClsSub clsName (ABS.QType [ABS.QTypeSegment $ ABS.TypeIdent "AnyObject"]) -- root class
       : generateClsSubs clsName imps

       ++
       -- create the typeclass-instances
       map (\ (ABS.TypeIdent interf, imdecls) -> 
                HS.InstDecl noLoc [{- empty context for now, may need to fix later -}] 
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
         tNonMethDecl interfName (ABS.MethClassBody _ (ABS.Ident mident) mparams (ABS.Bloc block)) = 
             -- the underline non-method implementation
             HS.FunBind [HS.Match noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams) -- does not take this as param
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             [foldl (\ acc (ABS.Par ptyp pident) -> 
                                                       M.insertWith (const $ const $ error $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams] interfName)  (HS.BDecls [])]
             -- the sync wrapper
           : HS.FunBind [HS.Match noLoc (HS.Ident $ mident ++ "_sync") (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams 
                                                                                ++ [HS.PatTypeSig noLoc HS.PWildCard $ HS.TyCon $ HS.UnQual $ HS.Ident "AnyObject"])
                                    Nothing (HS.UnGuardedRhs (foldl (\ acc (ABS.Par _ (ABS.Ident pident)) -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident pident)) (HS.Var $ HS.UnQual $ HS.Ident mident) mparams))  (HS.BDecls [])]
             -- the async wrapper
           : [HS.FunBind [HS.Match noLoc (HS.Ident $ mident ++ "_async") (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams 
                                                                                ++ [HS.PatTypeSig noLoc HS.PWildCard $ HS.TyCon $ HS.UnQual $ HS.Ident "AnyObject"])
                                    Nothing (HS.UnGuardedRhs (HS.Do [HS.Generator noLoc (HS.PRec (HS.UnQual (HS.Ident "AConf")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCOG")) (HS.PAsPat (HS.Ident "__cog") (HS.PTuple HS.Boxed [HS.PVar (HS.Ident "__chan"),HS.PWildCard])),HS.PFieldPat (HS.UnQual (HS.Ident "aThis")) (HS.PVar (HS.Ident "__obj"))]) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "ask"))),HS.Generator noLoc (HS.PAsPat (HS.Ident "astate") (HS.PParen (HS.PRec (HS.UnQual (HS.Ident "AState")) [HS.PFieldPat (HS.UnQual (HS.Ident "aCounter")) (HS.PVar (HS.Ident "__counter"))]))) (HS.App (HS.Var $ identI "lift") (HS.Var (identI "get"))),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var (identI "put")) (HS.Paren (HS.RecUpdate (HS.Var (HS.UnQual (HS.Ident "astate"))) [HS.FieldUpdate (HS.UnQual (HS.Ident "aCounter")) (HS.InfixApp (HS.Var (HS.UnQual (HS.Ident "__counter"))) (HS.QVarOp (HS.UnQual (HS.Symbol "+"))) (HS.Lit (HS.Int 1)))]))))),HS.Generator noLoc (HS.PVar (HS.Ident "__mvar")) (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Var (identI "newEmptyMVar"))))),HS.LetStmt (HS.BDecls [HS.PatBind noLoc (HS.PVar (HS.Ident "__f")) Nothing (HS.UnGuardedRhs (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "FutureRef"))) (HS.Var (HS.UnQual (HS.Ident "__mvar")))) (HS.Var (HS.UnQual (HS.Ident "__cog")))) (HS.Var (HS.UnQual (HS.Ident "__counter"))))) (HS.BDecls [])]),HS.Qualifier (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.Var $ identI "lift") (HS.Paren (HS.App (HS.App (HS.Var (identI "writeChan")) (HS.Var (HS.UnQual (HS.Ident "__chan")))) (HS.Paren (HS.App (HS.App (HS.App (HS.Con (HS.UnQual (HS.Ident "RunJob"))) (HS.Var (HS.UnQual (HS.Ident "__obj")))) (HS.Var (HS.UnQual (HS.Ident "__f")))) (HS.Paren (foldl (\ acc (ABS.Par _ (ABS.Ident pident)) -> HS.App acc (HS.Var $ HS.UnQual $ HS.Ident pident)) (HS.Var $ HS.UnQual $ HS.Ident mident) mparams))))))))),HS.Qualifier (HS.App (HS.Var (HS.UnQual (HS.Ident "return"))) (HS.Var (HS.UnQual (HS.Ident "__f"))))])) (HS.BDecls [])]]

         tNonMethDecl _ _ = error "non method declaration error"



         allFields :: Scope -- order matters, because the fields are indexed
         allFields = M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params ++ mapMaybe (\case
                                                                       ABS.FieldClassBody t i -> Just (i,t)
                                                                       ABS.FieldAssignClassBody t i _ -> Just (i,t)
                                                                       ABS.MethClassBody _ _ _ _ -> Nothing
                                                                      ) ldecls
         fieldInits = foldr (\ fdecl acc -> (case fdecl of
                                                ABS.FieldAssignClassBody _t (ABS.Ident fid) pexp -> 
                                                    (HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident $ "__" ++ fid) Nothing 
                                                                                   (HS.UnGuardedRhs $ tPureExp pexp [] allFields M.empty "AnyObject") (HS.BDecls [])]) : acc
                                                ABS.FieldClassBody t (ABS.Ident fid) -> (if isInterface t symbolTable
                                                                                  then  (HS.LetStmt $ HS.BDecls [HS.PatBind noLoc (HS.PVar $ HS.Ident $ "__" ++ fid) Nothing
                                                                                                                       (HS.UnGuardedRhs $ tPureExp (ABS.ELit ABS.LNull) [] allFields M.empty "AnyObject") (HS.BDecls [])])
                                                                                  else error "A field must be initialised if it is not of a reference type"
                                                                                  )
                                                                                      : acc
                                                ABS.MethClassBody _ _ _ _ -> (case maybeBlock of
                                                                          ABS.NoBlock -> acc
                                                                          ABS.JustBlock _->  error "Second parsing error: Syntactic error, no method declaration accepted here")
                               )) [] ldecls

         tMethDecl interfName (ABS.MethClassBody _ (ABS.Ident mident) mparams (ABS.Bloc block)) = HS.InsDecl $ 
                      HS.FunBind [HS.Match noLoc (HS.Ident mident) (map (\ (ABS.Par _ (ABS.Ident pid)) -> HS.PVar (HS.Ident pid)) mparams ++ [HS.PVar $ HS.Ident "this"])
                                    Nothing (HS.UnGuardedRhs $ tBlockWithReturn block clsName allFields 
                                             -- method scoping of input arguments
                                             [foldl (\ acc (ABS.Par ptyp pident) -> 
                                                       M.insertWith (const $ const $ error $ "Parameter " ++ show pident ++ " is already defined") pident ptyp acc) M.empty  mparams] interfName)  (HS.BDecls (concatMap (tNonMethDecl interfName) nonMethods))]
         tMethDecl _ _ = error "Second parsing error: Syntactic error, no field declaration accepted here"
         -- TODO, can be optimized
         scanInterfs :: M.Map ABS.TypeIdent [ABS.ClassBody] -- assoc list of interfaces to methods
         scanInterfs = M.map (\ mnames -> filter (\case
                                                 ABS.MethClassBody _ mname _ _ -> mname `elem` mnames
                                                 _ -> False
                                                ) mdecls)
                              $ M.filterWithKey (\ interfName _ -> interfName `elem` scanInterfs') (M.unions $ map methods symbolTable) -- filtered methods symboltable
             where
               scanInterfs' = scan imps
               unionedST = (M.unions $ map hierarchy symbolTable)
               scan :: [ABS.QualType] -> [ABS.TypeIdent] -- gathers all interfaces that must be implemented
               scan imps = M.foldlWithKey (\ acc k extends ->  if ABS.QType [ABS.QTypeSegment k] `elem` imps
                                                              then if null extends
                                                                   then k:acc
                                                                   else k:(scan extends ++ acc)
                                                              else acc)
                           [] unionedST


    generateSubSelf :: String -> HS.Decl
    generateSubSelf iname = HS.InstDecl noLoc [] 
                            (HS.UnQual $ HS.Ident "Sub")
                            [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident iname] -- instance Sup Interf1 Interf1
                            [   -- the upcasting method
                                -- is id in this case
                                HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [HS.PVar $ HS.Ident "x"] Nothing 
                                                           (HS.UnGuardedRhs $ HS.Var $ HS.UnQual $ HS.Ident "x") (HS.BDecls [])]
                            ]

    generateSubNull :: String -> HS.Decl
    generateSubNull iname = HS.InstDecl noLoc [] 
                            (HS.UnQual $ HS.Ident "Sub")
                            -- instance Sub (ObjectRef Null) Interf1
                            [HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyCon $ HS.UnQual $ HS.Ident "Null"), HS.TyCon $ HS.UnQual $ HS.Ident iname]
                            [   -- the upcasting method
                                HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [] Nothing 
                                                           -- up = Interf1
                                                           (HS.UnGuardedRhs $ HS.Con $ HS.UnQual $ HS.Ident iname) (HS.BDecls [])]
                            ]
                                                      

    generateSubs :: String -> [ABS.QualType] -> [HS.Decl]
    generateSubs iname extends = map (generateSub iname) (nub $ collectSubs extends)

    generateSub iname (ABS.QType sup) =  HS.InstDecl noLoc [] 
                              (HS.UnQual $ HS.Ident "Sub")
                              [HS.TyCon $ HS.UnQual $ HS.Ident iname, HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sup Interf1 Interf1
                              [   -- the upcasting method
                                  -- is id in this case
                                  HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [HS.PApp (HS.UnQual $ HS.Ident iname) [HS.PVar $ HS.Ident "a"]] Nothing 
                                                                             (HS.UnGuardedRhs $ HS.App (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    (HS.Var $ HS.UnQual $ HS.Ident "a")) (HS.BDecls [])]
                                        ]


    generateClsSubs :: String -> [ABS.QualType] -> [HS.Decl]
    generateClsSubs clsName impls = map (generateClsSub clsName) (nub $ collectSubs impls)

    generateClsSub clsName (ABS.QType sup) =  HS.InstDecl noLoc [] 
                              (HS.UnQual $ HS.Ident "Sub")
                              [HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyCon $ HS.UnQual $ HS.Ident clsName), 
                                 HS.TyCon $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup] -- instance Sup (ObjectRef Cls) Interf1
                              [   -- the upcasting method
                                  -- is id in this case
                                  HS.InsDecl $ HS.FunBind $ [HS.Match noLoc (HS.Ident "up") [] Nothing 
                                                                             (HS.UnGuardedRhs $ (HS.Con $ HS.UnQual $ HS.Ident $ joinQualTypeIds sup)
                                                                                    ) (HS.BDecls [])]]



    collectSubs :: [ABS.QualType] -> [ABS.QualType]
    collectSubs extends = extends ++ concat (mapMaybe (\ (ABS.QType eqids) -> liftM collectSubs (M.lookup (case last eqids of
                                                                                                            ABS.QTypeSegment eid ->  eid
                                                                                                         )
                                                                                                interfMap
                                                                                               )) extends)
        where
          interfMap = M.unions (map hierarchy symbolTable)


    tBody :: ABS.FunBody -> [ABS.TypeIdent] -> [ABS.Param] -> HS.Exp
    tBody ABS.BuiltinFunBody _tyvars _params = HS.Var $ identI "undefined" -- builtin turned to undefined
    tBody (ABS.NormalFunBody exp) tyvars params = tPureExp exp tyvars M.empty (M.fromList $ map (\ (ABS.Par t i) -> (i,t)) params) (error "no class context") -- no class scope and no global scope

    -- tPureExp :: ABS.PureExp -> TypeVarsInScope -> CurrentClassScope -> CurrentNormalScope -> InterfaceName -> HS.Exp
    tPureExp :: ABS.PureExp -> [ABS.TypeIdent] -> Scope -> Scope -> String -> HS.Exp
    tPureExp (ABS.If predE thenE elseE) tyvars clsScope fscope interf = HS.If (tPureExp predE tyvars clsScope fscope interf) (tPureExp thenE tyvars clsScope fscope interf) (tPureExp elseE tyvars clsScope fscope interf)

    -- translate it into a lambda exp
    tPureExp (ABS.Let (ABS.Par ptyp pid@(ABS.Ident var)) eqE inE) tyvars clsScope fscope interf = 
                                              (HS.App -- apply the created lamdba to the equality expr
                                                  (HS.Lambda noLoc
                                                   [if ptyp == ABS.TUnderscore
                                                    then pat -- infer the parameter type
                                                    else HS.PatTypeSig noLoc pat (tTypeOrTyVar tyvars ptyp)] -- wrap with an explicit type annotation -- bound variable
                                                   (tPureExp inE tyvars clsScope (M.insert pid ptyp fscope) interf))
                                                  (tPureExp eqE tyvars clsScope fscope interf)
                                              )
        where
          pat = HS.PVar $ HS.Ident var

    tPureExp (ABS.Case matchE branches) tyvars clsScope fscope interf = 
      let scope = M.union fscope clsScope
          tCaseException matchE brs = (HS.App (HS.App (HS.Var (identI "caseEx")) (tPureExp matchE tyvars clsScope fscope interf))
                                    (HS.List (map 
                                              (\ (ABS.CaseBranc pat pexp) -> HS.App
                                                                             (HS.Con (identI "PHandler"))
                                                                             -- TODO: if hse support lambdacase, it will lead to cleaner code
                                                                             (HS.Lambda  noLoc (case pat of
                                                                                                 -- a catch-all is a wrapped someexception
                                                                                                 ABS.PUnderscore -> [HS.PApp 
                                                                                                                          (identI "SomeException")
                                                                                                                          [HS.PWildCard]]
                                                                                                 -- otherwise generate a pattern-match catch_all
                                                                                                 _ -> [HS.PVar $ HS.Ident "__0"]
                                                                                               )
                                                                              (case pat of
                                                                                 -- wrap the normal returned expression in a just
                                                                                 ABS.PUnderscore -> (HS.App (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.Paren $ tPureExp pexp tyvars clsScope fscope interf))
                                                                                 _ -> HS.Case (HS.Var $ HS.UnQual $ HS.Ident "__0")
                                                                                     [HS.Alt noLoc (tFunPat pat)
                                                                                      -- wrap the normal returned expression in a just
                                                                                      (HS.UnGuardedAlt (HS.App (HS.Con $ HS.UnQual $ HS.Ident "Just") (HS.Paren $ tPureExp pexp tyvars clsScope fscope interf))) (HS.BDecls []),
                                                                                      -- pattern match fail, return nothing
                                                                                      HS.Alt noLoc HS.PWildCard (HS.UnGuardedAlt $ HS.Con $ HS.UnQual $ HS.Ident "Nothing") (HS.BDecls [])]

                                                                             )))
                                              
                                                                           brs)))
      in
        case matchE of
          ABS.EVar ident | M.lookup ident scope == Just (ABS.TSimple (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Exception")])) -> tCaseException matchE branches
          ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment tid]) | tid `elem` concatMap exceptions symbolTable -> tCaseException matchE branches
          ABS.EParamConstr (ABS.QType [ABS.QTypeSegment tid]) _ | tid `elem` concatMap exceptions symbolTable -> tCaseException matchE branches
          _ -> HS.Case (tPureExp matchE tyvars clsScope fscope interf) (map 
                                                                 (\ (ABS.CaseBranc pat exp) -> HS.Alt noLoc (tFunPat pat) (HS.UnGuardedAlt (tPureExp exp tyvars clsScope fscope interf )) (HS.BDecls []))
                                                                 branches)

    tPureExp (ABS.EFunCall (ABS.Ident cid) args) tyvars clsScope fscope interf = foldl 
                                            (\ acc nextArg -> HS.App acc (tPureExp nextArg tyvars clsScope fscope interf))
                                            (HS.Var $ HS.UnQual $ HS.Ident cid)
                                            args

    tPureExp (ABS.ENaryFunCall (ABS.Ident cid) args) tyvars clsScope fscope interf = HS.App 
                                                (HS.Var $ HS.UnQual $ HS.Ident cid)
                                                (HS.List (map (\ arg -> tPureExp arg tyvars clsScope fscope interf) args))

    -- be careful to parenthesize infix apps
    tPureExp (ABS.EOr left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual $ HS.Symbol "||")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EAnd left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual $ HS.Symbol "&&")  (tPureExp right tyvars clsScope fscope interf)

    -- Equality handler
    tPureExp (ABS.EEq (ABS.ELit ABS.LNull) (ABS.ELit ABS.LNull)) _tyvars _clsScope _fscope _interf = HS.Con $ HS.UnQual $ HS.Ident "True"

    tPureExp (ABS.EEq (ABS.ELit ABS.LNull) right) tyvars clsScope fscope interf = tPureExp (ABS.EEq right (ABS.ELit ABS.LNull)) tyvars clsScope fscope interf -- normalize

    tPureExp (ABS.EEq left (ABS.ELit ABS.LNull)) tyvars clsScope fscope interf = check (tPureExp left tyvars clsScope fscope interf)
              where
                check exp = case exp of
                  HS.Paren exp' -> check exp'
                  -- it is a this object
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.UnQual (HS.Ident "this"))) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ interf) 
                                                                              hvar)
                                                                    (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null"))

                  -- it is a non-this object
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.Qual _m (HS.Ident v))) -> let vtyp@(ABS.TSimple (ABS.QType qtids)) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident v) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                                                            in if isInterface vtyp symbolTable
                                                               then (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) 
                                                                              hvar)
                                                                    (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null"))
                                                               else (error "incomparable types")
                  -- same as above
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.UnQual (HS.Ident v))) -> let vtyp@(ABS.TSimple (ABS.QType qtids)) = maybe (error "incomparable types") id $ M.lookup (ABS.Ident v) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                                                            in if isInterface vtyp symbolTable
                                                               then (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) 
                                                                              hvar)
                                                                    (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "null"))
                                                               else (error "incomparable types")
                  HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
                  HS.Con _ -> error "cannot compare ADT constructor with null"
                  HS.InfixApp _ _ _ -> error "cannot compare pure value with null"
                  HS.NegApp _ -> error "cannot compare pure value with null"
                  HS.Lit _ -> error "cannot compare pure value with null"
                  HS.Tuple _ _ -> error "cannot compare tuple with null"

    tPureExp (ABS.EEq (ABS.ELit (ABS.LThis)) (ABS.ELit (ABS.LThis))) _ _ _ interf = if length interf > 0 -- hack to ensure it is not in a main block
                                                                                    then HS.Con $ HS.UnQual $ HS.Ident "True"
                                                                                    else error "not this in main block"
    tPureExp (ABS.EEq left@(ABS.ELit (ABS.LThis)) right) tyvars clsScope fscope interf = tPureExp (ABS.EEq right left) tyvars clsScope fscope interf -- normalize
    
    tPureExp (ABS.EEq left (ABS.ELit (ABS.LThis))) tyvars clsScope fscope interf = check (tPureExp left tyvars clsScope fscope interf)
              where
                check exp = case exp of
                  HS.Paren exp' -> check exp'
                  -- it is a non-this object
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.Qual _m (HS.Ident v))) -> let vtyp@(ABS.TSimple qtyp) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident v) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                                                            in if isInterface vtyp symbolTable
                                                               then case joinSub qtyp (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent interf)]) symbolTable of
                                                                      Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) hvar) (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "this"))
                                                                      Nothing -> (error "incomparable types")
                                                               else (error "incomparable types")
                  -- same as above
                  HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) hvar@(HS.Var (HS.UnQual (HS.Ident v))) -> let vtyp@(ABS.TSimple qtyp) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident v) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                                                            in if isInterface vtyp symbolTable
                                                               then case joinSub qtyp (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent interf)]) symbolTable of
                                                                      Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) hvar) (HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") $ HS.Var $ HS.UnQual $ HS.Ident "this"))
                                                                      Nothing -> (error "incomparable types")
                                                               else (error "incomparable types")
                  HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
                  HS.Con _ -> error "cannot compare ADT constructor with null"
                  HS.InfixApp _ _ _ -> error "cannot compare pure value with null"
                  HS.NegApp _ -> error "cannot compare pure value with null"
                  HS.Lit _ -> error "cannot compare pure value with null"
                  HS.Tuple _ _ -> error "cannot compare tuple with null"

    tPureExp (ABS.EEq left right) tyvars clsScope fscope interf = let
           tLeft = (tPureExp left tyvars clsScope fscope interf)
           tRight = (tPureExp right tyvars clsScope fscope interf)
           check (HS.Paren lexp) rexp = check lexp rexp -- eliminate parentheses
           check lexp (HS.Paren rexp) = check lexp rexp -- eliminate parentheses
           check leftapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident leftVarName)))) rexp = 
               case rexp of 
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                     in case joinSub qtypLeft qtypRight symbolTable of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                     in case joinSub qtypLeft qtypRight symbolTable of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
           check leftapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident leftVarName)))) rexp = 
               case rexp of 
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.Qual _m (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                     in case joinSub qtypLeft qtypRight symbolTable of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 rightapp@(HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) (HS.Var (HS.UnQual (HS.Ident rightVarName)))) -> 
                     let (ABS.TSimple qtypLeft) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident leftVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                         (ABS.TSimple qtypRight) = maybe (error "incomparable types") id $  M.lookup (ABS.Ident rightVarName) (M.union fscope (M.mapKeys (\ (ABS.Ident field) -> ABS.Ident $ "__" ++ field) clsScope))
                     in case joinSub qtypLeft qtypRight symbolTable of
                          Just (ABS.QType qtids) -> (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident $ "__eq" ++ joinQualTypeIds qtids) leftapp) rightapp)
                          Nothing -> error "incomparable types"
                 HS.App _ _ -> error "equality coupled with function calls not implemented yet" -- TODO
           check (HS.App _ _) (HS.App (HS.Var (HS.UnQual (HS.Ident "up"))) _)   = error "equality coupled with function calls not implemented yet" -- TODO
           -- then it should be an equality between pure expressions
           check _ _ = HS.Paren $ HS.InfixApp tLeft (HS.QVarOp $ HS.UnQual  $ HS.Symbol "==") tRight
      in check tLeft tRight 
         
    -- normalize it to not . ==
    tPureExp (ABS.ENeq left right) tyvars clsScope fscope interf = HS.Paren $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (tPureExp (ABS.EEq left right) tyvars clsScope fscope interf)

    tPureExp (ABS.ELt left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.ELe left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "<=")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EGt left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EGe left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol ">=")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EAdd left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "+")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.ESub left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "-")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EMul left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "*")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EDiv left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "/")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.EMod left right) tyvars clsScope fscope interf = HS.Paren $ HS.InfixApp (tPureExp left tyvars clsScope fscope interf) (HS.QVarOp $ HS.UnQual  $ HS.Symbol "%")  (tPureExp right tyvars clsScope fscope interf)

    tPureExp (ABS.ELogNeg e) tyvars clsScope fscope interf = HS.Paren $ HS.App (HS.Var $ HS.UnQual $ HS.Ident "not") (tPureExp e tyvars clsScope fscope interf)

    tPureExp (ABS.EIntNeg e) tyvars clsScope fscope interf = HS.Paren $ HS.NegApp (tPureExp e tyvars clsScope fscope interf)

    tPureExp (ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Nil")])) _ _ _ _ = HS.Con $ HS.Special HS.ListCon -- for the translation to []

    tPureExp (ABS.ESinglConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "EmptyMap")])) _ _ _ _ = HS.Var $ HS.UnQual $ HS.Ident "empty" -- for the translation to Data.Map

    tPureExp (ABS.ESinglConstr (ABS.QType qids)) _ _ _ _ = let mids = init qids
                                                               tid@(ABS.TypeIdent sid) = (\ (ABS.QTypeSegment cid) -> cid) (last qids)
                                                           in if tid `elem` concatMap exceptions symbolTable
                                                              -- if is an exception constructor, replace it with its smart constructor
                                                              then HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ headToLower sid  
                                                              else
                                                                  HS.Con $ (if null mids 
                                                                            then HS.UnQual 
                                                                            else HS.Qual (HS.ModuleName $ joinQualTypeIds mids)
                                                                           ) $ HS.Ident sid

    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Triple")]) pexps) tyvars clsScope fscope interf | length pexps == 3 = HS.Tuple HS.Boxed (map (\ pexp -> tPureExp pexp tyvars clsScope fscope interf) pexps) -- for the translation to tuples
                                                                                                                               | otherwise = error "wrong number of arguments to Triple"
    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Pair")]) pexps) tyvars clsScope fscope interf | length pexps == 2  = HS.Tuple HS.Boxed (map (\ pexp -> tPureExp pexp tyvars clsScope fscope interf) pexps) -- for the translation to tuples
                                                                                                                             | otherwise = error "wrong number of arguments to Pair"
    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Cons")]) [pexp1, pexp2]) tyvars clsScope fscope interf =  -- for the translation to pexp1:pexp2
                                                                                                           HS.Paren (HS.InfixApp 
                                                                                                                         (tPureExp pexp1 tyvars clsScope fscope interf)
                                                                                                                         (HS.QConOp $ HS.Special $ HS.Cons)
                                                                                                                         (tPureExp pexp2 tyvars clsScope fscope interf))
    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "Cons")]) _) _ _ _ _ = error "wrong number of arguments to Cons"
    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "InsertAssoc")]) [pexp1, pexp2]) tyvars clsScope fscope interf = HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "insertAssoc") (tPureExp pexp1 tyvars clsScope fscope interf)) (tPureExp pexp2 tyvars clsScope fscope interf)

    tPureExp (ABS.EParamConstr (ABS.QType [ABS.QTypeSegment (ABS.TypeIdent "InsertAssoc")]) _) _ _ _ _ = error "wrong number of arguments to InsertAssoc"
    tPureExp (ABS.EParamConstr qids args) tyvars clsScope fscope interf = foldl
                                       (\ acc nextArg -> HS.App acc (tPureExp nextArg tyvars clsScope fscope interf))
                                       (tPureExp (ABS.ESinglConstr qids) tyvars clsScope fscope interf)
                                       args


    tPureExp (ABS.EVar var@(ABS.Ident pid)) _tyvars clsScope fscope interf = case M.lookup var fscope of
                                                       Nothing -> case M.lookup var clsScope of
                                                                   -- lookup in the clsScope
                                                                   -- if it of an int type, upcast it
                                                                   Just (ABS.TSimple (ABS.QType ([ABS.QTypeSegment (ABS.TypeIdent "Int")]))) ->HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid)
                                                                   Just t -> HS.Paren ((if isInterface t symbolTable
                                                                                         -- upcasting if it is of a class type
                                                                                         then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up")
                                                                                         else id)
                                                                                      (HS.Var $ HS.UnQual $ HS.Ident $ "__" ++ pid))
                                                                   -- TODO: this should be turned into warning
                                                                   --  pure expressions don't have a scope, because they rely in haskell for scoping
                                                                   Nothing -> HS.Var $ HS.UnQual $ HS.Ident pid 
                                                       -- if it of an int type, upcast it
                                                       Just (ABS.TSimple (ABS.QType ([ABS.QTypeSegment (ABS.TypeIdent "Int")]))) -> HS.App (HS.Var $ identI "fromIntegral") (HS.Var $ HS.UnQual $ HS.Ident pid)
                                                       Just t -> HS.Paren $ ((if isInterface t symbolTable
                                                                            then HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") -- upcasting if it is of a class type
                                                                            else id)
                                                                                     (HS.Var $ HS.UnQual $ HS.Ident pid))

    tPureExp (ABS.ELit lit) _ _ _ _ = case lit of
                                         (ABS.LStr str) ->  HS.Lit $ HS.String str
                                         (ABS.LInt i) ->  HS.Lit $ HS.Int i
                                         ABS.LThis -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "this")
                                         ABS.LNull -> HS.App (HS.Var $ HS.UnQual $ HS.Ident "up") (HS.Var $ HS.UnQual $ HS.Ident "null")


    -- this is a trick for sync_call and async_call TODO: error "Cannot compile object accesses in mathematically pure expressions"
    -- translate this.field
    tPureExp (ABS.EThis (ABS.Ident ident)) _ _ _ _ = HS.Var $ HS.UnQual $ HS.Ident ("__" ++ ident)

    isInterface :: ABS.Type -> [ModuleST] -> Bool 
    isInterface (ABS.TSimple (ABS.QType [ABS.QTypeSegment iid])) sts = iid `M.member` (M.unions (map methods sts))
    isInterface _ _ = False

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

    tMethodSig :: String -> ABS.MethSignat -> HS.ClassDecl
    tMethodSig ityp (ABS.MethSig tReturn (ABS.Ident mname) pars)  = HS.ClsDecl $
       HS.TypeSig noLoc [HS.Ident mname] (foldr  -- function application is right-associative
                                     (\ tpar acc -> HS.TyFun (tType tpar) acc)
                                     -- the this objectref passed as input to the method
                                     (HS.TyFun 
                                      (HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ObjectRef") (HS.TyVar $ HS.Ident "a"))
                                      (HS.TyApp ((HS.TyApp (HS.TyCon $ HS.UnQual $ HS.Ident "ABS") 
                                                              (HS.TyVar $ HS.Ident "a")) )
                                                (tType tReturn))
                                     )
                                     (map (\ (ABS.Par typ _) -> typ) pars))


    -- tPureExpWrap is a pure expression in the statement world
    -- what it basically does, is that it wraps "return" around tPureExp
    tPureExpWrap :: ABS.PureExp -> String -> Scope -> [Scope] -> String -> HS.Exp
    tPureExpWrap pexp cls clsScope scopes interf = 
       let thisFields = collect pexp currentClassScope
           fscope = M.unions scopes
           currentClassScope = clsScope M.\\ fscope
       in if null thisFields
          -- TODO: no type variables, has to be changed for polymorphic methods
          then (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp pexp [] clsScope fscope interf)) --  rhs  
          else
              HS.Paren $ HS.InfixApp 
                    (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                    (HS.QVarOp $ symbolI ">>=")
                    (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                       map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                    (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))
                                     -- TODO: no type variables, has to be changed for polymorphic methods
                                     ] (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (tPureExp pexp [] clsScope fscope interf)))
        
    tType :: ABS.Type -> HS.Type
    tType t = tTypeOrTyVar [] t     -- no type variables in scope

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


    tMain :: ABS.MaybeBlock -> [HS.Decl]
    tMain ABS.NoBlock = []
    tMain (ABS.JustBlock (ABS.Bloc block)) = 
       -- main can only return with: return Unit;
       HS.PatBind noLoc (HS.PVar (HS.Ident "mainABS")) Nothing (HS.UnGuardedRhs $ tBlockWithReturn block
                                                                      ("Top")
                                                                      M.empty -- (error "No context for this")
                                                                      []
                                                                      (error "no class context")
                                                               ) (HS.BDecls [])
                                      :
                                      [HS.PatBind noLoc (HS.PVar (HS.Ident "main")) Nothing 
                                             (HS.UnGuardedRhs (HS.App (HS.Var (HS.UnQual (HS.Ident "main_is")))
                                                                      (HS.Var (HS.UnQual (HS.Ident "mainABS"))))) (HS.BDecls [])]

    tBlockWithReturn :: [ABS.Stm] -> String -> Scope -> [Scope] -> String -> HS.Exp
    -- method block or main block
    -- can return and also pushes a new scope
    tBlockWithReturn stmts cls clsScope scopes interfName = tBlock stmts True cls clsScope scopes interfName

    tBlock :: [ABS.Stm] -> Bool -> String -> Scope -> [Scope] -> String -> HS.Exp
    tBlock [] _canReturn _ _ _ _ = eReturnUnit
    tBlock stmts canReturn cls clsScope scopes interf = HS.Do $ tStmts stmts canReturn cls clsScope (M.empty:scopes) interf ++
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
                                            

                                          

    -- tail-recursive tStmts
    tStmts :: [ABS.Stm] -> Bool -> String -> Scope -> [Scope] -> String -> [HS.Stmt]
    tStmts [] _canReturn _ _ _ _ = []
    tStmts (stmt:rest) canReturn cls clsScope scopes interf = case stmt of
                       ABS.SExp e -> HS.Qualifier (case e of
                                      -- TODO: have to force to WHNF
                                      ABS.ExpE eexp -> tEffExpWrap eexp cls clsScope scopes interf 
                                      ABS.ExpP texp -> tPureExpWrap texp cls clsScope scopes interf)
                                                           : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SSuspend -> HS.Qualifier (HS.Var $ HS.UnQual $ HS.Ident "suspend") : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SBlock stmts -> HS.Qualifier (tBlock stmts False cls clsScope scopes interf) : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SThrow texp ->  (HS.Qualifier (HS.App 
                                                               (HS.Var (HS.UnQual $ HS.Ident "throw"))
                                                               (tPureExpWrap texp cls clsScope scopes interf)))  -- takes a pureexp to throw
                                          : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SSkip ->  HS.Qualifier (HS.Var (HS.UnQual $ HS.Ident "skip")) : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SReturn e -> if canReturn
                                       then if null rest
                                            -- nothing follows the return, normalize to a sole exp
                                            then (tStmts [ABS.SExp e] canReturn cls clsScope scopes interf)
                                            else error "Return must be the last statement"
                                       else error "Return must be the last statement" -- maybe differentiate between these two errors
                       ABS.SIf texp stm -> HS.Qualifier (HS.App 
                                                              (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenM") (tPureExpWrap texp cls clsScope scopes interf))
                                                        (tBlock [stm] False cls clsScope scopes interf)) : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SIfElse texp stm_then stm_else -> HS.Qualifier (HS.App
                                                                                (HS.App
                                                                                       (HS.App (HS.Var $ HS.UnQual $ HS.Ident "ifthenelseM") (tPureExpWrap texp cls clsScope scopes interf))
                                                                                       (tBlock [stm_then] False cls clsScope scopes interf))
                                                                                (tBlock [stm_else] False cls clsScope scopes interf)) : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SAssert texp -> HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "assert") (tPureExpWrap texp cls clsScope scopes interf)) : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SWhile texp stm -> HS.Generator noLoc (HS.PTuple HS.Boxed patVars) -- lhs
                                             (HS.App (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "while")
                                                                    (HS.Tuple HS.Boxed initVars)) -- initial environment, captured by the current environment
                                                      (HS.Lambda noLoc [HS.PTuple HS.Boxed patVars] (tPureExpWrap texp cls clsScope scopes interf))) -- the predicate
                                                      (HS.Lambda noLoc [HS.PTuple HS.Boxed patVars] -- the loop block
                                                         (HS.Do $ tStmts (case stm of
                                                                           ABS.SBlock stmts ->  stmts 
                                                                           stmt -> [stmt]) False cls clsScope (M.empty:scopes) interf ++ [HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Tuple HS.Boxed expVars))])))
                                             : tStmts rest canReturn cls clsScope scopes interf
                                                 where
                                                   fscope = M.unions scopes
                                                   vars = nub $ collectAssigns stm fscope
                                                   patVars = map (\ v -> HS.PVar $ HS.Ident v) vars
                                                   initVars = map (\ v -> if ABS.Ident v `M.member` fscope
                                                                         then HS.Var $ HS.UnQual $ HS.Ident v -- it's already in scope
                                                                         else (HS.Var $ identI "undefined") -- initialize to undefined

                                                                  ) vars
                                                   expVars = map (\ v -> HS.Var $ HS.UnQual $ HS.Ident v) vars

                       ABS.SDec typ ident -> tStmts rest canReturn cls clsScope (addToScope scopes ident typ) interf
                       -- dec just adds the ident with its type to the scope
                       -- TODO: remove the ident from the class attributes to check

                       -- normalize it to Dec + Ass
                       ABS.SDecAss typ ident texp -> tStmts (ABS.SDec typ ident : ABS.SAss ident texp : rest) canReturn cls clsScope scopes interf

                       ABS.SAss ident@(ABS.Ident var) (ABS.ExpP texp) ->  case M.lookup ident (M.unions scopes) of
                                                                           Just t -> (HS.Generator noLoc 
                                                                                     -- lhs
                                                                                     (case t of
                                                                                        ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                                                                        ptyp -> HS.PatTypeSig noLoc (HS.PVar $ HS.Ident var)  (tType ptyp))
                                                                                     -- rhs
                                                                                     (tPureExpWrap texp cls clsScope scopes interf)) : tStmts rest canReturn cls clsScope scopes interf
                                                                           Nothing -> case M.lookup ident clsScope of -- maybe it is in the class scope
                                                                                     -- normalize it to a field ass
                                                                                     Just _t -> tStmts (ABS.SFieldAss ident (ABS.ExpP texp) : rest) canReturn cls clsScope scopes interf
                                                                                     Nothing -> error (var ++ " not in scope")
                                                                             

                       ABS.SAss ident@(ABS.Ident var) (ABS.ExpE eexp)->  
                           case M.lookup ident (M.unions scopes) of
                             Just t -> (HS.Generator noLoc
                                       -- lhs
                                       (case t of
                                          ABS.TUnderscore -> (HS.PVar $ HS.Ident var) -- infer the type
                                          ptyp -> HS.PatTypeSig noLoc (HS.PVar $ HS.Ident var)  (tType ptyp))
                                       -- rhs
                                       ((case eexp of
                                           ABS.New _ _ -> liftInterf ident clsScope scopes
                                           ABS.NewLocal _ _ -> liftInterf ident clsScope scopes
                                           _ -> id ) (tEffExpWrap eexp cls clsScope scopes interf)))
                                       : tStmts rest canReturn cls clsScope scopes interf
                             Nothing -> case M.lookup ident clsScope of -- maybe it is in the class scope
                                         -- normalize it to a field ass
                                         Just _t -> tStmts (ABS.SFieldAss ident (ABS.ExpE eexp) : rest) canReturn cls clsScope scopes interf
                                         Nothing -> error (var ++ " not in scope")

                       ABS.SFieldAss (ABS.Ident ident) (ABS.ExpP texp) ->  (HS.Qualifier (HS.Paren $ HS.InfixApp 
                                                                                               (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ ident)
                                                                                               (HS.QVarOp $ symbolI "=<<")
                                                                                         (HS.Paren (tPureExpWrap texp cls clsScope scopes interf)))) -- paren are necessary here
                                                                          : tStmts rest canReturn cls clsScope scopes interf
                       ABS.SFieldAss ident@(ABS.Ident var) (ABS.ExpE eexp)-> 
                           (HS.Qualifier (HS.Paren $ HS.InfixApp 
                                          (HS.Var $ HS.UnQual $ HS.Ident $ "set_" ++ headToLower cls ++ "_" ++ var)
                                          (HS.QVarOp $ symbolI "=<<")
                                          ((case eexp of
                                              ABS.New _ _ -> liftInterf ident clsScope scopes
                                              ABS.NewLocal _ _ -> liftInterf ident clsScope scopes
                                              _ -> id )
                                          (tEffExpWrap eexp cls clsScope scopes interf))))
                                 : tStmts rest canReturn cls clsScope scopes interf
                           
                       ABS.SAwait g -> (HS.Qualifier (HS.App (HS.Var $ HS.UnQual $ HS.Ident "await") (tAwaitGuard g cls clsScope scopes interf))) :
                                                                    (tStmts rest canReturn cls clsScope scopes interf)

                       ABS.STryCatchFinally try_stm cbranches mfinally -> HS.Qualifier
                            -- (optionally) wrap the try-catch in a finally block
                            ((case mfinally of
                                ABS.NoFinally -> id
                                ABS.JustFinally fstm -> (\ try_catch -> 
                                                            (HS.App (HS.App (HS.Var $ HS.UnQual $ HS.Ident "finally")
                                                                     try_catch)
                                                             (tBlock [fstm] False cls clsScope scopes interf)))) 
                             (HS.App
                                    (HS.App (HS.Var (HS.UnQual $ HS.Ident "catches"))
                                           (tBlock [try_stm] False cls clsScope scopes interf))
                                    (HS.List (map 
                                              (\ (ABS.CatchBranc pat cstm) -> HS.App
                                                                             (HS.Con (identI "Handler"))
                                                                             -- TODO: if hse support lambdacase, it will lead to cleaner code
                                                                             (HS.Lambda  noLoc (case pat of
                                                                                                 -- a catch-all is a wrapped someexception
                                                                                                 ABS.PUnderscore -> [HS.PApp 
                                                                                                                          (identI "SomeException")
                                                                                                                          [HS.PWildCard]]
                                                                                                 -- otherwise generate a pattern-match catch_all
                                                                                                 _ -> [HS.PVar $ HS.Ident "__0"]
                                                                                               )
                                                                              (case pat of
                                                                                 -- wrap the normal returned expression in a just
                                                                                 ABS.PUnderscore -> (HS.App (HS.App (HS.Var $ identI "liftM") (HS.Con $ HS.UnQual $ HS.Ident "Just")) (HS.Paren $ (tBlock [cstm] False cls clsScope scopes interf)))
                                                                                 _ -> HS.Case (HS.Var $ HS.UnQual $ HS.Ident "__0")
                                                                                     [HS.Alt noLoc (tFunPat pat)
                                                                                      -- wrap the normal returned expression in a just
                                                                                      (HS.UnGuardedAlt (HS.App (HS.App (HS.Var $ identI "liftM") (HS.Con $ HS.UnQual $ HS.Ident "Just")) (HS.Paren $ (tBlock [cstm] False cls clsScope scopes interf)))) (HS.BDecls []),
                                                                                      -- pattern match fail, return nothing
                                                                                      HS.Alt noLoc HS.PWildCard (HS.UnGuardedAlt $ (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return") (HS.Con $ HS.UnQual $ HS.Ident "Nothing"))) (HS.BDecls [])])))
                                                                           cbranches))))
                                                                         : tStmts rest canReturn cls clsScope scopes interf

    liftInterf ident@(ABS.Ident var) clsScope scopes = case M.lookup ident (M.union (M.unions scopes) clsScope) of
                                Nothing -> error $ "Identifier " ++ var ++ " cannot be resolved from scope"
                                Just (ABS.TUnderscore) -> error $ "Cannot interface type for variable" ++ var
                                Just (ABS.TSimple (ABS.QType qids)) -> HS.App (HS.App (HS.Var $ identI "liftM") (HS.Var $ HS.UnQual $ HS.Ident $ (\ (ABS.QTypeSegment (ABS.TypeIdent iid)) -> iid) (last qids)))
                                Just _ -> error $ var ++ " not of interface type"

    -- tEffExpWrap is a wrapper arround tEffExp that adds a single read to the object pointer to collect the necessary fields
    -- it is supposed to be an optimization compared to reading each time the field at the place it is accessed
    tEffExpWrap eexp cls clsScope scopes interf = (let argsExps = case eexp of
                                                         ABS.Get pexp -> [pexp]
                                                         ABS.New _ pexps  -> pexps
                                                         ABS.NewLocal _ pexps -> pexps
                                                         ABS.SyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                                                         ABS.ThisSyncMethCall _ pexps -> pexps
                                                         ABS.AsyncMethCall pexp1 _ pexps2 -> pexp1:pexps2
                                                         ABS.ThisAsyncMethCall _ pexps -> pexps
                                                       -- collect this.fields, so we can read them
                                                       thisFields = concatMap ((flip collect) currentClassScope) argsExps
                                     in
                                 (if null thisFields
                                  then tEffExp eexp cls clsScope scopes interf
                                  else -- readObject this >>= \ Class1 { record bindings   } ->
                                      HS.Paren $ HS.InfixApp 
                                                 (HS.Var $ HS.UnQual $ HS.Ident "readThis")
                                                (HS.QVarOp $ symbolI ">>=")
                                                (HS.Lambda noLoc [(HS.PRec (HS.UnQual $ HS.Ident cls) $ -- introduce bindings
                                                                   map (\ arg -> HS.PFieldPat (HS.UnQual $ HS.Ident (headToLower cls ++ '_' : arg)) 
                                                                                (HS.PVar $ HS.Ident $ "__" ++ arg) )  (nub thisFields))
                                                                 ]
                                                 (tEffExp eexp cls clsScope scopes interf))))
      where
        currentClassScope = clsScope M.\\ (M.unions scopes)


    tAwaitGuard :: ABS.Guard -> String -> Scope -> [Scope] -> String -> HS.Exp
    tAwaitGuard (ABS.VarGuard (ABS.Ident ident)) cls clsScope scopes interf = HS.App
                                               (HS.Con $ HS.UnQual $ HS.Ident "FutureGuard")
                                               (HS.Var $ HS.UnQual $ HS.Ident ident)
    tAwaitGuard (ABS.ExpGuard pexp) cls clsScope scopes interf = let awaitFields = collect pexp currentClassScope
                                                in
                                                  (HS.App (HS.App (HS.Con $ HS.UnQual $ HS.Ident "ThisGuard") 
                                                                 (HS.List (map (HS.Lit . HS.Int . toInteger) (findIndices ((\ (ABS.Ident field) -> field `elem` awaitFields)) (M.keys clsScope)))))
                                                    (tPureExp pexp [] clsScope fscope interf))
        where fscope = M.unions scopes
              currentClassScope = clsScope M.\\ fscope




    tAwaitGuard (ABS.FieldGuard (ABS.Ident ident)) cls clsScope _ _ = error "Not implemented yet, take Cosimo's consideration into account"
    tAwaitGuard (ABS.AndGuard gl gr) cls clsScope scopes interf = HS.Paren $ HS.InfixApp 
                                   (tAwaitGuard gl cls clsScope scopes interf)
                                   (HS.QVarOp $ HS.UnQual  $ HS.Symbol ":&:")
                                   (tAwaitGuard gr cls clsScope scopes interf)


    tEffExp :: ABS.EffExp -> String -> Scope -> [Scope] -> String -> HS.Exp
    tEffExp (ABS.New (ABS.TSimple (ABS.QType qtids)) pexps) _cls clsScope scopes interf = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp [] clsScope fscope interf))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ "__" ++ headToLower ( (\ (ABS.QTypeSegment (ABS.TypeIdent cid)) -> cid) (last qtids)))))
                                                                        pexps))
        where fscope = (M.unions scopes)
    tEffExp (ABS.New _ _) _ _ _ _ = error "Not valid class name"

    tEffExp (ABS.NewLocal (ABS.TSimple (ABS.QType qtids)) pexps) _cls clsScope scopes interf = (HS.App
                                                                       (HS.Var $ HS.UnQual $ HS.Ident "new_local")
                                                                       (foldl
                                                                        (\ acc pexp -> HS.App acc (tPureExp pexp [] clsScope fscope interf))
                                                                        (HS.Var  
                                                                               ((let mids = init qtids
                                                                                in
                                                                                  if null mids
                                                                                  then HS.UnQual
                                                                                  else HS.Qual (HS.ModuleName $ joinQualTypeIds mids))
                                                                               (HS.Ident $ "__" ++ headToLower ((\ (ABS.QTypeSegment (ABS.TypeIdent cid)) -> cid) (last qtids)))))
                                                                        pexps))
        where fscope = (M.unions scopes)

    tEffExp (ABS.NewLocal _ _) _ _ _ _ = error "Not valid class name"


    tEffExp (ABS.SyncMethCall texp (ABS.Ident method) args) _cls clsScope scopes interf = HS.Paren $ HS.App 
                                                               (foldl
                                                                (\ acc arg -> HS.App acc (tPureExp arg [] clsScope fscope interf))
                                                                (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_sync")
                                                                args)
       (tPureExp texp [] clsScope fscope interf)
        where fscope = (M.unions scopes)
    -- normalize
    tEffExp (ABS.ThisSyncMethCall method args) cls clsScope scopes interf = tEffExp (ABS.SyncMethCall (ABS.ELit $ ABS.LThis) method args) cls clsScope scopes interf

    tEffExp (ABS.AsyncMethCall texp (ABS.Ident method) args) _cls clsScope scopes interf = HS.Paren $ HS.App 
                                                               (foldl
                                                                (\ acc arg -> HS.App acc (tPureExp arg [] clsScope fscope interf))
                                                                (HS.Var $ HS.UnQual $ HS.Ident $ method ++ "_async")
                                                                args)
       (tPureExp texp [] clsScope fscope interf)
        where fscope = M.unions scopes
    -- normalize
    tEffExp (ABS.ThisAsyncMethCall method args) cls clsScope scopes interf = tEffExp (ABS.AsyncMethCall (ABS.ELit $ ABS.LThis) method args) cls clsScope scopes interf

    tEffExp (ABS.Get texp) _cls clsScope scopes interf = HS.App (HS.Var $ HS.UnQual $ HS.Ident "get") (tPureExp texp [] clsScope fscope interf)
        where fscope = (M.unions scopes)

    tModuleName :: ABS.QualType -> HS.ModuleName
    tModuleName (ABS.QType qtis) = HS.ModuleName $ joinQualTypeIds qtis

    eReturnUnit :: HS.Exp
    eReturnUnit = (HS.App (HS.Var $ HS.UnQual $ HS.Ident "return")
                         (HS.Con $ HS.Special $ HS.UnitCon)) -- return ()

    in mapM_ (ppTranslatedHaskell . tProgram) asts


-- Utils

joinQualTypeIds :: [ABS.QualTypeSegment] -> String
joinQualTypeIds qtids = concat $ intersperse "." $ map (\ (ABS.QTypeSegment (ABS.TypeIdent str)) -> str) qtids

typOfConstrType :: ABS.ConstrType -> ABS.Type
typOfConstrType (ABS.EmptyConstrType typ) = typ
typOfConstrType (ABS.RecordConstrType typ _) = typ

headToLower :: String -> String
headToLower (x:xs) = toLower x : xs

-- collects pure variables and class attributes
collect                              :: ABS.PureExp -> Scope -> [String]
collect (ABS.Let _ pexp1 pexp2) ccs      = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.If pexp1 pexp2 pexp3) ccs   = collect pexp1 ccs ++ collect pexp2 ccs ++ collect pexp3 ccs
collect (ABS.Case pexp cbranches) ccs    = collect pexp ccs ++ concatMap (\ (ABS.CaseBranc _ pexp') -> collect pexp' ccs) cbranches
collect (ABS.EOr pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EAnd pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EEq pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.ENeq pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.ELt pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.ELe pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EGt pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EGe pexp1 pexp2) ccs        = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EAdd pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.ESub pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EMul pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EDiv pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.EMod pexp1 pexp2) ccs       = collect pexp1 ccs ++ collect pexp2 ccs
collect (ABS.ELogNeg pexp) ccs           = collect pexp ccs
collect (ABS.EIntNeg pexp) ccs           = collect pexp ccs
collect (ABS.EFunCall _ pexps) ccs          = concatMap ((flip collect) ccs) pexps
collect (ABS.ENaryFunCall _ pexps) ccs      = concatMap ((flip collect) ccs) pexps
collect (ABS.EParamConstr _ pexps) ccs    = concatMap ((flip collect) ccs) pexps
collect (ABS.EThis (ABS.Ident attr)) _ccs = [attr] -- qualify it
collect (ABS.EVar ident@(ABS.Ident var)) ccs = if ident `M.member` ccs -- currentClassScope
                                               then [var]
                                               else []
collect _ _ = []




collectAssigns :: ABS.Stm -> Scope -> [String]
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


-- unify two interfaces, to their *Common interface*
-- will return Nothing if the interfaces are not unifiable
joinSub :: ABS.QualType -> ABS.QualType -> [ModuleST] -> Maybe ABS.QualType
joinSub interf1 interf2 _ | interf1 == interf2 = Just interf1 -- same interface subtyping
joinSub interf1 interf2 symbolTable | otherwise = let 
    unionedST = (M.unions $ map hierarchy symbolTable) :: M.Map ABS.TypeIdent [ABS.QualType]
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

type Scope = M.Map ABS.Ident (ABS.Type)

-- this scope is the oo-scope: it does not allow re-declaration
-- pure-scope is done with lambdas, so it allows re-declaration
addToScope :: [Scope] -> ABS.Ident -> ABS.Type -> [Scope]
addToScope (topscope:restscopes) var@(ABS.Ident pid) typ = 
    if (any (\ scope -> var `M.member` scope) restscopes)
    then error $ pid ++ " already defined in an outer scope"
    else M.insertWith (const $ const $ error $ pid ++ " already defined in this scope") var typ topscope  : restscopes


-- create Include-qualified Haskell *identifiers* for the generated stub code to *not clash* with ABS user-written code
identI :: String -> HS.QName
identI str = HS.Qual (HS.ModuleName "I__") (HS.Ident str)

-- create Include-qualified Haskell *symbols* for the generated stub code to *not clash* with ABS user-written code
symbolI :: String -> HS.QName
symbolI sym = HS.Qual (HS.ModuleName "I__") (HS.Symbol sym)

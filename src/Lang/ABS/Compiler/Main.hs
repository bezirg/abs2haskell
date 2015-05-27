module Main where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Conf
import Lang.ABS.Compiler.Top (tProg)
import Lang.ABS.Compiler.Utils

import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.Pretty as HS (prettyPrint)
import qualified Lang.ABS.Compiler.BNFC.ParABS as BNFC (myLexer, pProgram)
import qualified Lang.ABS.Compiler.BNFC.ErrM as BNFC

import System.FilePath ((</>), replaceExtension, replaceFileName)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import System.Console.CmdArgs (cmdArgs)
import Control.Monad (when, liftM)

-- | The a2h transcompiler main entrypoint
main :: IO ()
main = do
  conf <- cmdArgs confOpt
  if null (srcFiles conf)
   then error "No ABS files to translate are given as input. Try --help"
   else do
     let ?conf = conf
     isDir <- doesDirectoryExist (outputDir conf)
     when (not isDir) $ error "Output directory does not exist. Please create it first."
     asts <- liftM concat $ mapM absParseFileOrDir (srcFiles conf)
     let ?moduleTable = concatMap firstPass asts :: ModuleTable -- executes 1st-pass of all modules to collect info
     mapM_ (\ (fp, ast) -> mapM_ (ppHaskellFile fp) (tProg ast)) asts     -- calls the program-translator on each AST and prettyprints its Haskell output
     -- TODO: do not translate modules that are not dependent in the main module


-- | 1st-pass of an ABS file: Given an ABS source file, it collects info about the module
-- the interfaces hierarchy, the methods, and exceptions declared in the module
firstPass :: (FilePath, ABS.Program) -> [ModuleInfo]
firstPass (fp, (ABS.Prog moduls)) = map firstPass' moduls
    where
 firstPass' (ABS.Modul mName _es is decls _) = ModuleInfo {
   filePath = fp,
   moduleName = mName,
   hierarchy = foldl insertInterfs 
   -- the IDC is a principal interface of the stdlib
               (M.singleton (ABS.UIdent ((-1,-1), "IDC")) []) decls,
   methods = foldl insertMethods 
             -- the IDC is the stdlib interface with 2 methods
             (M.singleton (ABS.UIdent ((-1,-1),"IDC")) 
                   [ABS.LIdent ((-1,-1),"shutdown"),
                    ABS.LIdent ((-1,-1),"getLoad")]) decls,
   exceptions = foldl (\ acc decl -> 
                           case decl of
                             (ABS.AnnDec _ (ABS.ExceptionDecl cident)) -> (case cident of
                                                           ABS.SinglConstrIdent tid -> tid
                                                           ABS.ParamConstrIdent tid _ -> tid) : acc
                             _ -> acc) [] decls,
   fimports = foldl (\ acc imp ->
                         case imp of
                           --ABS.AnyImport ABS.ForeignImport anyidents _ TODO: qualified foreign
                           ABS.AnyFromImport ABS.ForeignImport anyidents _ -> anyidents ++ acc
                           _ -> acc) [] is,
   exports = undefined -- TODO
                                                }
    where 
      insertInterfs :: M.Map ABS.UIdent [ABS.QType] -> ABS.AnnotDecl -> M.Map ABS.UIdent [ABS.QType]
      insertInterfs acc (ABS.AnnDec _ (ABS.InterfDecl tident@(ABS.UIdent (p,_)) _msigs)) = M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident [] acc
      insertInterfs acc (ABS.AnnDec _ (ABS.ExtendsDecl tident@(ABS.UIdent (p,_)) extends _msigs)) = M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident extends acc
      insertInterfs acc _ = acc

      insertMethods :: M.Map ABS.UIdent [ABS.LIdent] -> ABS.AnnotDecl -> M.Map ABS.UIdent [ABS.LIdent]
      insertMethods acc (ABS.AnnDec a (ABS.InterfDecl tident msigs)) = insertMethods acc (ABS.AnnDec a (ABS.ExtendsDecl tident [] msigs))  -- normalization
      insertMethods acc (ABS.AnnDec _ (ABS.ExtendsDecl tident@(ABS.UIdent (p,_)) _extends msigs)) = 
          {- TODO it could generate a compilation error because of duplicate method declaration -}
          M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident (collectMethods msigs) acc
      insertMethods acc _ = acc
      collectMethods :: [ABS.AnnotMethSignat] -> [ABS.LIdent]
      collectMethods = map (\ (ABS.AnnMethSig _ (ABS.MethSig _ ident _)) -> ident)


-- | parse whole ABS src directories
--
-- TODO: to recurse more deep than just 1 level
absParseFileOrDir :: (?conf :: Conf)  => FilePath -> IO [(FilePath, ABS.Program)]
absParseFileOrDir fileOrDir = do
  isdir <- doesDirectoryExist fileOrDir
  if isdir
    then do
      contents <- getDirectoryContents fileOrDir
      let absFiles = filter (isSuffixOf ".abs") contents
      mapM (\ relativeFile -> parseFile (fileOrDir </> relativeFile)) absFiles
    else liftM return $ parseFile fileOrDir
 where
    -- parse wrapper for an ABS file, 
    parseFile absFilePath = do
      isfile <- doesFileExist absFilePath
      when (not isfile) $ error "ABS file does not exist"
      absSource <- readFile absFilePath
      let parseABS = BNFC.pProgram $ BNFC.myLexer absSource
      case parseABS of
        BNFC.Ok res -> do
          -- if --ast option is enabled, print ABSFile.ast, containing the AST haskell datatype
          when (dumpAST ?conf) $ writeFile (replaceExtension absFilePath ".ast") (show  res)
          return (absFilePath, res)
        BNFC.Bad _errorString -> error "Error in parsing" -- TODO: move to exceptions

-- | pretty-print a Haskell-AST to a file ".hs"
ppHaskellFile :: (?conf::Conf) => FilePath -> HS.Module -> IO ()
ppHaskellFile fp m@(HS.Module _ (HS.ModuleName s) _ _ _ _ _) = do
  let haskellFilePath = if s == "Main"
                        then replaceExtension fp "hs" -- it's the main module, use the name of the parent filepath
                        else replaceFileName fp (map (\ c -> if c == '.' then '/' else c) s  ++ ".hs")
  writeFile (outputDir ?conf </> haskellFilePath) (HS.prettyPrint m)



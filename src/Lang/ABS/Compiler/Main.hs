module Main where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Conf
import Lang.ABS.Compiler.Top (tProg)
import Lang.ABS.Compiler.Utils
import Control.Monad (liftM)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Language.Haskell.Exts.Syntax as HS
import Lang.ABS.Compiler.BNFC.ParABS (myLexer, pProgram)
import Lang.ABS.Compiler.BNFC.ErrM
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath ((</>), replaceExtension, replaceFileName)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Control.Monad (when)

main :: IO ()
main = do
  let Conf {files = inputFilesOrDirs, outputdir = odir} = conf
  if null inputFilesOrDirs
   then error "No ABS files to translate are given as input. Try --help"
   else do
     isDir <- doesDirectoryExist odir
     when (not isDir) $ error "Output directory does not exist. Please create it first."
     asts <- liftM concat $ mapM absParseFileOrDir inputFilesOrDirs
     let ?moduleTable = concatMap firstPass asts :: ModuleTable -- executes 1st-pass of all modules to collect info
     mapM_ (\ (fp, ast) -> mapM_ (ppHaskellFile fp) (tProg ast)) asts     -- calls the program-translator on each AST and prettyprints its Haskell output
     -- TODO: do not translate modules that are not dependent in the main module


-- | 1st-pass of an ABS file: Given an ABS source file, it collects info about the module
-- the interfaces hierarchy, the methods, and exceptions declared in the module
firstPass :: (FilePath, ABS.Program) -> [ModuleInfo]
firstPass (fp, (ABS.Prog moduls)) = map (firstPass' fp) moduls
firstPass' fp (ABS.Modul mName _ _ decls _) = ModuleInfo {
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
      insertInterfs :: M.Map ABS.UIdent [ABS.QType] -> ABS.Decl -> M.Map ABS.UIdent [ABS.QType]
      insertInterfs acc (ABS.InterfDecl tident@(ABS.UIdent (p,_)) _msigs) = M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident [] acc
      insertInterfs acc (ABS.ExtendsDecl tident@(ABS.UIdent (p,_)) extends _msigs) = M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident extends acc
      insertInterfs acc _ = acc

      insertMethods :: M.Map ABS.UIdent [ABS.LIdent] -> ABS.Decl -> M.Map ABS.UIdent [ABS.LIdent]
      insertMethods acc (ABS.InterfDecl tident msigs) = insertMethods acc (ABS.ExtendsDecl tident [] msigs)  -- normalization
      insertMethods acc (ABS.ExtendsDecl tident@(ABS.UIdent (p,_)) _extends msigs) = 
          {- TODO it could generate a compilation error because of duplicate method declaration -}
          M.insertWith (const $ const $ errorPos p "duplicate interface declaration") tident (collectMethods msigs) acc
      insertMethods acc _ = acc
      collectMethods :: [ABS.MethSignat] -> [ABS.LIdent]
      collectMethods = map (\ (ABS.MethSig _ ident _) -> ident)


-- parse whole ABS src directories (TODO: to recurse more deep than just 1 level)
absParseFileOrDir :: FilePath -> IO [(FilePath, ABS.Program)]
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
      let parseABS = pProgram $ myLexer absSource
      case parseABS of
        Ok res -> do
          -- if --ast option is enabled, print ABSFile.ast, containing the AST haskell datatype
          when (ast conf) $ writeFile (replaceExtension absFilePath ".ast") (show  res)
          return (absFilePath, res)
        Bad _errorString -> error "Error in parsing" -- TODO: move to exceptions

ppHaskellFile fp m@(HS.Module _ (HS.ModuleName s) _ _ _ _ _) = do
  let haskellFilePath = if s == "Main"
                        then replaceExtension fp "hs" -- it's the main module, use the name of the parent filepath
                        else replaceFileName fp (map (\ c -> if c == '.' then '/' else c) s  ++ ".hs")
  writeFile (outputdir conf </> haskellFilePath) (prettyPrint m)



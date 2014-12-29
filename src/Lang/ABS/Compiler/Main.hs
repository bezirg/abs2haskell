module Main where

import Lang.ABS.Compiler.Base
import Lang.ABS.Compiler.Conf
import Lang.ABS.Compiler.Top (tProg)
import Control.Monad (liftM)
import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import Lang.ABS.Compiler.BNFC.ParABS (myLexer, pProgram)
import Lang.ABS.Compiler.BNFC.ErrM
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath ((</>), replaceExtension)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Control.Monad (when)

main :: IO ()
main = do
  let Conf {files = inputFilesOrDirs} = conf
  asts <- liftM concat $ mapM absParseFileOrDir inputFilesOrDirs
  let ?moduleTable = map firstPass asts :: ModuleTable -- executes 1st-pass of all modules to collect info
  mapM_ (\ (fp, ast) -> ppHaskell fp (tProg fp ast)) asts     -- calls the program-translator on each AST and prettyprints its Haskell output
  -- TODO: do not translate modules that are not dependent in the main module


-- | 1st-pass of an ABS file: Given an ABS source file, it collects info about the module
-- the interfaces hierarchy, the methods, and exceptions declared in the module
firstPass :: (FilePath, ABS.Program) -> ModuleInfo
firstPass (fp, (ABS.Prog [ABS.Modul mName _ _ decls _])) = ModuleInfo {
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

ppHaskell absFilePath translation = do
  let haskellFilePath = replaceExtension absFilePath ".hs"
  writeFile haskellFilePath (prettyPrint translation)



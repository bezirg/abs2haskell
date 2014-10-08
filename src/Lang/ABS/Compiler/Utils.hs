module Lang.ABS.Compiler.Utils
    (parseABSFiles
    ,ppTranslatedHaskell
    ) where

import Lang.ABS.Compiler.Conf (conf, ast)
import Lang.ABS.Compiler.BNFC.ParABS (myLexer, pProgram)
import Lang.ABS.Compiler.BNFC.ErrM
import Language.Haskell.Exts.Pretty (prettyPrint)

import System.FilePath ((</>), replaceExtension)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf)
import Control.Monad (when, liftM)

-- parse whole ABS src directories (TODO: to recurse more deep than just 1 level)
parseABSFiles fileOrDir = do
  isdir <- doesDirectoryExist fileOrDir
  if isdir
    then do
      contents <- getDirectoryContents fileOrDir
      let absFiles = filter (isSuffixOf ".abs") contents
      mapM (\ relativeFile -> parseABSFile (fileOrDir </> relativeFile)) absFiles
    else liftM return $ parseABSFile fileOrDir
 where
    -- parse wrapper for an ABS file, 
    parseABSFile absFilePath = do
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

ppTranslatedHaskell (absFilePath, translation) = do
  let haskellFilePath = replaceExtension absFilePath ".hs"
  writeFile haskellFilePath (prettyPrint translation)

{-# LANGUAGE DeriveDataTypeable #-}

module Lang.ABS.Compiler.Conf
    (conf
    ,Conf (..)
    ) where

import System.IO.Unsafe (unsafePerformIO)
import System.Console.CmdArgs

conf = unsafePerformIO (cmdArgs confOpt)

data Conf = Conf {
      files :: [FilePath]
      , ast :: Bool
      , outputdir :: FilePath
    } deriving (Show, Eq, Data, Typeable)

confOpt = Conf {
          files = def &= args &= typ "FILES/DIRS"
          , outputdir = "." &= name "outputdir" &= typDir
          , ast = def &= name "ast" &= help "Output an .ast file containing the parsed AST Haskell datatype"
          }
          &= program "abs2haskell" &= help "ABS to Haskell transpiler" &= summary "abs2haskell v0.0.2, Nikolaos Bezirgiannis, Envisage Project"


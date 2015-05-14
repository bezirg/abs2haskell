{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.ABS.Compiler.Conf
    (confOpt
    ,Conf (..)
    ) where

import System.Console.CmdArgs
import Distribution.PackageDescription.TH -- for injecting  cabal version

data Conf = Conf {
      srcFiles :: [FilePath]
    , dumpAST :: Bool
    , outputDir :: FilePath
    } deriving (Show, Eq, Data, Typeable)

confOpt :: Conf
confOpt = Conf {
          srcFiles = def &= args &= typ "FILES/DIRS"
          , outputDir = "." &= name "output-dir" &= explicit &= typDir
          , dumpAST = def &= name "dump-ast" &= name "a" &= explicit &= help "Output an .ast file containing the parsed AST Haskell datatype"
          }
          &= program "abs2haskell" 
          &= help "a transcompiler from the ABS language to Haskell" 
          &= helpArg [explicit, name "h", name "help"]
          -- summary is --version
          &= summary ("The abs2haskell compiler v" ++ $(packageVariable (pkgVersion . package))  ++ " Nikolaos Bezirgiannis, Envisage Project")

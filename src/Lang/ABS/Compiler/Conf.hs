-- | Possible ABS-transcompiler options. For a summary, check: a2h --help
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.ABS.Compiler.Conf
    (confOpt
    ,Conf (..)
    ) where

import System.Console.CmdArgs
import Distribution.PackageDescription.TH -- for injecting  cabal version

data Conf = Conf {
      srcFiles :: [FilePath]     -- ^ The input ABS module files (ending in .abs)
    , dumpAST :: Bool            -- ^ A flag to dump the parsed AST in a ModuleName.ast file
    , outputDir :: Maybe FilePath      -- ^ In which directory to put all the Haskell translated files (.hs files)
    , createScript :: Bool             -- ^ creates a bash script for easier invoking the ghc Haskell compiler
    } deriving (Show, Eq, Data, Typeable)

confOpt :: Conf
confOpt = Conf {
            srcFiles = def &= args &= typ "FILES/DIRS"
          , outputDir = def &= name "output-dir" &= name "d" &= explicit &= typDir &= help "In which directory to put all the Haskell translated files (.hs files)"
          , dumpAST = def &= name "dump-ast" &= name "a" &= explicit &= help "A flag to dump the parsed AST in a ModuleName.ast file"
          , createScript = def &= name "create-script" &= name "c" &= explicit &= help "If given, creates a bash script for easier invoking the ghc Haskell compiler."
          }
          &= program "abs2haskell" 
          &= help "a transcompiler from the ABS language to Haskell" 
          &= helpArg [explicit, name "h", name "help"]
          -- summary is --version
          &= summary ("The abs2haskell compiler v" ++ $(packageVariable (pkgVersion . package))  ++ " Nikolaos Bezirgiannis, Envisage Project")

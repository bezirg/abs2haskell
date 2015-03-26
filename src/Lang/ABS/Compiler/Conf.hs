{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.ABS.Compiler.Conf
    (conf
    ,Conf (..)
    ) where

import System.IO.Unsafe (unsafePerformIO)
import System.Console.CmdArgs
import Distribution.PackageDescription.TH -- for injecting  cabal version

{-# NOINLINE conf #-}
conf = unsafePerformIO (cmdArgs confOpt)

data Conf = Conf {
      files :: [FilePath]
      , ast :: Bool
      , outputdir :: FilePath
    } deriving (Show, Eq, Data, Typeable)

confOpt = Conf {
          files = def &= args &= typ "FILES/DIRS"
          , outputdir = "." &= name "outputdir" &= explicit &= typDir
          , ast = def &= help "Output an .ast file containing the parsed AST Haskell datatype"
          }
          &= program "abs2haskell" 
          &= help "a transcompiler from the ABS language to Haskell" 
          &= helpArg [explicit, name "h", name "help"]
          &= summary ("abs2haskell " ++ $(packageVariable (pkgVersion . package))  ++ " Nikolaos Bezirgiannis, Envisage Project") -- this text is printed on --version

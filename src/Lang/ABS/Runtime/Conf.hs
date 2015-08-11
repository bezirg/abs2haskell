-- | Possible options passed to The ABS-Haskell runtime-system
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.ABS.Runtime.Conf
    (conf
    ,Conf (..)
    ) where

import System.Console.CmdArgs
import Distribution.PackageDescription.TH -- for injecting  cabal version

import System.IO.Unsafe (unsafePerformIO)
{-# NOINLINE conf #-}
conf :: Conf
conf = unsafePerformIO (cmdArgs confOpt)

data Conf = Conf {
      keepAlive :: Bool -- ^ If enabled, the ABS-program will keep running even after the end of the main-block process
    , traceExceptions :: Bool -- ^ The COGs consume any uncaught exceptions in their processes. This option prints to the stdout a message of the uncaught exception when it happens. Used mostly for debugging
    } deriving (Show, Eq, Data, Typeable)

confOpt :: Conf
confOpt = Conf {
            keepAlive = def &= explicit &= name "keep-alive" &= name "k" &= help "If enabled, the ABS-program will keep running even after the end of the main-block process"
          , traceExceptions = def &= explicit &= name "trace-exceptions" &= name "t" &= help "The COGs consume any uncaught exceptions in their processes. This option prints to the stdout a message of the uncaught exception when it happens. Used mostly for debugging"
          }
          &= program "The ABS-Haskell runtime" 
          &= help "The parallel&distributed runtime system of ABS-Haskell" 
          &= helpArg [explicit, name "h", name "help"]
          -- summary is --version
          &= summary ("The ABS-Haskell runtime v" ++ $(packageVariable (pkgVersion . package))  ++ " Nikolaos Bezirgiannis, Envisage Project")

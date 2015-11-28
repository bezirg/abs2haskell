{-# LANGUAGE NoImplicitPrelude, 
  ExistentialQuantification, MultiParamTypeClasses,
  PatternSignatures, DeriveDataTypeable, DeriveGeneric, InstanceSigs, FlexibleInstances #-}
module Lang.ABS.StdLib.LocalDC where
import qualified Lang.ABS.Runtime.Base as I__
import Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import System.Process hiding (main)
import System.Environment (getExecutablePath)
import Data.Binary (encode)
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Distributed.Process (ProcessId, processNodeId)
import Control.Distributed.Process.Closure
import Control.Distributed.Static
import Data.Rank1Typeable
import Data.List ((++))
import qualified Data.Binary as B__

data LocalDC = LocalDC{localDC_pid :: Fut ProcessId, localDC_port :: Int}
             deriving (I__.Typeable, I__.Generic)

__localDC port = LocalDC{localDC_port = port, localDC_pid = I__.NullFutureRef}

instance I__.Binary LocalDC

instance I__.Root_ LocalDC where
        __init this@(I__.ObjectRef _ thisCOG@(I__.COG (_, pid)) _) = do
          -- the creator runs the init
             port <- localDC_port <$!> I__.readThis this
             d0 <- I__.liftIO getExecutablePath
             __mvar <- I__.liftIO I__.newEmptyMVar
             __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
             I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
             let __f = I__.FutureRef __mvar thisCOG __counter
             I__.set 1 (\ v__ c__ -> c__{localDC_pid = v__}) __f this
             I__.liftIO (createProcess ((proc d0 ["--port", I__.show port, "--ip", I__.myIP, "-t"]) { env = Just [("FROM_PID", I__.show (B64.encode (B__.encode __f)))
                                                                                               ] }))
             return ()

 
instance I__.Sub (I__.Obj LocalDC) I__.Root where
        up = I__.Root
 
instance I__.Sub (I__.Obj LocalDC) IDC where
        up = IDC
 
instance IDC_ LocalDC where
        shutdown this = return ()
        load this
          = do (pure ((,,)) <*> ((/) <$!> pure 1 <*> pure 2) <*>
                  ((/) <$!> pure 1 <*> pure 2)
                  <*> ((/) <$!> pure 1 <*> pure 2))
        spawns :: forall o. (I__.Root_ o) => o -> I__.Obj LocalDC -> I__.ABS (I__.Obj o)
        spawns smart this = do
             fpid <- localDC_pid <$!> I__.readThis this
             dc_main_pid <- get fpid
             println (pure "before spawn")
             s <- I__.lift (I__.lift (call' (processNodeId dc_main_pid) 
                                               (I__.spawnClosure (staticLabel ((I__.show (typeOf (I__.undefined :: o))) ++ "__rootDict")) smart)))
             println(pure "after spawn")
             return s



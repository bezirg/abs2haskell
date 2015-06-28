{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module Lang.ABS.StdLib.DC where
import Lang.ABS.Runtime.Prim
import Lang.ABS.Runtime.Base
import qualified Lang.ABS.Compiler.Include as I__
-- import Lang.ABS.StdLib -- removed
import Lang.ABS.Compiler.Include -- added
import Lang.ABS.StdLib.Prelude -- added
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Data.List (words)
import System.IO (readFile)
import Prelude (Double(..))
import Text.Read (read)
import Prelude (toRational)
import Control.Concurrent.MVar (newMVar)
 
-- * The DC interface

-- | The deployment component ("IDC_") interface extends the "Object__" root-interface. Thus, deployment components are objects.
--
-- All DC classes must implement two methods: 'shutdown' and 'getLoad'.
class (Root_ a) => IDC_ a where
         
        shutdown :: Obj a -> ABS a Unit
         
        getLoad :: Obj a -> ABS a (Triple Rat Rat Rat)
 
-- | An existential-type wrapper for DC-derived objects (used for typing and subtyping)
data IDC = forall a . (IDC_ a) => IDC (Obj a)
 
instance Sub IDC IDC where
        up x = x
 
instance Sub (Obj Null) IDC where
        up = IDC
 
instance Sub IDC Root where
        up (IDC a) = Root a
 
instance IDC_ Null where
        shutdown
          = I__.error
              "this should not happen. report the program to the compiler developers"
        getLoad
          = I__.error
              "this should not happen. report the program to the compiler developers"

-- * DC Internals

__eqIDC (IDC NullRef) (IDC NullRef) = True
__eqIDC (IDC (ObjectRef _ id1 pid1)) (IDC (ObjectRef _ id2 pid2))
  = (id1 == id2) && (pid1 == pid2)
__eqIDC _ _ = False
 
instance I__.Eq IDC where
        (==) = __eqIDC
shutdown_sync ((IDC __obj@(ObjectRef __ioref _ _)))
  = do error "sync method calls of DC objects not allowed"
       -- REMOVED: we don't allow sync calls of DC objects
       -- __hereCOG <- thisCOG
       -- __obj1 <- I__.readRef __ioref
       -- otherCOG <- __cog __obj1
       -- I__.when (not (__hereCOG == otherCOG))
       --   (I__.error "Sync Call on a different COG detected")
       -- I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
       --   (shutdown __obj)
shutdown_sync (IDC NullRef) = error "sync method calls of DC objects not allowed"
shutdown_async ((IDC __obj@(ObjectRef __ioref _ _)))
  = do __obj1 <- I__.readRef __ioref
       COG (__chan, _) <- I__.thisCOG -- __cog __obj1  -- REMOVED: it does not matter where it is executed
       __mvar <- I__.liftIO I__.newEmptyMVar
       __hereCOG <- I__.thisCOG
       astate@(AState{aCounter = __counter}) <- I__.lift I__.get
       I__.lift (I__.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __hereCOG __counter
       I__.liftIO
         (I__.writeChan __chan (RunJob __obj __f (shutdown __obj)))
       return __f
shutdown_async (IDC NullRef) = I__.error "async call to null"

rload :: ProcessId -> Process ()
rload pid = do
  (s1: s5: s15: _) <- I__.liftIO (words <$> (readFile "/proc/loadavg"))
  send pid (toRational (read s1 :: Double), toRational (read s5 :: Double), toRational (read s15 :: Double))
  return ()

$(remotable ['rload])


getLoad_sync ((IDC __obj@(ObjectRef __ioref _ _)))
  = do error "sync method calls of DC objects not allowed"
       -- REMOVED: we don't do same-COG-check for DC objects
       -- __hereCOG <- thisCOG
       -- __obj1 <- I__.readRef __ioref
       -- otherCOG <- __cog __obj1
       -- I__.when (not (__hereCOG == otherCOG))
       --   (I__.error "Sync Call on a different COG detected")
       -- I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
       --   (getLoad __obj)
getLoad_sync (IDC NullRef) = error "sync method calls of DC objects not allowed"
getLoad_async ((IDC __obj@(ObjectRef __ioref (COG (_,pid)) _)))
  = do __obj1 <- I__.readRef __ioref
       lnid <- I__.lift (I__.lift getSelfNode)
       let rnid = processNodeId pid
       __hereCOG@(COG (__chan, _)) <- thisCOG -- __cog __obj1 -- REMOVED: it does not matter where it is executed
       astate@(AState{aCounter = __counter}) <- I__.lift I__.get
       I__.lift (I__.put (astate{aCounter = __counter + 1}))
       if rnid == lnid
         then do
           -- fakely-async call. we turn it to sync, because thisDC object does not live in a COG 
           res <- I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj})) (getLoad __obj)
           __mvar <- I__.liftIO (newMVar res)
           let __f = FutureRef __mvar __hereCOG __counter
           return __f
         else do
           self <- lift $ lift $ getSelfPid -- this is probably wrong, it has to record the forwarder_pid, not the cog_pid
           lift $ lift $ spawn rnid ($(mkClosure 'rload) pid)
           res <- lift $ lift $ expect :: ABS o (Triple Rat Rat Rat)
           __mvar <- I__.liftIO (newMVar res)
           let __f = FutureRef __mvar __hereCOG __counter
           return __f

getLoad_async (IDC NullRef) = I__.error "async call to null"


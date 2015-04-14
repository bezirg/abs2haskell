{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module NebulaDC where
import Lang.ABS.Runtime
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.StdLib
import OpenNebula hiding (main)
import Control.Distributed.Process (NodeId(..))
import Control.Distributed.Process.Internal.Types (nullProcessId) -- DC is under a null COG-process
--import Network.Transport.TCP (encodeEndPointAddress)
import System.IO (readFile)
import Data.List (words)
import Prelude (Double(..))
import Text.Read (read)
import Prelude (toRational)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (lookup)
import Data.List (map)
import Data.Maybe (fromMaybe)
import Control.Concurrent (myThreadId)
import Network.Transport.TCP (encodeEndPointAddress)
 
data NebulaDC = NebulaDC{nebulaDC_loc :: (Object__ o) => ABS o COG,
                         nebulaDC_cpu :: Int, nebulaDC_load :: Int, nebulaDC_memory :: Int,
                         nebulaDC_nodeId :: Maybe NodeId, nebulaDC_vmId :: Int}
__nebulaDC cpu memory
  = NebulaDC{nebulaDC_cpu = cpu, nebulaDC_memory = memory}
 
instance Object__ NebulaDC where
        new __cont
          = do __chan <- I__.liftIO I__.newChan
               __new_tid <- I__.lift (I__.lift (spawnCOG __chan))
               let load = 0
               let vmId = (-1)
               let nodeId = Nothing
               let __c
                     = __cont{nebulaDC_load = load, nebulaDC_vmId = vmId,
                              nebulaDC_nodeId = nodeId,
                              nebulaDC_loc = return (COG (__chan, __new_tid))}
               __ioref <- I__.liftIO (I__.newIORef __c)
               let __obj = ObjectRef __ioref 0 __new_tid
               do __mvar <- I__.liftIO I__.newEmptyMVar
                  __hereCOG <- thisCOG
                  astate@(AState{aCounter = __counter}) <- I__.lift I__.get
                  I__.lift (I__.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __hereCOG __counter
                  I__.liftIO (I__.writeChan __chan (RunJob __obj __f (__init __obj)))
               do __mvar <- I__.liftIO I__.newEmptyMVar
                  __hereCOG <- thisCOG
                  astate@(AState{aCounter = __counter}) <- I__.lift I__.get
                  I__.lift (I__.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __hereCOG __counter
                  I__.liftIO (I__.writeChan __chan (RunJob __obj __f (__run __obj)))
               return __obj
        new_local __cont
          = 
            -- REMOVED:
            -- do let load = 0
            --    let vmId = (-1)
            --    let nodeId = Nothing
            --    __thisCOG@(_, __tid) <- thisCOG
            --    let __c
            --          = __cont{nebulaDC_load = load, nebulaDC_vmId = vmId,
            --                   nebulaDC_nodeId = nodeId, nebulaDC_loc = return __thisCOG}
            --    __ioref <- I__.liftIO (I__.newIORef __c)
            --    __astate@(AState{aCounter = __counter}) <- I__.lift I__.get
            --    I__.lift (I__.put (__astate{aCounter = __counter + 1}))
            --    let __obj = ObjectRef __ioref __counter __tid
            --    I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
            --      (__init __obj)
            --    I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
            --      (__run __obj)
            --    return __obj
            
            -- ADDED
            I__.error "Local DC objects cannot be created (new local)"
        __cog = nebulaDC_loc
        -- REMOVED: init block
        --__init this = return ()
        -- ADDED: init block
        __init this = do
          NebulaDC { nebulaDC_cpu = newCpu, nebulaDC_memory = newMem } <- readThis
          let maybeNewTempl = cloneSlaveTemplate myTempl newCpu newMem ("from-Pid-stub") myRpcServer myRpcProxy mySession
          (success, vmId, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_allocate (fromJust maybeNewTempl)))
          I__.when (not success) (I__.error "Allocating VM failed")
          set_nebulaDC_vmId vmId

set_nebulaDC_cpu :: Int -> ABS NebulaDC ()
set_nebulaDC_cpu v
  = do (AConf (ObjectRef ioref oid _) (COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       astate@(AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_cpu = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 0) om
       maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put astate{aSleepingO = om'})
 
set_nebulaDC_load :: Int -> ABS NebulaDC ()
set_nebulaDC_load v
  = do (AConf (ObjectRef ioref oid _) (COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       astate@(AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_load = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 1) om
       maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put astate{aSleepingO = om'})
 
set_nebulaDC_memory :: Int -> ABS NebulaDC ()
set_nebulaDC_memory v
  = do (AConf (ObjectRef ioref oid _) (COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       astate@(AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_memory = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 2) om
       maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put astate{aSleepingO = om'})
 
set_nebulaDC_nodeId :: Maybe NodeId -> ABS NebulaDC ()
set_nebulaDC_nodeId v
  = do (AConf (ObjectRef ioref oid _) (COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       astate@(AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_nodeId = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 3) om
       maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put astate{aSleepingO = om'})
 
set_nebulaDC_vmId :: Int -> ABS NebulaDC ()
set_nebulaDC_vmId v
  = do (AConf (ObjectRef ioref oid _) (COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       astate@(AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_vmId = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 4) om
       maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put astate{aSleepingO = om'})
 
instance Sub (ObjectRef NebulaDC) AnyObject where
        up = AnyObject
 
instance Sub (ObjectRef NebulaDC) IDC where
        up = IDC
 
-- REMOVED the stub
-- instance IDC_ NebulaDC where
--         shutdown this = return ()
--         getLoad this
--           = do (pure ((,,)) <*> ((/) <$> pure 1 <*> pure 2) <*>
--                   ((/) <$> pure 1 <*> pure 2)
--                   <*> ((/) <$> pure 1 <*> pure 2))

-- ADDED FROM HERE ON:
instance IDC_ NebulaDC where
        shutdown this = do
                   NebulaDC { nebulaDC_vmId = thisVmId } <- readThis
                   (True, _, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_action "cancel" thisVmId))
                   return ()
        getLoad this = do 
                   NebulaDC { nebulaDC_vmId = thisVmId } <- readThis
                   -- only works when called on this dc (thisDC)
                   -- otherwise raises a not-implemented-yet error
                   -- if (thisVmId == myVmId)
                      -- then do
                   (s1: s5: s15: _) <- I__.liftIO (I__.liftM words (readFile "/proc/loadavg"))
                   return (toRational (read s1 :: Double), toRational (read s5 :: Double), toRational (read s15 :: Double))
                   -- else I__.error "TODO: remote checking the load of the system is not implemented yet"

{-# NOINLINE myTyp #-}
{-# NOINLINE myCreatorPid #-}
{-# NOINLINE myRpcServer #-}
{-# NOINLINE mySession #-}
{-# NOINLINE myRpcProxy #-}
{-# NOINLINE myTempl #-}
[myTyp, myCreatorPid, myRpcServer, mySession, myRpcProxy, myTempl] = 
    let env = unsafePerformIO getEnvironment
    in map (\ v -> fromMaybe "" (lookup v env))   ["TYPE","FROM_PID","RPC_SERVER","SESSION","RPC_PROXY","VM_TEMPLATE"]

{-# NONLINE myVmId #-}
myVmId = fromMaybe 
         (-1)                   -- signals erroneous extraction of VM ID
         (templateVmId myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code

{-# NONLINE myVmId #-}
myVmIP = fromMaybe
         "127.0.0.1"
         (templateVmIP myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code

{-# NOINLINE thisDC #-}
thisDC = IDC (ObjectRef (unsafePerformIO (I__.newIORef (
                                                        NebulaDC{nebulaDC_loc = I__.undefined,
                                                                 nebulaDC_cpu = -1, -- TODO
                                                                 nebulaDC_memory = -1, -- TODO
                                                                 nebulaDC_load = -1, -- it's for sim purposes
                                                                 nebulaDC_nodeId = Nothing, -- TODO
                                                                 nebulaDC_vmId = myVmId}
                                                       ))) 
              (-2)                   -- a stub object-id of the DC object
              (nullProcessId (NodeId (encodeEndPointAddress myVmIP "8889" 0))))         -- no processid (ForwarderCOG ID) associated with the DC object

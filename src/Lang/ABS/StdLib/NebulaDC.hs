-- | An implementation of deployment components for OpenNebula clouds.
-- 
-- Since it is packaged in the ABS standard-library, the ABS user has to simply import it in an ABS program as:
-- 
-- > import NebulaDC
--
-- More about the open-source OpenNebula project at <http://opennebula.org>
{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module Lang.ABS.StdLib.NebulaDC where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import Lang.ABS.Runtime.Prim
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.StdLib
import OpenNebula hiding (main)
import Control.Distributed.Process (NodeId(..))
import Control.Distributed.Process.Internal.Types (nullProcessId) -- DC is under a null COG-process
--import Network.Transport.TCP (encodeEndPointAddress)
import System.IO (readFile)
import Data.List (words)
import Prelude (Double(..), (++), elem, fmap)
import Text.Read (read)
import Prelude (toRational)
import System.Environment (getEnvironment, getArgs, getProgName)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Control.Concurrent (myThreadId)
import Network.Transport.TCP (encodeEndPointAddress)
 
-- * Internals

data NebulaDC = NebulaDC{nebulaDC_loc :: (I__.Root_ o) => I__.ABS o I__.COG,
                         nebulaDC_cpu :: Int, nebulaDC_load :: Int, nebulaDC_memory :: Int,
                         nebulaDC_nodeId :: Maybe NodeId, nebulaDC_vmId :: Int}
__nebulaDC cpu memory
  = NebulaDC{nebulaDC_cpu = cpu, nebulaDC_memory = memory}
 
instance I__.Root_ NebulaDC where
        new __cont
          = do __chan <- I__.liftIO I__.newChan
               __new_tid <- I__.lift (I__.lift (I__.spawnCOG __chan))
               let load = 0
               let vmId = (-1)
               let nodeId = Nothing
               let __c
                     = __cont{nebulaDC_load = load, nebulaDC_vmId = vmId,
                              nebulaDC_nodeId = nodeId,
                              nebulaDC_loc = return (I__.COG (__chan, __new_tid))}
               __ioref <- I__.liftIO (I__.newIORef __c)
               let __obj = I__.ObjectRef __ioref 0 __new_tid
               do __mvar <- I__.liftIO I__.newEmptyMVar
                  __hereCOG <- I__.thisCOG
                  __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
                  I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
                  let __f = I__.FutureRef __mvar __hereCOG __counter
                  I__.liftIO (I__.writeChan __chan (I__.RunJob __obj __f (I__.__init __obj)))
               do __mvar <- I__.liftIO I__.newEmptyMVar
                  __hereCOG <- I__.thisCOG
                  __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
                  I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
                  let __f = I__.FutureRef __mvar __hereCOG __counter
                  I__.liftIO (I__.writeChan __chan (I__.RunJob __obj __f (I__.__run __obj)))
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
          NebulaDC { nebulaDC_cpu = newCpu, nebulaDC_memory = newMem } <- I__.readThis this
          myProgName <- I__.liftIO getProgName
          let maybeNewTempl = cloneSlaveTemplate myTempl newCpu newMem ("from-Pid-stub") myRpcServer myRpcProxy mySession myProgName
          (success, vmId, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_allocate (fromJust maybeNewTempl)))
          I__.when (not success) (I__.error "Allocating VM failed")
          set_nebulaDC_vmId vmId

set_nebulaDC_cpu :: Int -> I__.ABS NebulaDC ()
set_nebulaDC_cpu v
  = do (I__.AConf (I__.ObjectRef ioref oid _) (I__.COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       __astate@(I__.AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_cpu = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 0) om
       I__.maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put __astate{I__.aSleepingO = om'})
 
set_nebulaDC_load :: Int -> I__.ABS NebulaDC ()
set_nebulaDC_load v
  = do (I__.AConf (I__.ObjectRef ioref oid _) (I__.COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       __astate@(I__.AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_load = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 1) om
       I__.maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put __astate{I__.aSleepingO = om'})
 
set_nebulaDC_memory :: Int -> I__.ABS NebulaDC ()
set_nebulaDC_memory v
  = do (I__.AConf (I__.ObjectRef ioref oid _) (I__.COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       __astate@(I__.AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_memory = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 2) om
       I__.maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put __astate{I__.aSleepingO = om'})
 
set_nebulaDC_nodeId :: Maybe NodeId -> I__.ABS NebulaDC ()
set_nebulaDC_nodeId v
  = do (I__.AConf (I__.ObjectRef ioref oid _) (I__.COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       __astate@(I__.AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_nodeId = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 3) om
       I__.maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put __astate{I__.aSleepingO = om'})
 
set_nebulaDC_vmId :: Int -> I__.ABS NebulaDC ()
set_nebulaDC_vmId v
  = do (I__.AConf (I__.ObjectRef ioref oid _) (I__.COG (thisChan, _))) <- I__.lift
                                                          I__.ask
       __astate@(I__.AState _ om _) <- I__.lift I__.get
       I__.liftIO (I__.modifyIORef' ioref (\ c -> c{nebulaDC_vmId = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (oid, 4) om
       I__.maybe (return ())
         (\ woken -> I__.liftIO (I__.writeList2Chan thisChan woken))
         maybeWoken
       I__.lift (I__.put __astate{I__.aSleepingO = om'})
 
instance I__.Sub (I__.Obj NebulaDC) I__.Root where
        up = I__.Root
 
instance I__.Sub (I__.Obj NebulaDC) IDC where
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
                   NebulaDC { nebulaDC_vmId = thisVmId } <- I__.readThis this
                   (True, _, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_action "cancel" thisVmId))
                   return ()
        getLoad this = do 
                   NebulaDC { nebulaDC_vmId = thisVmId } <- I__.readThis this
                   -- only works when called on this dc (thisDC)
                   -- otherwise raises a not-implemented-yet error
                   -- if (thisVmId == myVmId)
                      -- then do
                   (s1: s5: s15: _) <- I__.liftIO (words <$> (readFile "/proc/loadavg"))
                   return (toRational (read s1 :: Double), toRational (read s5 :: Double), toRational (read s15 :: Double))
                   -- else I__.error "TODO: remote checking the load of the system is not implemented yet"

{-# NOINLINE myTyp #-}
{-# NOINLINE myCreatorPid #-}
{-# NOINLINE myRpcServer #-}
{-# NOINLINE mySession #-}
{-# NOINLINE myRpcProxy #-}
{-# NOINLINE myTempl #-}
{-# NOINLINE myArgs #-}
(myTyp:myCreatorPid:myRpcServer:mySession:myRpcProxy:myTempl:myArgs) = 
    let env = unsafePerformIO getEnvironment
        args = unsafePerformIO getArgs
    in fmap (\ v -> fromMaybe "" (lookup v env))   ["TYPE","FROM_PID","RPC_SERVER","SESSION","RPC_PROXY","VM_TEMPLATE"] Prelude.++ args

{-# NONLINE myVmId #-}
myVmId = fromMaybe 
         (-1)                   -- signals erroneous extraction of VM ID
         (templateVmId myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code

{-# NONLINE myVmIP #-}
myVmIP = if "--distributed" `Prelude.elem` myArgs
         then fromMaybe
                  "127.0.0.1"
                  (templateVmIP myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code
         else "127.0.0.1"

{-# NOINLINE thisDC #-}
thisDC = IDC (I__.ObjectRef (unsafePerformIO (I__.newIORef (
                                                        NebulaDC{nebulaDC_loc = I__.undefined,
                                                                 nebulaDC_cpu = -1, -- TODO
                                                                 nebulaDC_memory = -1, -- TODO
                                                                 nebulaDC_load = -1, -- it's for sim purposes
                                                                 nebulaDC_nodeId = Nothing, -- TODO
                                                                 nebulaDC_vmId = myVmId}
                                                       ))) 
              (-2)                   -- a stub object-id of the DC object
              (nullProcessId (NodeId (encodeEndPointAddress myVmIP "9000" 0))))         -- no processid (ForwarderCOG ID) associated with the DC object

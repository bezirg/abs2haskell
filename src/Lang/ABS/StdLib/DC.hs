{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}

-- This file is created by the stub file DC.abs and filled in
-- to connect to OpenNebula

module Lang.ABS.StdLib.DC where

import qualified Control.Monad.Trans.RWS as RWS
import Lang.ABS.Runtime
import Lang.ABS.Compiler.Include
import Lang.ABS.StdLib.Prelude

-- added
import OpenNebula
import Control.Distributed.Process (NodeId (..), ProcessId (..)) 
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.List (words)
import Prelude (readFile, toRational, read, Double)
import Data.Maybe (fromJust)

[typ, creatorPid, server, session, proxy, templ] = unsafePerformIO getArgs -- the args passed to this slave by OpenNebula framework

myVmId = fromJust (extractVmId templ)

slaveTemplateWithContext :: ProcessId -> Int -> Int -> String
slaveTemplateWithContext myPid new_cpu new_memory = showPlain (toPlain (fromJust 
                                                                        (contextualize (U.toString (encode myPid)) new_cpu new_memory =<< inheritXmlTemplate templ)))

class (Object__ a) => IDC_ a where
        shutdown :: IDC -> ABS a ()
        getLoad :: IDC -> ABS a (Rat, Rat, Rat)
 
data IDC = forall a . (IDC_ a) => IDC (ObjectRef a)
 
instance Sub IDC IDC where
        up x = x
 
instance Sub (ObjectRef Null) IDC where
        up = IDC
 
instance Sub IDC AnyObject where
        up (IDC a) = AnyObject a
 
instance IDC_ Null where
        shutdown
          = error
              "this should not happen. report the program to the compiler developers"
        getLoad
          = error
              "this should not happen. report the program to the compiler developers"
__eqIDC (IDC NullRef) (IDC NullRef) = True
__eqIDC (IDC (ObjectRef _ tid1 id1)) (IDC (ObjectRef _ tid2 id2))
  = (tid1 == tid2) && (id1 == id2)
__eqIDC _ _ = False
 
instance Eq IDC where
        (==) = __eqIDC
shutdown_sync (__wrapped@(IDC __obj@(ObjectRef __ioref _ _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- __cog __obj1
       when (not (__hereCOG == otherCOG))
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (shutdown __wrapped)
shutdown_sync (IDC NullRef) = error "sync call to null"
shutdown_async (__wrapped@(IDC __obj@(ObjectRef __ioref _ _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       (__chan, _) <- __cog __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aCOG = __cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __cog __counter
       lift
         (lift (writeChan __chan (RunJob __obj __f (shutdown __wrapped))))
       return __f
shutdown_async (IDC NullRef) = error "async call to null"
getLoad_sync (__wrapped@(IDC __obj@(ObjectRef __ioref _ _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- __cog __obj1
       when (not (__hereCOG == otherCOG))
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (getLoad __wrapped)
getLoad_sync (IDC NullRef) = error "sync call to null"
getLoad_async (__wrapped@(IDC __obj@(ObjectRef __ioref _ _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       (__chan, _) <- __cog __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aCOG = __cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __cog __counter
       lift
         (lift (writeChan __chan (RunJob __obj __f (getLoad __wrapped))))
       return __f
getLoad_async (IDC NullRef) = error "async call to null"
 
data DC = DC{dC_loc :: (Object__ o) => ABS o COG,
             dC_cpu :: Int, dC_load :: Int, dC_memory :: Int,
             dC_nodeId :: Maybe NodeId, dC_vmId :: Int}
__dC cpu memory = DC{dC_cpu = cpu, dC_memory = memory}
 
instance Object__ DC where
        new __cont
          = do __chan <- lift (lift newChan)
               __new_tid <- lift (lift (spawnCOG __chan))
               let __load = 0
               let __vmId = (-1)
               let __nodeId = Nothing
               let __c
                     = __cont{dC_load = __load, dC_vmId = __vmId, dC_nodeId = __nodeId,
                              dC_loc = return (__chan, __new_tid)}
               __ioref <- lift (lift (newIORef __c))
               let __obj = ObjectRef __ioref 0 __new_tid
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aCOG = __cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__init ( __obj)))))
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aCOG = __cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__run (__obj)))))
               return __obj
        new_local __cont
          = do let __load = 0
               let __vmId = (-1)
               let __nodeId = Nothing
               __thisCOG@(_, __tid) <- thisCOG
               let __c
                     = __cont{dC_load = __load, dC_vmId = __vmId, dC_nodeId = __nodeId,
                              dC_loc = return __thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter __tid
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__init ( __obj))
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__run ( __obj))
               return __obj
        __cog = dC_loc
        __init this = do
                       DC { dC_cpu = new_cpu, dC_memory = new_memory } <- readThis
                       let new_templ = slaveTemplateWithContext undefined new_cpu new_memory
                       (success, vmId, errCode) <- lift (lift (xmlrpc server session (Just proxy) (vm_allocate (slaveTemplateWithContext undefined new_cpu new_memory))))
                       when (not success) (error "Allocating VM failed")
                       set_dC_vmId vmId

set_dC_cpu :: Int -> ABS DC ()
set_dC_cpu v
  = do (AConf (ObjectRef ioref oid _) (thisChan, _)) <- lift
                                                             RWS.ask
       astate@(AState _ om _) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{dC_cpu = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (oid, 0) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisChan woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_dC_load :: Int -> ABS DC ()
set_dC_load v
  = do (AConf (ObjectRef ioref oid _) (thisChan, _)) <- lift
                                                             RWS.ask
       astate@(AState _ om _) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{dC_load = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (oid, 1) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisChan woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_dC_memory :: Int -> ABS DC ()
set_dC_memory v
  = do (AConf (ObjectRef ioref oid _) (thisChan, _)) <- lift
                                                             RWS.ask
       astate@(AState _ om _) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{dC_memory = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (oid, 2) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisChan woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_dC_nodeId :: Maybe NodeId -> ABS DC ()
set_dC_nodeId v
  = do (AConf (ObjectRef ioref oid _) (thisChan, _)) <- lift
                                                             RWS.ask
       astate@(AState _ om _) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{dC_nodeId = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (oid, 3) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisChan woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_dC_vmId :: Int -> ABS DC ()
set_dC_vmId v
  = do (AConf (ObjectRef ioref oid _) (thisChan, _)) <- lift
                                                             RWS.ask
       astate@(AState _ om _) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{dC_vmId = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (oid, 4) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisChan woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
instance IDC_ DC where
        shutdown this = do
                   DC { dC_vmId = thisVmId } <- readThis
                   (True, _, errCode) <- lift (lift (xmlrpc server session (Just proxy) (vm_action "cancel" thisVmId)))
                   return ()
        getLoad this = do 
                   DC { dC_vmId = thisVmId } <- readThis
                   -- only works when called on this dc (whereami)
                   -- otherwise raises a not-implemented-yet error
                   if (thisVmId == myVmId)
                      then do
                        (s1: s5: s15: _) <- lift (lift (liftM words (readFile "/proc/loadavg")))
                        return (toRational (read s1 :: Double), toRational (read s5 :: Double), toRational (read s15 :: Double))
                      else error "TODO: remote checking the load of the system is not implemented yet"

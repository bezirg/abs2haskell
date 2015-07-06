{-# LANGUAGE TemplateHaskell #-}
-- | The core of the ABS-Haskell runtime execution. 
--
-- This module implements the execution-logic of 
-- 
-- 1. a main COG with 'main-is' action
-- 2. a local or distributed COG (depending on if --distributed is enabled) with 'spawnCOG' action

module Lang.ABS.Runtime.Core 
    (spawnCOG
    ,main_is
    ,updateWoken
    ,spawnCOG__static
    ) where

import Lang.ABS.Runtime.Base
import Lang.ABS.Runtime.Conf
import qualified Lang.ABS.StdLib.DC as DC (__remoteTable) -- the remotable methods table of DC
import Data.List (foldl', find, splitAt)
import qualified Data.Map.Strict as M (Map, empty, insertWith, updateLookupWithKey, update, findWithDefault, insertLookupWithKey)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import qualified Control.Monad.Trans.State.Strict as S (runStateT, modify)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)
import Control.Exception (throw)
import Control.Monad.Catch (catchAll)
import qualified Control.Distributed.Process as CH
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters, socketToEndPoint, encodeEndPointAddress)
import Network.Transport (newEndPoint, address)
import Network.Socket (withSocketsDo)
import Network.Info -- querying NIC IPs
import System.Environment (getEnv)
import System.IO.Error (tryIOError)
import qualified Data.Binary as Bin (decode)
import Data.String (fromString)
import Control.Monad (when, foldM, liftM)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Internal.Types (nullProcessId) -- DC is under a null COG-process
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Distributed.Process.Closure
import Data.IORef

-- NOTE: the loops must be tail-recursive (not necessarily syntactically tail-recursive) to avoid stack leaks

-- | (only applied when --distributed). This is a so-called cog-forwarder which is an extra (lightweight) thread
-- that accompanies the COG thread and acts as the mediator from the outside-world to the local world.
--
-- It listens in a remote queue (mailbox) and forwards any messages to the COG's local queue ("Chan")
fwd_cog :: COG              -- ^ the _local-only_ channel (queue) of the COG process
        -> CH.Process ()          -- ^ is itself a CH process
fwd_cog thisCOG@(COG(c,pid)) = do
  return ()
  j <- CH.expect
  case j of
    WakeupSignal v cog i -> do
              -- lookup future foreign table
              -- putMVar to local future
              CH.liftIO $ writeChan c j
    RemotJob (ObjectRef _ (COG (_,pid)) oid) f clos -> do
              -- lookup object foreign table
              unclos <- CH.unClosure clos
              CH.liftIO $ writeChan c (LocalJob (ObjectRef undefined (COG (c,pid)) oid) f unclos)
    InitJob obj -> do
             __ioref <- CH.liftIO $ newIORef obj
             let __obj = ObjectRef __ioref thisCOG 0
             CH.liftIO $ writeChan c (LocalJob __obj NullFutureRef (__init __obj))
    MachineUp remoteAddress -> do
              CH.liftIO $ print "Machines connected"
              myNid <- fmap CH.processNodeId CH.getSelfPid
              let myNodeAddress = CH.nodeAddress myNid
              Right _ <- CH.liftIO $ socketToEndPoint myNodeAddress remoteAddress
                                    True -- reuseaddr
                                    (Just 10000000) -- 10secs timeout to establish connection
              return ()
    _ -> error "this should not happen"
  fwd_cog thisCOG

-- | Each COG is a thread or a process
spawnCOG :: CH.Process COG -- ^ it returns the created COG-thread ProcessId. This is used to update the location of the 1st created object
spawnCOG = do
  c <- CH.liftIO newChan
  if (distributed conf)     -- DISTRIBUTED (default) (1 COG Process + 1 Forwarder Process)
    then do
      fwdPid <- CH.spawnLocal (CH.getSelfPid >>= \ pid -> fwd_cog (COG (c,pid))) -- Proc-1  is the forwarder cog. It's pid is 1st part of the COG's id
      _ <- CH.spawnLocal $ do  -- Proc-2 is the local cog thread. It's job channel is the 2nd part of the COG's id
            -- each COG holds two tables:
        let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
        let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
        -- start the loop of the COG
        loop c fwdPid sleepingOnFut sleepingOnAttr 1
      return $ COG (c,fwdPid)
  -- LOCAL-ONLY (multicore) (1 COG Process)
    else do
      pid <- CH.spawnLocal $ do  -- Proc-2 is the local cog thread. It's job channel is the 2nd part of the COG's id
           -- each COG holds two tables:
           let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
           let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
           -- start the loop of the COG
           myPid <- CH.getSelfPid
           loop c myPid sleepingOnFut sleepingOnAttr 1
      return $ COG (c, pid)
    where
      -- COG loop definition
      loop c pid sleepingOnFut sleepingOnAttr counter = do
        -- on each iteration, it listens for next job on the input job queue
        nextJob <- CH.liftIO $ readChan c
        case nextJob of
          -- wake signals are transmitted (implicitly) from a COG to another COG to wakeup some of the latter's sleeping process
          WakeupSignal v cog i -> do
             let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepingOnFut
             -- put the woken back to the enabled queue
             sleepingOnAttr' <- maybe (return sleepingOnAttr) (CH.liftIO . updateWoken c sleepingOnAttr) maybeWoken
             loop c pid sleepingOnFut'' sleepingOnAttr' counter
          -- run-jobs are issued by the user *explicitly by async method-calls* to do *ACTUAL ABS COMPUTE-WORK*
          LocalJob obj fut coroutine -> do
             (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''})) <- S.runStateT (do
              -- the cog catches any exception and lazily records it into the future-box (mvar)
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 when (traceExceptions conf) $ 
                                                      CH.liftIO $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx) 
              case p of
                    -- the job of the callee finished, send a wakeup signal to remote cog that "nourishes" the sleeping caller-process
                    Right fin -> do
                       case fut of
                         (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> do
                           CH.liftIO $ putMVar mvar fin
                           if ftid /= pid -- remote job finished, wakeup the remote cog
                              then do
                                CH.liftIO $ writeChan fcog (WakeupSignal fin cog fid)
                                return sleepingOnFut
                              else do
                                -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                -- because it adds an extra iteration
                                let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,fid) sleepingOnFut
                                -- put the woken back to the enabled queue
                                maybe (return ()) (\ woken -> do
                                                   sleepingOnAttr' <- CH.liftIO $ updateWoken c sleepingOnAttr woken
                                                   S.modify (\ astate -> astate {aSleepingO = sleepingOnAttr'})) maybeWoken
                                return sleepingOnFut'
                         NullFutureRef -> return sleepingOnFut
                    -- the process deliberately suspended (by calling suspend)
                    Left (Yield S cont) -> do
                           CH.liftIO $ writeChan c (LocalJob obj fut cont) -- reschedule its continuation at the end of the job-queue
                           return sleepingOnFut
                    -- the process deliberately decided to await on a *LOCAL* future to finish (by calling await f?;)
                    Left (Yield (FL f@(FutureRef _ cog i)) cont) -> do
                           return (M.insertWith (++) (cog,i) [(LocalJob obj fut cont, Nothing)] sleepingOnFut) -- update sleepingf-table
                    -- the process deliberately decided to await on a *FIELD* future to finish (by calling await f?;)
                    Left (Yield (FF f@(FutureRef _ cog i) fid) cont) -> do
                       -- updating both suspended tables
                       let ObjectRef _ _ oid = obj
                       let lengthOnAttr = length $ M.findWithDefault [] (oid, fid) sleepingOnAttr
                       let (mFutEntry, sleepingOnFut') = M.insertLookupWithKey (\ _k p n -> p ++ n) (cog,i) [(LocalJob obj fut cont, Just ((oid,fid),lengthOnAttr))] sleepingOnFut

                       
                       let lengthOnFut = maybe 0 length mFutEntry
                       let sleepingOnAttr' = M.insertWith (++) (oid,fid) [(LocalJob obj fut cont, Just ((cog,i),lengthOnFut))] sleepingOnAttr
                       S.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                       return sleepingOnFut' -- update sleepingf-table                          
                    -- the process deliberately decided to await on a this.field to change (by calling await (this.field==v);)
                    Left (Yield (A o@(ObjectRef _ _ oid)  fields) cont) -> do
                           -- update sleepingo-table
                           let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) (oid,i) [(LocalJob obj fut cont,Nothing)] m) sleepingOnAttr fields
                           S.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                           return sleepingOnFut
                                                    )  (AState {aCounter = counter,
                                                                aSleepingO = sleepingOnAttr})
             loop c pid sleepingOnFut'' sleepingOnAttr'' counter''


-- | ABS main-block thread (COG-like thread)
main_is :: (Obj Null -> ABS ()) -- ^ a main-block monadic action (a fully-applied method with a null this-context that returns "Unit")
        -> CH.RemoteTable        -- ^ this is a _remotely-shared_ table of pointers to __remotable__ methods (methods that can be called remotely)
        -> IO ()                  -- ^ returns void. It is the main procedure of a compiled ABS application.
main_is mainABS outsideRemoteTable = withSocketsDo $ do -- for windows fix
  let port' = maybe "9000" show (port conf)
  print port'
  -- DISTRIBUTED (1 COG Process + 1 Forwarder Process)
  if (distributed conf || isJust (port conf))
    then do
      nics <- getNetworkInterfaces -- which is the lan IP? it is under eth0 nic
      let myIp = maybe 
             (error "An outside network interface was not found.")
             (show . ipv4) $ find (\ nic -> name nic == "wlp2s0") nics -- otherwise, eth0's IP
      Right trans <- createTransport myIp port' defaultTCPParameters
      myLocalNode <- newLocalNode trans (DC.__remoteTable outsideRemoteTable) -- new my-node
      Right ep <- newEndPoint trans -- our outside point, HEAVYWEIGHT OPERATION

      c <- newChan            -- sharing in-memory channel between Forwarder and Cog
      fwdPid <- forkProcess myLocalNode (CH.getSelfPid >>= \ pid -> fwd_cog (COG (c,pid))) -- start the Forwarder process
      print "created node"
      -- Was this Node-VM created by another (remote) VM? then connect with this *CREATOR* node and answer back with an ack
      maybeCreatorPidStr <- tryIOError (getEnv "FROM_PID" )
      case maybeCreatorPidStr of
        Left _ex -> do -- no creator, this is the START-SYSTEM and runs MAIN-BLOCK
           print "no creator"
           writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c,nullProcessId $ CH.NodeId $ encodeEndPointAddress myIp port' 0)) undefined)) -- send the Main Block as the 1st created process
           runProcess myLocalNode (loop c fwdPid M.empty M.empty 1) -- start the COG process
        Right "" -> do -- no creator, this is the START-SYSTEM and runs MAIN-BLOCK
           print "no creator"
           writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c,nullProcessId $ CH.NodeId $ encodeEndPointAddress myIp port' 0)) undefined)) -- send the Main Block as the 1st created process
           runProcess myLocalNode (loop c fwdPid M.empty M.empty 1) -- start the COG process
        Right creatorPidStr -> do -- there is a Creator PID; extract its NodeId
                    -- try to establish TCP connection with the creator
                    print ("creator str" ++ creatorPidStr)
                    let creatorPid = Bin.decode (B64.decodeLenient (C8.pack creatorPidStr)) :: CH.ProcessId
                    let creatorNodeAddress = CH.nodeAddress (CH.processNodeId creatorPid)
                    print ("with creator" ++ show creatorPid)
                    let myNodeAddress = address ep
                    Right _ <- socketToEndPoint myNodeAddress creatorNodeAddress
                              True -- reuseaddr
                              (Just 10000000) -- 10secs timeout to establish connection
                    runProcess myLocalNode (do
                                             CH.liftIO $ print "sent machine-up"
                                             CH.send creatorPid (MachineUp myNodeAddress) -- send ack to creatorPid
                                             loop c fwdPid M.empty M.empty 1) -- start the COG process


  -- LOCAL-ONLY (DEFAULT) (multicore) (1 COG Process)
    else do
      let myIp = "127.0.0.1" -- a placeholder for identifying the local node. No outside connection will be created.
      Right trans <- createTransport myIp port' defaultTCPParameters
      myLocalNode <- newLocalNode trans initRemoteTable -- not needed to create the remote-table

      c <- newChan               -- in-memory channel
      writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c,nullProcessId $ CH.NodeId $ encodeEndPointAddress myIp port' 0)) undefined)) -- send the Main Block as the 1st created process
      runProcess myLocalNode (CH.getSelfPid >>= \ pid -> loop c pid M.empty M.empty 1)


   where
     loop c pid sleepingOnFut sleepingOnAttr counter = do
       nextJob <- CH.liftIO $ readChan c
       case nextJob of
         WakeupSignal v cog i -> do
           let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepingOnFut
           sleepingOnAttr' <- maybe (return sleepingOnAttr) (CH.liftIO . updateWoken c sleepingOnAttr) maybeWoken -- put the woken back to the enabled queue
           loop c pid sleepingOnFut'' sleepingOnAttr' counter
         LocalJob obj fut coroutine -> do
           (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''})) <- S.runStateT (do
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 when (traceExceptions conf) $ 
                                                      CH.liftIO $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx) 
              case p of
                Right fin -> case fut of
                              (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> do
                                 CH.liftIO $ putMVar mvar fin
                                 if ftid /= pid
                                   then do -- remote job finished, wakeup the remote cog
                                     CH.liftIO $ writeChan fcog (WakeupSignal fin cog fid)
                                     return sleepingOnFut
                                   else do
                                     -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                     -- because it adds an extra iteration
                                     let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,fid) sleepingOnFut
                                     -- put the woken back to the enabled queue
                                     maybe (return ()) (\ woken -> do
                                                         sleepingOnAttr' <- CH.liftIO $ updateWoken c sleepingOnAttr woken
                                                         S.modify (\ astate -> astate {aSleepingO = sleepingOnAttr'})) maybeWoken
                                     return sleepingOnFut'
                              NullFutureRef -> CH.liftIO $ do
                                         if (distributed conf || isJust (port conf) || keepAlive conf)
                                           then do
                                             print $ "main finished" ++ show pid
                                             return sleepingOnFut
                                           -- exit early otherwise
                                           else do
                                            print "Main COG has exited with success"
                                            exitSuccess
                Left (Yield S cont) -> do
                       CH.liftIO $ writeChan c (LocalJob obj fut cont) 
                       return sleepingOnFut
                Left (Yield (FL f@(FutureRef _ cog i)) cont) -> do
                       return (M.insertWith (++) (cog,i) [(LocalJob obj fut cont, Nothing)] sleepingOnFut)
                Left (Yield (FF f@(FutureRef _ cog i) fid) cont) -> do
                       -- updating both suspended tables
                       let ObjectRef _ _ oid = obj
                       let lengthOnAttr = length $ M.findWithDefault [] (oid, fid) sleepingOnAttr
                       let (mFutEntry, sleepingOnFut') = M.insertLookupWithKey (\ _k p n -> p ++ n) (cog,i) [(LocalJob obj fut cont, Just ((oid,fid),lengthOnAttr))] sleepingOnFut

                       
                       let lengthOnFut = maybe 0 length mFutEntry
                       let sleepingOnAttr' = M.insertWith (++) (oid,fid) [(LocalJob obj fut cont, Just ((cog,i),lengthOnFut))] sleepingOnAttr
                       S.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                       return sleepingOnFut' -- update sleepingf-table                          
                Left (Yield (A o@(ObjectRef _ _ oid) fields) cont) -> do
                       let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) (oid,i) [(LocalJob obj fut cont,Nothing)] m) sleepingOnAttr fields
                       S.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                       return sleepingOnFut
                                              ) (AState {aCounter = counter,
                                                         aSleepingO = sleepingOnAttr})
           loop c pid sleepingOnFut'' sleepingOnAttr'' counter''



updateWoken :: Ord k => Chan a -> M.Map k [a1] -> [(a, Maybe (k, Int))] -> IO (M.Map k [a1])
updateWoken ch m ls = liftM fst $ foldM (\ (m, alreadyDeleted) (j, mo) -> do
                                                   writeChan ch j
                                                   return $ case mo of
                                                              Nothing -> (m, alreadyDeleted)
                                                              Just (k,i) -> (M.update (\ l -> if length l == 1
                                                                                                  then Nothing
                                                                                                  else Just $ l `deleteIndex` (i-alreadyDeleted)) 
                                                                                  k m
                                                                                 , alreadyDeleted+1)
                                                ) (m,0) ls
                              where
                                deleteIndex :: [a] -> Int -> [a]
                                deleteIndex l i = let (left,right) = Data.List.splitAt (i-1) l
                                                  in left ++ tail right


remotable ['spawnCOG]

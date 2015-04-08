module Lang.ABS.Runtime.Core 
    (spawnCOG
    ,main_is
    ) where

import Lang.ABS.Runtime.Base
import qualified Lang.ABS.StdLib.DC as DC (__remoteTable) -- the remotable methods table of DC
import Data.List (foldl', find)
import qualified Data.Map.Strict as M (empty, insertWith, updateLookupWithKey)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, writeList2Chan, Chan)
import qualified Control.Monad.Trans.RWS as RWS (runRWST, modify)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)
import Control.Exception (throw)
import Control.Monad.Catch (catchAll)
import qualified Control.Distributed.Process as CH
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters, socketToEndPoint)
import Network.Transport (newEndPoint, address)
import Network.Info -- querying NIC IPs
import System.Environment (getEnv)
import System.IO.Error (tryIOError)
import qualified Data.Binary as Bin (decode)
import Data.String (fromString)
import qualified Lang.ABS.StdLib.DC as DC (__remoteTable)
import System.Environment (getArgs)
import Control.Monad (when)

-- NOTE: the loops must be tail-recursive (not necessarily syntactically tail-recursive) to avoid stack leaks

fwd_cog :: Chan Job -> CH.Process ()
fwd_cog c = do
  AnyFuture j <- CH.expect :: CH.Process AnyFuture
  -- TODO: putMVar to local future
  CH.liftIO $ writeChan c (WakeupSignal j)

spawnCOG :: Chan Job -> CH.Process CH.ProcessId -- it returns a ProcessId to update the new (1st object in the COG) location value.
spawnCOG c = do
  args <- CH.liftIO getArgs
  if "--distributed" `elem` args     -- DISTRIBUTED (default) (1 COG Process + 1 Forwarder Process)
    then do
      fwdPid <- CH.spawnLocal (fwd_cog c) -- Proc-1  is the forwarder cog. It's pid is 1st part of the COG's id
      _ <- CH.spawnLocal $ do  -- Proc-2 is the local cog thread. It's job channel is the 2nd part of the COG's id
            -- each COG holds two tables:
        let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
        let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
        -- start the loop of the COG
        loop fwdPid sleepingOnFut sleepingOnAttr 1
      return fwdPid
  -- LOCAL-ONLY (multicore) (1 COG Process)
    else CH.spawnLocal $ do  -- Proc-2 is the local cog thread. It's job channel is the 2nd part of the COG's id
           -- each COG holds two tables:
           let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
           let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
           -- start the loop of the COG
           myPid <- CH.getSelfPid
           loop myPid sleepingOnFut sleepingOnAttr 1
    where
      -- COG loop definition
      loop pid sleepingOnFut sleepingOnAttr counter = do
        -- on each iteration, it listens for next job on the input job queue
        nextJob <- CH.liftIO $ readChan c
        case nextJob of
          -- wake signals are transmitted (implicitly) from a COG to another COG to wakeup some of the latter's sleeping process
          WakeupSignal f -> do
             let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingOnFut
             maybe (return ()) (\ woken -> CH.liftIO $ writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
             loop pid sleepingOnFut'' sleepingOnAttr counter
          -- run-jobs are issued by the user *explicitly by async method-calls* to do *ACTUAL ABS COMPUTE-WORK*
          RunJob obj fut@(FutureRef mvar (COG (fcog, ftid)) _) coroutine -> do
             (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''}), _) <- RWS.runRWST (do
              -- the cog catches any exception and lazily records it into the future-box (mvar)
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 args <- CH.liftIO getArgs
                                                 when ("--trace-exceptions" `elem` args) $ 
                                                      CH.liftIO $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx) 
              case p of
                    -- the job of the callee finished, send a wakeup signal to remote cog that "nourishes" the sleeping caller-process
                    Right fin -> do
                           CH.liftIO $ putMVar mvar fin
                           if ftid /= pid -- remote job finished, wakeup the remote cog
                              then do
                                CH.liftIO $ writeChan fcog (WakeupSignal fut)
                                return sleepingOnFut
                              else do
                                -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                -- because it adds an extra iteration
                                let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingOnFut
                                -- put the woken back to the enabled queue
                                maybe (return ()) (\ woken -> CH.liftIO $ writeList2Chan c woken) maybeWoken
                                return sleepingOnFut'
                    -- the process deliberately suspended (by calling suspend)
                    Left (Yield S cont) -> do
                           CH.liftIO $ writeChan c (RunJob obj fut cont) -- reschedule its continuation at the end of the job-queue
                           return sleepingOnFut
                    -- the process deliberately decided to await on a future to finish (by calling await f?;)
                    Left (Yield (F f) cont) -> do
                           return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingOnFut) -- update sleepingf-table
                    -- the process deliberately decided to await on a this.field to change (by calling await (this.field==v);)
                    Left (Yield (T o@(ObjectRef _ oid _)  fields) cont) -> do
                           -- update sleepingo-table
                           let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) (oid,i) [RunJob obj fut cont] m) sleepingOnAttr fields
                           RWS.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                           return sleepingOnFut
                                                    ) (AConf {aThis = obj, 
                                                              aCOG = COG (c, pid)
                                                             })
                                                                     (AState {aCounter = counter,
                                                                              aSleepingO = sleepingOnAttr})
             loop pid sleepingOnFut'' sleepingOnAttr'' counter''


-- ABS Main-block thread (COG-like thread)
main_is :: ABS Null () -> CH.RemoteTable -> IO () 
main_is mainABS outsideRemoteTable = do
  args <- getArgs

  -- DISTRIBUTED (1 COG Process + 1 Forwarder Process)
  if "--distributed" `elem` args
    then do
      -- which is the lan IP? it is under eth0 nic
      nics <- getNetworkInterfaces
      let myIp = maybe 
             (error "An outside network interface was not found.")
             (show . ipv4) $ find (\ nic -> name nic == "eth0") nics -- otherwise, eth0's IP
      Right trans <- createTransport myIp "8889" defaultTCPParameters
      myLocalNode <- newLocalNode trans (DC.__remoteTable outsideRemoteTable) -- new my-node
      Right ep <- newEndPoint trans -- our outside point, HEAVYWEIGHT OPERATION

      -- Was this Node-VM created by another (remote) VM? then connect with this *CREATOR* node and answer back with an ack
      maybeCreatorPidStr <- tryIOError (getEnv "FROM_PID" )
      case maybeCreatorPidStr of
        Left _ex -> return ()       -- no creator, this is the START-SYSTEM
        Right "" -> return ()      -- no creator, this is the START-SYSTEM
        Right creatorPidStr -> do -- there is a Creator PID; extract its NodeId
                    -- try to establish TCP connection with the creator
                    let creatorPid = Bin.decode (fromString creatorPidStr) :: CH.ProcessId
                    let creatorNodeAddress = CH.nodeAddress (CH.processNodeId creatorPid)
                    let myNodeAddress = address ep
                    Right _ <- socketToEndPoint myNodeAddress creatorNodeAddress
                              True -- reuseaddr
                              (Just 10000000) -- 10secs timeout to establish connection
                    return () -- TODO: send ack to creatorPid
      c <- newChan            -- sharing in-memory channel between Forwarder and Cog
      fwdPid <- forkProcess myLocalNode (fwd_cog c)
      runProcess myLocalNode (loop c fwdPid M.empty M.empty 1)

  -- LOCAL-ONLY (DEFAULT) (multicore) (1 COG Process)
    else do
      let myIp = "127.0.0.1" -- a placeholder for identifying the local node. No outside connection will be created.
      Right trans <- createTransport myIp "8889" defaultTCPParameters
      myLocalNode <- newLocalNode trans initRemoteTable -- not needed to create the remote-table

      c <- newChan               -- in-memory channel
      writeChan c (RunJob (error "not this at top-level") TopRef mainABS) -- send the Main Block as the 1st created process
      runProcess myLocalNode (CH.getSelfPid >>= \ pid -> loop c pid M.empty M.empty 1)


   where
     loop c pid sleepingOnFut sleepingOnAttr counter = do
       nextJob <- CH.liftIO $ readChan c
       case nextJob of
         WakeupSignal f -> do
           let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingOnFut
           maybe (return ()) (\ woken -> CH.liftIO (writeList2Chan c woken)) maybeWoken -- put the woken back to the enabled queue
           loop c pid sleepingOnFut'' sleepingOnAttr counter
         RunJob obj fut coroutine -> do
           (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''}), _) <- RWS.runRWST (do
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 args <- CH.liftIO getArgs
                                                 when ("--trace-exceptions" `elem` args) $ 
                                                      CH.liftIO $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx) 
              case p of
                Right fin -> case fut of
                              (FutureRef mvar (COG (fcog, ftid)) _) -> do
                                 CH.liftIO $ putMVar mvar fin
                                 if ftid /= pid
                                   then do -- remote job finished, wakeup the remote cog
                                     CH.liftIO $ writeChan fcog (WakeupSignal fut)
                                     return sleepingOnFut
                                   else do
                                     -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                     -- because it adds an extra iteration
                                     let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingOnFut
                                     -- put the woken back to the enabled queue
                                     maybe (return ()) (\ woken -> CH.liftIO $ writeList2Chan c woken) maybeWoken
                                     return sleepingOnFut'
                              TopRef -> CH.liftIO $ do
                                         args <- getArgs
                                         if ("--distributed" `elem` args || "--keep-alive" `elem` args)
                                           then return sleepingOnFut
                                           -- exit early otherwise
                                           else do
                                            print "Main COG has exited with success"
                                            exitSuccess
                Left (Yield S cont) -> do
                       CH.liftIO $ writeChan c (RunJob obj fut cont) 
                       return sleepingOnFut
                Left (Yield (F f) cont) -> do
                       return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingOnFut)
                Left (Yield (T o@(ObjectRef _ oid _) fields) cont) -> do
                       let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) (oid,i) [RunJob obj fut cont] m) sleepingOnAttr fields
                       RWS.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                       return sleepingOnFut
                                              ) (AConf {aThis = obj, 
                                                        aCOG = COG (c, pid)
                                                       }) (AState {aCounter = counter,
                                                                   aSleepingO = sleepingOnAttr})
           loop c pid sleepingOnFut'' sleepingOnAttr'' counter''

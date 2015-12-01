{-# LANGUAGE TemplateHaskell, GADTs, DeriveDataTypeable, ScopedTypeVariables, OverloadedStrings #-}
-- | The core of the ABS-Haskell runtime execution. 
--
-- This module implements the execution-logic of 
-- 
-- 1. a main COG with 'main-is' action
-- 2. a local or distributed COG (depending on if --distributed is enabled) with 'spawnCOG' action

module Lang.ABS.Runtime.Core 
    (spawnCOG
    ,main_is
    ,new
    ,new_local
    ,set
    ,spawnClosure
    ,demo_vms
    ,demo_reqs
    ,demo_name
    ,demo_load
    ,RootDict(..)
    ) where

import Lang.ABS.Runtime.Base
import Lang.ABS.Runtime.Conf
--import qualified Lang.ABS.StdLib.DC as DC (__remoteTable) -- the remotable methods table of DC

-- shared memory
import Data.IORef (newIORef, modifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar, putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)

-- the ABS monad stack
import qualified Control.Monad.Trans.State.Strict as S (runStateT, modify, get, put)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))

-- utils
import System.Exit (exitSuccess)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, foldM, liftM, forever)
import Data.Maybe (isJust)
import Data.List (foldl', find, splitAt)
import qualified Data.Map.Strict as M (Map, empty, insertWith, updateLookupWithKey, update, findWithDefault, lookup, insertLookupWithKey)

-- for exception handling of the COGs
import Control.Exception (throw)
import Control.Monad.Catch (catchAll)

-- for distributed programming and communication = Cloud Haskell
import qualified Control.Distributed.Process as CH
import Control.Distributed.Process.Node -- for local node creation and run/fork local processes
import Network.Transport.TCP (createTransport, defaultTCPParameters, encodeEndPointAddress)
import Network.Socket (withSocketsDo)
import Control.Distributed.Process.Internal.Types (nullProcessId) -- COG has a null pid on local (non-distributed) runtime

-- for encoding the FROM_PID environment variable
import System.Environment (getEnv)
import System.IO.Error (tryIOError)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BS

-- for serializing builtin closures (the spawn) and derisializing
import qualified Control.Distributed.Static as Static
import Data.Rank1Dynamic
import Data.Rank1Typeable
import Unsafe.Coerce (unsafeCoerce)
import Data.Binary (encode,decode)

-- for demo
import Web.Scotty as Web
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)

-- NOTE: the loops must be tail-recursive (not necessarily syntactically tail-recursive) to avoid stack leaks

-- | (only applied when --distributed). This is a so-called cog-forwarder which is an extra (lightweight) thread
-- that accompanies the COG thread and acts as the mediator from the outside-world to the local world.
--
-- It listens in a remote queue (mailbox) and forwards any messages to the COG's local queue ("Chan")
fwd_cog :: COG              -- ^ the _local-only_ channel (queue) of the COG process
        -> CH.Process ()          -- ^ is itself a CH process
fwd_cog thisCOG@(COG(c,pid)) = do
  -- CH.liftIO $ print pid
  j <- CH.expect :: CH.Process Job
  -- CH.liftIO $ print "received something"
  case j of
    WakeupSignal v cog futId -> do
              -- CH.liftIO $ print "received wakeup"
              -- lookup future foreign table
              foreign_map <- CH.liftIO $ readMVar fm                   
              case M.lookup (cog,futId) foreign_map of
                Just (AnyMVar m) -> CH.liftIO $ putMVar (unsafeCoerce m) v -- putMVar to local future
                _ -> error "foreign table lookup fail"
              CH.liftIO $ writeChan c j
    RemotJob obj f clos -> do
              -- CH.liftIO $ print ("received remotjob")
              -- the only thing that it does is unclosure the clos
              unclos <- CH.unClosure clos
              CH.liftIO $ writeChan c (LocalJob obj f unclos)
    MachineUp remoteMainFwdPid futId -> do
              -- CH.liftIO $ print remoteAddress
              foreign_map <- CH.liftIO $ readMVar fm                   
              case M.lookup (thisCOG,futId) foreign_map of
                Just (AnyMVar m) -> CH.liftIO $ putMVar (unsafeCoerce m) (remoteMainFwdPid)
                _ -> error "foreign table lookup fail"

              CH.liftIO $ print "Machines connected"
              CH.liftIO $ writeChan c (WakeupSignal remoteMainFwdPid thisCOG futId)
              return ()
    _ -> error "this should not happen"
  fwd_cog thisCOG

-- | Each COG is a thread or a process
spawnCOG :: CH.Process COG -- ^ it returns the created COG-thread ProcessId. This is used to update the location of the 1st created object
spawnCOG = do
  c <- CH.liftIO newChan
  if (distributed conf || isJust (port conf))     -- DISTRIBUTED (default) (1 COG Process + 1 Forwarder Process)
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
                         (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> 
                          if CH.processNodeId ftid == myNodeId
                          then do 
                           CH.liftIO $ putMVar mvar fin -- is local future, write to it
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
                           else do -- is remote future, remote-send to it
                             lift $ CH.send ftid (WakeupSignal fin cog fid)
                             return sleepingOnFut
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
  -- DISTRIBUTED (1 COG Process + 1 Forwarder Process)
  if (distributed conf || isJust (port conf))
    then do
      Right trans <- createTransport myIP port' defaultTCPParameters
      myLocalNode <- newLocalNode trans (rtable (outsideRemoteTable)) -- outsideRemoteTable) -- new my-node
      c <- newChan            -- sharing in-memory channel between Forwarder and Cog
      fwdPid <- forkProcess myLocalNode (CH.getSelfPid >>= \ pid -> fwd_cog (COG (c,pid))) -- start the Forwarder process
      -- Was this Node-VM created by another (remote) VM? then connect with this *CREATOR* node and answer back with an ack
      maybeCreatorPidStr <- tryIOError (getEnv "FROM_PID" )
      case maybeCreatorPidStr of
        Left _ex -> do -- no creator, this is the START-SYSTEM and runs MAIN-BLOCK
           forkIO $ demo_server
           forkIO $ forever (atomicModifyIORef' demo_reqs (\ _ -> (0, ())) >> threadDelay 10000000)
           -- reqs <- liftIO $ atomicModifyIORef' demo_reqs (\ reqs -> (0, reqs))

           writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c,fwdPid)) (-1))) -- send the Main Block as the 1st created process
           runProcess myLocalNode (loop c fwdPid M.empty M.empty 1) -- start the COG process
        Right "" -> do -- no creator, this is the START-SYSTEM and runs MAIN-BLOCK
           runProcess myLocalNode (loop c fwdPid M.empty M.empty 1)
        Right respToFutStr -> do -- there is a Creator PID; extract its NodeId
                    -- try to establish TCP connection with the creator
                    let (FutureRef _ (COG (_,creatorPid)) i) = decode (B64.decodeLenient (C8.pack respToFutStr)) :: Fut a
                    runProcess myLocalNode (do
                                             CH.send creatorPid (MachineUp fwdPid i) -- send ack to creatorPid
                                             loop c fwdPid M.empty M.empty 1) -- start the COG process (this cog is needed, because it runs dc's methods)


  -- LOCAL-ONLY (DEFAULT) (multicore) (1 COG Process)
    else do
      let myIp = "127.0.0.1" -- a placeholder for identifying the local node. No outside connection will be created.
      Right trans <- createTransport myIp port' defaultTCPParameters
      myLocalNode <- newLocalNode trans initRemoteTable -- not needed to create the remote-table

      c <- newChan               -- in-memory channel
      writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c,nullProcessId $ CH.NodeId $ encodeEndPointAddress myIp port' 0)) 1)) -- send the Main Block as the 1st created process
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
                               if CH.processNodeId ftid == myNodeId
                                then do 
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
                                else do -- is remote future, remote-send to it
                                 lift $ CH.send ftid (WakeupSignal fin cog fid)
                                 return sleepingOnFut
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

{-# INLINE new #-}
new :: (Root_ a) => a -> ABS (Obj a)
new smart = do 
  new_cog@(COG (chan, _)) <- lift $ lift spawnCOG
  ioref <- CH.liftIO $ newIORef smart
  let obj = ObjectRef ioref new_cog 0
  CH.liftIO $ writeChan chan (LocalJob obj NullFutureRef (__init obj))
  return obj

spawn :: (Root_ a) => a -> CH.Process (Obj a)
spawn smart = do 
  CH.liftIO $ print "received spawn"
  new_cog@(COG (chan, _)) <- spawnCOG
  ioref <- CH.liftIO $ newIORef smart
  let obj = ObjectRef ioref new_cog 0
  CH.liftIO $ writeChan chan (LocalJob obj NullFutureRef (__init obj))
  return obj


{-# INLINE new_local #-}
new_local :: (Root_ a) => a -> Obj creator -> ABS (Obj a)
new_local smart (ObjectRef _ thisCOG _) = do
  ioref <- CH.liftIO $ newIORef smart
  astate@(AState{aCounter = counter}) <- lift S.get
  lift (S.put (astate{aCounter = counter + 1}))
  let obj = ObjectRef ioref thisCOG counter
  __init obj
  return obj


{-# INLINE set #-}
set :: Int -> (v -> a -> a) -> v -> Obj a -> ABS ()
set i upd v _this@(ObjectRef ioref (COG (chan, _)) oid)  = do 
  astate@(AState _ om fm) <- lift S.get
  CH.liftIO (modifyIORef' ioref (upd v))
  let (maybeWoken, om') = M.updateLookupWithKey (\ _k _v -> Nothing) (oid, i) om
  fm' <- maybe (return fm)
        (\ woken -> CH.liftIO (updateWoken chan fm woken))
        maybeWoken
  lift  (S.put astate{aSleepingO = om', aSleepingF = fm'})


data RootDict a where
  RootDict :: Root_ a => RootDict a
  deriving Typeable

spawnDict :: RootDict a -> a -> CH.Process (Obj a)
spawnDict RootDict = spawn

spawnDictStatic :: Static.Static (RootDict a -> a -> CH.Process (Obj a))
spawnDictStatic = Static.staticLabel "$spawnDict"

rootDecodeDict :: RootDict a -> BS.ByteString -> a
rootDecodeDict RootDict = decode 

rootDecodeDictStatic :: Static.Static (RootDict a -> BS.ByteString -> a)
rootDecodeDictStatic = Static.staticLabel "$rootDecodeDict"

rtable :: Static.RemoteTable -> Static.RemoteTable
rtable = Static.registerStatic "$spawnDict" (toDynamic (spawnDict :: RootDict ANY -> ANY -> CH.Process (Obj ANY)))
        . Static.registerStatic "$rootDecodeDict" (toDynamic (rootDecodeDict :: RootDict ANY -> BS.ByteString -> ANY))
        

spawnClosure :: forall a. Root_ a => Static.Static (RootDict a) -> a -> Static.Closure (CH.Process (Obj a))
spawnClosure dict objv = Static.closure decoder (encode objv)
   where
    decoder :: Static.Static (BS.ByteString -> CH.Process (Obj a))
    decoder = (spawnDictStatic `Static.staticApply` dict) `Static.staticCompose` (rootDecodeDictStatic `Static.staticApply` dict)


{-# NOINLINE demo_vms #-}
demo_vms :: IORef Int
demo_vms = unsafePerformIO $ newIORef 0

{-# NOINLINE demo_reqs #-}
demo_reqs :: IORef Int
demo_reqs = unsafePerformIO $ newIORef 0

{-# NOINLINE demo_name #-}
demo_name :: IORef String
demo_name = unsafePerformIO $ newIORef "Demo"

{-# NOINLINE demo_load #-}
demo_load :: IORef Double
demo_load = unsafePerformIO $ newIORef 0

demo_server :: IO ()
demo_server = Web.scotty 80 $ do
    -- homepage            
    Web.get "/" $ html "<html><head><script type='text/javascript' src='smoothie.js'></script></head><body><center> \
                        \ <h1 id='name'>Demo</h1> \
                        \ <font size='5'>Finished jobs/10s:</font> <canvas id='canvas1' width='1000' height='200'></canvas> \
                        \ <hr/> \
                        \ <h1 id='vms'>Running VMs: 0</h1> \
                        \ <hr/> \
                        \ <font size='5'>Average Cpu Load:</font> <canvas id='canvas2' width='1000' height='200'></canvas></center> \
                        \ <script type='text/javascript'> \
                        \ var line1 = new TimeSeries(); \
                        \ var line2 = new TimeSeries(); \
                        \ setInterval(function() { \
                        \    var request = new XMLHttpRequest(); \
                        \    request.open('GET', '/ajax', false); \
                        \    request.send(); \
                        \    var rsp = JSON.parse(request.responseText); \
                        \    document.getElementById('name').innerHTML = rsp[2]; \
                        \    document.getElementById('vms').innerHTML = 'Running VMs: <b>' + rsp[0]; +'</b>'; \
                        \    line1.append(new Date().getTime(), rsp[1]); \
                        \    line2.append(new Date().getTime(), rsp[3]); \
                        \ }, 10000); \
                        \ var smoothie1 = new SmoothieChart({labels:{fontSize:19},millisPerPixel:400, minValue: 0, timestampFormatter:SmoothieChart.timeFormatter, grid: {lineWidth: 1, millisPerLine: 30000, verticalSections: 9 } }); \
                        \ smoothie1.addTimeSeries(line1, { strokeStyle: 'rgb(0, 255, 0)', fillStyle: 'rgba(0, 255, 0, 0.4)', lineWidth: 3 }); \
                        \ smoothie1.streamTo(document.getElementById('canvas1'), 1000); \
                        \ var smoothie2 = new SmoothieChart({labels:{fontSize:19},millisPerPixel:400, minValue: 0, timestampFormatter:SmoothieChart.timeFormatter, grid: {lineWidth: 1, millisPerLine: 30000, verticalSections: 9 } }); \
                        \ smoothie2.addTimeSeries(line2, { strokeStyle: 'rgb(255, 0, 0)', fillStyle: 'rgba(255, 0, 0, 0.4)', lineWidth: 3 }); \
                        \ smoothie2.streamTo(document.getElementById('canvas2'), 1000); \
                        \ </script></body></html>"


    -- static js chart library, should be in current dir (next to the executable)
    -- todo: add it to cabal inlc. datafiles
    Web.get "/smoothie.js" $ do
                          Web.setHeader "Content-Type" "text/javascript"
                          Web.file "./smoothie.js"


    -- refreshed data ajax
    -- upon refresh, it zeroes the reqs counter
    Web.get "/ajax" $ do
      vms <- liftIO $ readIORef demo_vms
      name <- liftIO $ readIORef demo_name
      load <- liftIO $ readIORef demo_load
      reqs <- liftIO $ readIORef demo_reqs
      json (vms,reqs,name,load)


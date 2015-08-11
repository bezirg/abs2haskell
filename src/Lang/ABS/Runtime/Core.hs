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
    ) where

import Lang.ABS.Runtime.Base
import Lang.ABS.Runtime.Conf
--import qualified Lang.ABS.StdLib.DC as DC (__remoteTable) -- the remotable methods table of DC

-- shared memory
import Data.IORef (newIORef, modifyIORef')
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)

-- the ABS monad stack
import qualified Control.Monad.Trans.State.Strict as S (runStateT, modify, get, put)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import Control.Monad.IO.Class (liftIO)

-- utils
import System.Exit (exitSuccess)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, foldM, liftM)
import Data.List (foldl', splitAt)
import qualified Data.Map.Strict as M (Map, empty, insertWith, updateLookupWithKey, update, findWithDefault, insertLookupWithKey)

-- for exception handling of the COGs
import Control.Exception (throw)
import Control.Monad.Catch (catchAll)

-- | Each COG is a thread or a process
spawnCOG :: IO COG -- ^ it returns the created COG-thread ProcessId. This is used to update the location of the 1st created object
spawnCOG = do
  c <- newChan
  pid <- forkIO $ do  -- Proc-2 is the local cog thread. It's job channel is the 2nd part of the COG's id
             -- each COG holds two tables:
           let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
           let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
           -- start the loop of the COG
           myPid <- myThreadId
           loop c myPid sleepingOnFut sleepingOnAttr 1
  return $ COG (c, pid)
    where
      -- COG loop definition
      loop c pid sleepingOnFut sleepingOnAttr counter = do
        -- on each iteration, it listens for next job on the input job queue
        nextJob <- readChan c
        case nextJob of
          -- wake signals are transmitted (implicitly) from a COG to another COG to wakeup some of the latter's sleeping process
          WakeupSignal v cog i -> do
             let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepingOnFut
             -- put the woken back to the enabled queue
             sleepingOnAttr' <- maybe (return sleepingOnAttr) (updateWoken c sleepingOnAttr) maybeWoken
             loop c pid sleepingOnFut'' sleepingOnAttr' counter
          -- run-jobs are issued by the user *explicitly by async method-calls* to do *ACTUAL ABS COMPUTE-WORK*
          LocalJob obj fut coroutine -> do
             (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''})) <- S.runStateT (do
              -- the cog catches any exception and lazily records it into the future-box (mvar)
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 lift $ when (traceExceptions conf) $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx)
              case p of
                    -- the job of the callee finished, send a wakeup signal to remote cog that "nourishes" the sleeping caller-process
                    Right fin -> do
                       case fut of
                         (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> do
                           lift $ putMVar mvar fin -- is local future, write to it
                           if ftid /= pid -- remote job finished, wakeup the remote cog
                              then do
                                lift $ writeChan fcog (WakeupSignal fin cog fid)
                                return sleepingOnFut
                              else do
                                -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                -- because it adds an extra iteration
                                let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,fid) sleepingOnFut
                                -- put the woken back to the enabled queue
                                maybe (return ()) (\ woken -> do
                                                   sleepingOnAttr' <- lift $ updateWoken c sleepingOnAttr woken
                                                   S.modify (\ astate -> astate {aSleepingO = sleepingOnAttr'})) maybeWoken
                                return sleepingOnFut'
                         NullFutureRef -> return sleepingOnFut
                    -- the process deliberately suspended (by calling suspend)
                    Left (Yield S cont) -> do
                           lift $ writeChan c (LocalJob obj fut cont) -- reschedule its continuation at the end of the job-queue
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
        -> IO ()                  -- ^ returns void. It is the main procedure of a compiled ABS application.
main_is mainABS = do
  -- DISTRIBUTED (1 COG Process + 1 Forwarder Process)
  c <- newChan               -- in-memory channel
  pid <- myThreadId
  writeChan c (LocalJob ((error "not this at top-level") :: Obj Null) NullFutureRef (mainABS $ ObjectRef undefined (COG (c, pid)) 1)) -- send the Main Block as the 1st created process
  loop c pid M.empty M.empty 1

   where
     loop c pid sleepingOnFut sleepingOnAttr counter = do
       nextJob <- readChan c
       case nextJob of
         WakeupSignal v cog i -> do
           let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepingOnFut
           sleepingOnAttr' <- maybe (return sleepingOnAttr) (updateWoken c sleepingOnAttr) maybeWoken -- put the woken back to the enabled queue
           loop c pid sleepingOnFut'' sleepingOnAttr' counter
         LocalJob obj fut coroutine -> do
           (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''})) <- S.runStateT (do
              p <- resume coroutine `catchAll` (\ someEx -> do
                                                 lift $ when (traceExceptions conf) $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
                                                 return $ Right $ throw someEx) 
              case p of
                Right fin -> case fut of
                              (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> do
                                 lift $ putMVar mvar fin
                                 if ftid /= pid
                                   then do -- remote job finished, wakeup the remote cog
                                     lift $ writeChan fcog (WakeupSignal fin cog fid)
                                     return sleepingOnFut
                                   else do
                                     -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                     -- because it adds an extra iteration
                                     let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,fid) sleepingOnFut
                                     -- put the woken back to the enabled queue
                                     maybe (return ()) (\ woken -> do
                                                         sleepingOnAttr' <- lift $ updateWoken c sleepingOnAttr woken
                                                         S.modify (\ astate -> astate {aSleepingO = sleepingOnAttr'})) maybeWoken
                                     return sleepingOnFut'
                              NullFutureRef -> 
                                         if keepAlive conf
                                           then do
                                             lift $ print $ "main finished" ++ show pid
                                             return sleepingOnFut
                                           -- exit early otherwise
                                           else do
                                            lift $ print "Main COG has exited with success"
                                            lift $ exitSuccess
                Left (Yield S cont) -> do
                       lift $ writeChan c (LocalJob obj fut cont) 
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
  ioref <- liftIO $ newIORef smart
  let obj = ObjectRef ioref new_cog 0
  liftIO $ writeChan chan (LocalJob obj NullFutureRef (__init obj))
  return obj

{-# INLINE new_local #-}
new_local :: (Root_ a) => a -> Obj creator -> ABS (Obj a)
new_local smart (ObjectRef _ thisCOG _) = do
  ioref <- liftIO $ newIORef smart
  astate@(AState{aCounter = counter}) <- lift S.get
  lift (S.put (astate{aCounter = counter + 1}))
  let obj = ObjectRef ioref thisCOG counter
  __init obj
  return obj


{-# INLINE set #-}
set :: Int -> (v -> a -> a) -> v -> Obj a -> ABS ()
set i upd v _this@(ObjectRef ioref (COG (chan, _)) oid)  = do 
  astate@(AState _ om fm) <- lift S.get
  liftIO $ modifyIORef' ioref (upd v)
  let (maybeWoken, om') = M.updateLookupWithKey (\ _k _v -> Nothing) (oid, i) om
  fm' <- maybe (return fm)
        (\ woken -> liftIO $ updateWoken chan fm woken)
        maybeWoken
  lift  (S.put astate{aSleepingO = om', aSleepingF = fm'})





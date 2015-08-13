-- | The core of the ABS-Haskell runtime execution. 
--
-- This module implements the execution-logic of 
-- 
-- 1. a main COG with 'main-is' action
-- 2. a local or distributed COG (depending on if --distributed is enabled) with 'spawnCOG' action

module Lang.ABS.Runtime.Core 
    (spawnCOG
    --,main_is
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
import qualified Control.Monad.Trans.State.Strict as S (runStateT, evalStateT, modify, get, gets, put)
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


-- | ABS main-block thread (COG-like thread)
-- main_is :: (Obj Null -> ABS ()) -- ^ a main-block monadic action (a fully-applied method with a null this-context that returns "Unit")
--         -> IO ()                  -- ^ returns void. It is the main procedure of a compiled ABS application.
-- main_is mainABS = do
--   -- DISTRIBUTED (1 COG Process + 1 Forwarder Process)
--   c <- newChan               -- in-memory channel
--   pid <- myThreadId
--   writeChan c (LocalJob (mainABS $ ObjectRef undefined (COG (c, pid)) 1)) -- send the Main Block as the 1st created process
--   loop c pid M.empty M.empty 1

--    where
--      loop c pid sleepOnFut sleepOnAttr counter = do
--        nextJob <- readChan c
--        case nextJob of
--          WakeupSignal v cog i -> do
--            let (maybeWoken, sleepOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepOnFut
--            sleepOnAttr' <- maybe (return sleepOnAttr) (updateWoken c sleepOnAttr) maybeWoken -- put the woken back to the enabled queue
--            loop c pid sleepOnFut' sleepOnAttr' counter
--          LocalJob coroutine -> do
--            (p, (AState {aCounter = counter', aSleepingO = sleepOnAttr', aSleepingF = sleepOnFut'})) <- S.runStateT (resume coroutine `catchAll` (\ someEx -> do
--                                                  lift $ when (traceExceptions conf) $ print $ "Process died upon Uncaught-Exception: " ++ show someEx 
--                                                  return $ Right $ throw someEx)) (AState {aCounter = counter, aSleepingO = sleepOnAttr, aSleepingF = sleepOnFut})
--            (sleepOnAttr'', sleepOnFut'') <- case p of
--              Right fin -> case fut of
--                               (FutureRef mvar cog@(COG (fcog, ftid)) fid) -> do
--                                  putMVar mvar fin
--                                  if ftid /= pid
--                                    then do -- remote job finished, wakeup the remote cog
--                                      writeChan fcog (WakeupSignal fin cog fid)
--                                      return (sleepOnAttr', sleepOnFut')
--                                    else do
--                                      -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
--                                      -- because it adds an extra iteration
--                                      let (maybeWoken, sleepOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,fid) sleepOnFut'
--                                      sleepOnAttr'' <- maybe (return sleepOnAttr') (updateWoken c sleepOnAttr') maybeWoken -- put the woken back to the enabled queue
--                                      return (sleepOnAttr'', sleepOnFut'')
--                               NullFutureRef -> 
--                                          if keepAlive conf
--                                            then do
--                                              print $ "main finished" ++ show pid
--                                              return (sleepOnAttr', sleepOnFut')
--                                            -- exit early otherwise
--                                            else do
--                                             print "Main COG has exited with success"
--                                             exitSuccess
--              Left (Yield S cont) -> do
--                        writeChan c (LocalJob fut cont) 
--                        return (sleepOnAttr', sleepOnFut')
--              Left (Yield (FL f@(FutureRef _ cog i)) cont) -> do
--                        return (sleepOnAttr', M.insertWith (++) (cog,i) [(LocalJob fut cont, Nothing)] sleepOnFut')
--              Left (Yield (FF f@(FutureRef _ cog i) fid obj) cont) -> do
--                        -- updating both suspended tables
--                        let ObjectRef _ _ oid = obj
--                        let lengthOnAttr = length $ M.findWithDefault [] (oid, fid) sleepOnAttr'
--                        let (mFutEntry, sleepOnFut'') = M.insertLookupWithKey (\ _ p n -> p ++ n) (cog,i) [(LocalJob fut cont, Just ((oid,fid),lengthOnAttr))] sleepOnFut'
--                        let lengthOnFut = maybe 0 length mFutEntry
--                        let sleepOnAttr'' = M.insertWith (++) (oid,fid) [(LocalJob fut cont, Just ((cog,i),lengthOnFut))] sleepOnAttr'
--                        return (sleepOnAttr'', sleepOnFut'')
--              Left (Yield (A o@(ObjectRef _ _ oid) fields) cont) -> do
--                        return (foldl' (\ m i -> M.insertWith (++) (oid,i) [(LocalJob fut cont,Nothing)] m) sleepOnAttr' fields, sleepOnFut')
                                              
--            loop c pid sleepOnFut'' sleepOnAttr'' counter'







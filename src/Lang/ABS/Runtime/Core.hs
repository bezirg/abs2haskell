module Lang.ABS.Runtime.Core 
    (spawnCOG
    ,main_is
    ) where

import Lang.ABS.Runtime.Base
import Data.List (foldl')
import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Data.Map.Strict as M (empty, insertWith, updateLookupWithKey)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, writeList2Chan, Chan)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS as RWS (runRWST, modify)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)
import Control.Exception (throw)
import Control.Monad.Catch (catchAll)

-- NOTE: the loops must be tail-recursive (not necessarily syntactically tail-recursive) to avoid stack leaks

spawnCOG :: Chan Job -> IO ThreadId
spawnCOG c = forkIO $ do        -- each COG is a lightweight Haskell thread
  tid <- myThreadId
  -- each COG holds two tables:
  let sleepingOnFut = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
  let sleepingOnAttr = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
  -- start the loop of the COG
  loop tid sleepingOnFut sleepingOnAttr 1

    where
      -- COG loop definition
      loop tid sleepingOnFut sleepingOnAttr counter = do
        -- on each iteration, it listens for next job on the input job queue
        nextJob <- readChan c
        case nextJob of
          -- wake signals are transmitted (implicitly) from a COG to another COG to wakeup some of the latter's sleeping process
          WakeupSignal f -> do
             let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingOnFut
             maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
             loop tid sleepingOnFut'' sleepingOnAttr counter
          -- run-jobs are issued by the user *explicitly by async method-calls* to do *ACTUAL ABS COMPUTE-WORK*
          RunJob obj fut@(FutureRef mvar (fcog, ftid) _) coroutine -> do
             (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''}), _) <- RWS.runRWST (do
              -- the cog catches any exception and lazily records it into the future-box (mvar)
              p <- resume coroutine `catchAll` (\ someEx -> return $ Right $ throw someEx) 
              case p of
                    -- the job of the callee finished, send a wakeup signal to remote cog that "nourishes" the sleeping caller-process
                    Right fin -> do
                           lift $ putMVar mvar fin
                           if ftid /= tid -- remote job finished, wakeup the remote cog
                              then do
                                lift $ writeChan fcog (WakeupSignal fut)
                                return sleepingOnFut
                              else do
                                -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                -- because it adds an extra iteration
                                let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingOnFut
                                -- put the woken back to the enabled queue
                                maybe (return ()) (\ woken -> lift $ writeList2Chan c woken) maybeWoken
                                return sleepingOnFut'
                    -- the process deliberately suspended (by calling suspend)
                    Left (Yield S cont) -> do
                           lift $ writeChan c (RunJob obj fut cont) -- reschedule its continuation at the end of the job-queue
                           return sleepingOnFut
                    -- the process deliberately decided to await on a future to finish (by calling await f?;)
                    Left (Yield (F f) cont) -> do
                           return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingOnFut) -- update sleepingf-table
                    -- the process deliberately decided to await on a this.field to change (by calling await (this.field==v);)
                    Left (Yield (T o fields) cont) -> do
                           -- update sleepingo-table
                           let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingOnAttr fields
                           RWS.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                           return sleepingOnFut
                                                    ) (AConf {aThis = obj, 
                                                              aCOG = (c, tid)
                                                             })
                                                                     (AState {aCounter = counter,
                                                                              aSleepingO = sleepingOnAttr})
             loop tid sleepingOnFut'' sleepingOnAttr'' counter''


-- ABS Main-block thread (COG-like thread)
main_is :: ABS Null () -> IO () 
main_is mainABS = do
  tid <- myThreadId
  let sleepingOnFut = M.empty :: FutureMap
  let sleepingOnAttr = M.empty :: ObjectMap
  c <- newChan
  writeChan c (RunJob (error "not this at top-level") TopRef mainABS)
  -- start the main-block loop
  loop c tid sleepingOnFut sleepingOnAttr 1

   where
     loop c tid sleepingOnFut sleepingOnAttr counter = do
       nextJob <- readChan c
       case nextJob of
         WakeupSignal f -> do
           let (maybeWoken, sleepingOnFut'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingOnFut
           maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
           loop c tid sleepingOnFut'' sleepingOnAttr counter
         RunJob obj fut coroutine -> do
           (sleepingOnFut'', (AState {aCounter = counter'', aSleepingO = sleepingOnAttr''}), _) <- RWS.runRWST (do
              p <- resume coroutine `catchAll` (\ someEx -> return $ Right $ throw someEx) 
              case p of
                Right fin -> case fut of
                              (FutureRef mvar (fcog, ftid) _) -> do
                                 lift $ putMVar mvar fin
                                 if ftid /= tid
                                   then do -- remote job finished, wakeup the remote cog
                                     lift $ writeChan fcog (WakeupSignal fut)
                                     return sleepingOnFut
                                   else do
                                     -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                     -- because it adds an extra iteration
                                     let (maybeWoken, sleepingOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingOnFut
                                     -- put the woken back to the enabled queue
                                     maybe (return ()) (\ woken -> lift $ writeList2Chan c woken) maybeWoken
                                     return sleepingOnFut'
                              TopRef -> do
                                       lift $ print "Main COG has exited with success"
                                       lift $ exitSuccess
                Left (Yield S cont) -> do
                       lift $ writeChan c (RunJob obj fut cont) 
                       return sleepingOnFut
                Left (Yield (F f) cont) -> do
                       return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingOnFut)
                Left (Yield (T o fields) cont) -> do
                       let sleepingOnAttr' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingOnAttr fields
                       RWS.modify $ \ astate -> astate {aSleepingO = sleepingOnAttr'}
                       return sleepingOnFut
                                              ) (AConf {aThis = obj, 
                                                        aCOG = (c, tid)
                                                       }) (AState {aCounter = counter,
                                                                   aSleepingO = sleepingOnAttr})
           loop c tid sleepingOnFut'' sleepingOnAttr'' counter''

module Lang.ABS.Runtime.Core 
    (spawnCOG
    ,main_is
    ) where

import Lang.ABS.Runtime.Base
import Data.List (foldl')
import Control.Monad (when)
import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Data.Map.Strict as M (empty, insertWith, updateLookupWithKey)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, writeList2Chan, Chan)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS as RWS (runRWST, modify)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)

-- NOTE: the loops must be tail-recursive (not necessarily syntactically tail-recursive) to avoid stack leaks

spawnCOG :: Chan Job -> IO ThreadId
spawnCOG c = forkIO $ do        -- each COG is a lightweight Haskell thread
  tid <- myThreadId
  -- each COG holds two tables:
  let sleepingF = M.empty :: FutureMap  -- sleeping processes waiting on a future to be finished and arrive, so they can wake-up
  let sleepingO = M.empty :: ObjectMap  -- sleeping process waiting on a this.field to be mutated, so they can wake-up
  -- start the loop of the COG
  loop tid sleepingF sleepingO 1

    where
      -- COG loop definition
      loop tid sleepingF sleepingO counter = do
        -- on each iteration, it listens for next job on the input job queue
        nextJob <- readChan c
        case nextJob of
          -- wake signals are transmitted (implicitly) from a COG to another COG to wakeup some of the latter's sleeping process
          WakeupSignal f -> do
             let (maybeWoken, sleepingF'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingF
             maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
             loop tid sleepingF'' sleepingO counter
          -- run-jobs are issued by the user *explicitly by async method-calls* to do *ACTUAL ABS COMPUTE-WORK*
          RunJob obj fut@(FutureRef mvar (fcog, ftid) _) coroutine -> do
             (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
                  p <- resume coroutine
                  case p of
                    -- the job of the callee finished, send a wakeup signal to remote cog that "nourishes" the sleeping caller-process
                    Right fin -> do
                           lift $ putMVar mvar fin
                           if ftid /= tid -- remote job finished, wakeup the remote cog
                              then do
                                lift $ writeChan fcog (WakeupSignal fut)
                                return sleepingF
                              else do
                                -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                -- because it adds an extra iteration
                                let (maybeWoken, sleepingF') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingF
                                -- put the woken back to the enabled queue
                                maybe (return ()) (\ woken -> lift $ writeList2Chan c woken) maybeWoken
                                return sleepingF'
                    -- the process deliberately suspended (by calling suspend)
                    Left (Yield S cont) -> do
                           lift $ writeChan c (RunJob obj fut cont) -- reschedule its continuation at the end of the job-queue
                           return sleepingF
                    -- the process deliberately decided to await on a future to finish (by calling await f?;)
                    Left (Yield (F f) cont) -> do
                           return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingF) -- update sleepingf-table
                    -- the process deliberately decided to await on a this.field to change (by calling await (this.field==v);)
                    Left (Yield (T o fields) cont) -> do
                           -- update sleepingo-table
                           let sleepingO' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingO' fields
                           RWS.modify $ \ astate -> astate {aSleepingO = sleepingO'}
                           return sleepingF
                                                    ) (AConf {aThis = obj, 
                                                              aCOG = (c, tid)
                                                             })
                                                                     (AState {aCounter = counter,
                                                                              aSleepingO = sleepingO})
             loop tid sleepingF'' sleepingO'' counter''


-- ABS Main-block thread (COG-like thread)
main_is :: ABS Null () -> IO () 
main_is mainABS = do
  tid <- myThreadId
  let sleepingF = M.empty :: FutureMap
  let sleepingO = M.empty :: ObjectMap
  c <- newChan
  writeChan c (RunJob (error "not this at top-level") TopRef mainABS)
  -- start the main-block loop
  loop c tid sleepingF sleepingO 1

   where
     loop c tid sleepingF sleepingO counter = do
       nextJob <- readChan c
       case nextJob of
         WakeupSignal f -> do
           let (maybeWoken, sleepingF'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingF
           maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
           loop c tid sleepingF'' sleepingO counter
         RunJob obj fut coroutine -> do
           (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
              p <- resume coroutine
              case p of
                Right fin -> case fut of
                              (FutureRef mvar (fcog, ftid) _) -> do
                                 lift $ putMVar mvar fin
                                 if ftid /= tid
                                   then do -- remote job finished, wakeup the remote cog
                                     lift $ writeChan fcog (WakeupSignal fut)
                                     return sleepingF
                                   else do
                                     -- OPTIMIZATION: don't send a *superfluous* wakeup signal from->to the same COG, 
                                     -- because it adds an extra iteration
                                     let (maybeWoken, sleepingF') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture fut) sleepingF
                                     -- put the woken back to the enabled queue
                                     maybe (return ()) (\ woken -> lift $ writeList2Chan c woken) maybeWoken
                                     return sleepingF'
                              TopRef -> do
                                       lift $ print "Main COG has exited with success"
                                       lift $ exitSuccess
                Left (Yield S cont) -> do
                       lift $ writeChan c (RunJob obj fut cont) 
                       return sleepingF
                Left (Yield (F f) cont) -> do
                       return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingF)
                Left (Yield (T o fields) cont) -> do
                       let sleepingO' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingO' fields
                       RWS.modify $ \ astate -> astate {aSleepingO = sleepingO'}
                       return sleepingF
                                              ) (AConf {aThis = obj, 
                                                        aCOG = (c, tid)
                                                       }) (AState {aCounter = counter,
                                                                   aSleepingO = sleepingO})
           loop c tid sleepingF'' sleepingO'' counter''

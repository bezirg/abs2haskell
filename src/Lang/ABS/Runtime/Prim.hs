-- | The ABS-language primitives/statements implemented as a Haskell-embedded DSL (eDSL)
module Lang.ABS.Runtime.Prim
    (
     -- * Basic ABS primitives
     suspend', await', while', get', pro_new', pro_get', pro_give'-- , pro_new
    , pro_isempty', ifthenM', ifthenelseM', ifthenelse', null'
     -- * The async, async-optimized and sync calls
    ,async', osync', sync'
    ,main_is', new', newlocal', set'
     -- * The failure model
    ,throw', catches', finally', Exception, assert'
    )
 where

import Lang.ABS.Runtime.Base

import Control.Monad (when, unless)
import Control.Concurrent.Chan
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as S (get, put, modify', evalStateT, withStateT)
import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
import qualified Control.Monad.Catch
import qualified Control.Exception (fromException, evaluate, AssertionFailed (..))
import qualified Data.Set as S (insert, toList, empty)
import Data.List (foldl', splitAt)
import Control.Monad (when, foldM, liftM)
import Control.Concurrent (forkIO, myThreadId) --, runInUnboundThread)
import Data.IORef (newIORef, modifyIORef')
import System.Exit (exitSuccess)

{-# INLINE main_is' #-}
main_is' :: (Obj Null -> (() -> ABS ()) -> ABS ()) -> IO () 
main_is' mainABS = do          -- wrapping in runInUnboundThread may increase the perf if main is heavy
  c <- newChan
  pid <- myThreadId
  let main_obj = ObjectRef undefined (COG (c, pid)) 0
  -- send the Main Block as the 1st created process
  writeChan c $ LocalJob $ mainABS main_obj (\ () -> lift (print "main finished" >> exitSuccess)) -- >> back__ main_obj) -- no explicit exit but expect to later (slower) make a check and auto-throw blockonmvarexception
  S.evalStateT (back__ main_obj) (AState 1 M.empty M.empty)

back__  :: Obj a -> ABS ()
back__ from@(ObjectRef _ (COG (c,_)) _) = do
  nextJob <- lift $ readChan c
  case nextJob of
    WakeupSignal res cog i -> do
           AState {aSleepingF = sleepOnFut, aSleepingO = sleepOnAttr, aCounter = counter} <- S.get
           let (maybeWoken, sleepOnFut') = M.updateLookupWithKey (\ _k _v -> Nothing) (cog,i) sleepOnFut
           sleepOnAttr' <- maybe (return sleepOnAttr) (\ x -> lift $ updateWoken c sleepOnAttr x) maybeWoken -- put the woken back to the enabled queue
           S.put (AState counter sleepOnAttr' sleepOnFut')
           back__ from
    LocalJob coroutine -> coroutine

{-# INLINE suspend' #-}
suspend'  :: Obj a -> ABS () -> ABS ()
suspend' this@(ObjectRef _ (COG (thisChan,_)) _) k = lift (writeChan thisChan (LocalJob k)) >> back__ this

{-# INLINE await' #-}
await'  :: Obj a -> ABS () -> AwaitGuardCompiled b -> ABS ()
await' this k (FutureLocalGuard (FutureRef mvar cog i)) = do
   empty <- lift $ isEmptyMVar mvar
   if empty
    then S.modify' (\ (AState counter sleepOnAttr sleepOnFut) ->
                        AState counter sleepOnAttr (M.insertWith (++) (cog,i) [(LocalJob k, Nothing)] sleepOnFut)) >> back__ this
    else k

await' this@(ObjectRef _ _ oid) k g@(AttrsGuard is tg)  = do
  check <- tg
  if not check
    then if null is             -- no field-checks, so this will block indefinitely
         then throw' (return BlockedAwaitException)
         else S.modify' (\ (AState counter sleepOnAttr sleepOnFut) ->
                             AState counter (foldl' (\ m i -> M.insertWith (++) (oid,i) [(LocalJob $ await' this k g,Nothing)] m) sleepOnAttr is) sleepOnFut) >> back__ this
    else k                 -- check succeeded, continue


await' this@(ObjectRef _ _ oid) k g@(FutureFieldGuard i tg) = do
  fut <- tg
  case fut of
    (FutureRef mvar cog fid) -> do
             empty <- lift $ isEmptyMVar mvar
             if empty
              then S.modify' (\ (AState counter sleepOnAttr sleepOnFut) -> let
                -- updating both suspended tables
                lengthOnAttr = length $ M.findWithDefault [] (oid, i) sleepOnAttr
                (mFutEntry, sleepOnFut') = M.insertLookupWithKey (\ _ p n -> p ++ n) (cog,fid) [(LocalJob k, Just ((oid,i),lengthOnAttr))] sleepOnFut
                lengthOnFut = maybe 0 length mFutEntry
                sleepOnAttr' = M.insertWith (++) (oid,i) [(LocalJob $ await' this k g, Just ((cog,fid),lengthOnFut))] sleepOnAttr
                              in AState counter sleepOnAttr' sleepOnFut') >> back__ this
              else k
    NullFutureRef -> -- only awaits on one attribute to be field with a real future, not a nullfutureref
        S.modify' (\ (AState counter sleepOnAttr sleepOnFut) ->
                             AState counter (M.insertWith (++) (oid,i) [(LocalJob $ await' this k g,Nothing)] sleepOnAttr) sleepOnFut) >> back__ this


await' this@(ObjectRef _ hereCOG _) k (PromiseLocalGuard (PromiseRef mvar regsvar cog i)) = do
  empty <- lift $ isEmptyMVar mvar
  if empty
    then do
      mregs <- lift $ takeMVar regsvar
      case mregs of
        Nothing -> k        -- there was a race-condition, so we repair it with a maybe check
        Just scogs -> do
                   let scogs' = S.insert hereCOG scogs
                   lift $ putMVar regsvar (Just scogs')
                   S.modify' (\ (AState counter sleepOnAttr sleepOnFut) ->
                                  AState counter sleepOnAttr (M.insertWith (++) (cog,i) [(LocalJob k, Nothing)] sleepOnFut)) >> back__ this
    else k
  
await' this@(ObjectRef _ hereCOG oid) k g@(PromiseFieldGuard i tg)  = do
  p@(PromiseRef mvar regsvar cog fid) <- tg
  empty <- lift $ isEmptyMVar mvar
  if empty
   then do
    mregs <- lift $ takeMVar regsvar
    case mregs of
      Nothing -> k        -- there was a race-condition, so we repair it with a maybe check
      Just scogs -> do
                 let scogs' = S.insert hereCOG scogs
                 lift $ putMVar regsvar (Just scogs')
                 S.modify' (\ (AState counter sleepOnAttr sleepOnFut) -> let
                                -- updating both suspended tables
                                lengthOnAttr = length $ M.findWithDefault [] (oid, i) sleepOnAttr
                                (mFutEntry, sleepOnFut') = M.insertLookupWithKey (\ _ p n -> p ++ n) (cog,fid) [(LocalJob k, Just ((oid,i),lengthOnAttr))] sleepOnFut
                                lengthOnFut = maybe 0 length mFutEntry
                                sleepOnAttr' = M.insertWith (++) (oid,i) [(LocalJob $ await' this k g, Just ((cog,fid),lengthOnFut))] sleepOnAttr
                           in AState counter sleepOnAttr' sleepOnFut') >> back__ this
   else k

while' :: ABS Bool -> (ABS () -> ABS ()) -> ABS () -> ABS ()
while' predAction loopAction k = do
  res <- predAction
  if res
   then loopAction (while' predAction loopAction k)
   else k

pro_give' :: ABS (Promise a) -> ABS a -> ABS ()
pro_give' aP aVal = do
  (PromiseRef valMVar regsMVar _creatorCog _creatorCounter) <- aP
  val <- aVal
  success <- lift $ tryPutMVar valMVar val
  unless success $ Control.Monad.Catch.throwM PromiseRewriteException -- already resolved promise
  Just cogs <- lift $ takeMVar regsMVar
  lift $ mapM_ (\ (COG (fcog, _ftid)) -> writeChan fcog (WakeupSignal val _creatorCog _creatorCounter)) (S.toList cogs)
  lift $ putMVar regsMVar Nothing

pro_new' :: (Root_ o) => Obj o -> ABS (Promise a)
pro_new' (ObjectRef _ hereCOG _) = do
  valMVar <- lift $ newEmptyMVar
  regsMVar <-lift $ newMVar (Just S.empty)
  astate@(AState{aCounter = __counter}) <- S.get
  S.put (astate{aCounter = __counter + 1})
  return (PromiseRef valMVar regsMVar hereCOG __counter)

get' :: Fut f -> ABS f
get' (FutureRef mvar _ _) = lift $ Control.Exception.evaluate =<< readMVar mvar 

pro_get' :: Promise f -> ABS f
pro_get' (PromiseRef mvar _ _ _) = lift $ Control.Exception.evaluate =<< readMVar mvar 

pro_isempty' :: Promise f -> ABS Bool
pro_isempty' (PromiseRef mvar _ _ _) = lift $ Control.Exception.evaluate =<< isEmptyMVar mvar 

-- forces the reading of the future-box to whnf, so when the future-box is opened (through get) then the remote future exception will be raised

-- for using inside ABS monad
{-# INLINE ifthenM' #-}
ifthenM' :: ABS Bool -> (ABS () -> ABS ()) -> ABS () -> ABS ()
ifthenM' texp stm_then k = texp >>= (\ e -> if e
                                          then stm_then k
                                          else k)

-- for using inside ABS monad
{-# INLINE ifthenelseM' #-}
ifthenelseM' :: ABS Bool -> (ABS () -> ABS ()) -> (ABS () -> ABS ()) -> ABS () -> ABS ()
ifthenelseM' texp stm_then stm_else k = texp >>= (\ e -> if e 
                                                       then stm_then k
                                                       else stm_else k)

-- for using inside OO code but in pure-exp, it is just lifted if-then-else
{-# INLINE ifthenelse' #-}
ifthenelse' :: ABS Bool -> ABS b -> ABS b -> ABS b
ifthenelse' p t e = do
  res <- p 
  if res
    then t
    else e

-- -- instances to throwIO/try-catch-finally inside an ABS monad
-- instance (Functor s, Control.Monad.Catch.MonadThrow m) => Control.Monad.Catch.MonadThrow (Coroutine s m) where
--   throwM e = lift $ Control.Monad.Catch.throwM e

-- instance (Functor s, Control.Monad.Catch.MonadCatch m) => Control.Monad.Catch.MonadCatch (Coroutine s m) where
--   catch (Coroutine m) f = Coroutine $  m `Control.Monad.Catch.catch` \ e -> resume (f e)

-- instance (Functor s, Control.Monad.Catch.MonadMask m) => Control.Monad.Catch.MonadMask (Coroutine s m) where
--   mask a = Coroutine $ Control.Monad.Catch.mask $ \u -> resume (a $ q u)
--     where q u b = Coroutine $ u (resume b)
--   uninterruptibleMask a =
--     Coroutine $ Control.Monad.Catch.uninterruptibleMask $ \u -> resume (a $ q u)
--       where q u b = Coroutine $ u (resume b)

-- | aliases for easier exporting
throw' :: Control.Monad.Catch.Exception e => ABS e -> ABS a
throw' e = e >>= Control.Monad.Catch.throwM

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catches' :: ABS a -> [Control.Monad.Catch.Handler ABS (Maybe a)] -> ABS a
catches' a hs = a `Control.Monad.Catch.catch` handler
  where
    handler e = foldr probe (Control.Monad.Catch.throwM e) hs
      where
        probe (Control.Monad.Catch.Handler h) xs = maybe xs (\ e -> h e >>= (\case
                                                                              Nothing -> xs -- trick to allow actual term pattern-matching
                                                                              Just res -> return res))
                                                                              (Control.Exception.fromException e)


finally' :: ABS a -> ABS b -> ABS a
finally' = Control.Monad.Catch.finally

type Exception = Control.Monad.Catch.SomeException

-- | The ABS assertions. 
assert' :: ABS Prelude.Bool -> ABS ()
assert' act = act Prelude.>>= \ pred -> when (Prelude.not pred) 
             (throw' $ return $ Control.Exception.AssertionFailed "Assertion Failed")


-- | The reference to a null object
--
-- The class of a null object is "Null".
{-# INLINE null' #-}
null' :: Obj Null
null' = NullRef

{-# INLINE async' #-}
-- | The asynchronous wrapper that makes the method call
async' :: Obj caller -> Obj callee -> (Obj callee -> (a -> ABS ()) -> ABS ()) -> ABS (Fut a)
async' this@(ObjectRef _ thisCOG@(COG (thisChan,_)) _) (obj@(ObjectRef _ (COG (otherChan, _)) _)) mth
  = do __mvar <- lift newEmptyMVar
       __astate@(AState{aCounter = __counter}) <- S.get
       S.put (__astate{aCounter = __counter + 1})
       let destiny = FutureRef __mvar thisCOG __counter
       lift $ writeChan otherChan $ LocalJob (mth obj (\ res -> lift (putMVar __mvar res >> writeChan thisChan (WakeupSignal res thisCOG __counter)) >> back__ obj)) -- respond as the continuation
       return destiny
async' _ NullRef _ = error "async call to null"

{-# INLINE osync' #-}
-- | Optimized wrapper where we throw away the result. The transcompiler checks if the future returned is not stored to a variable.
osync' :: Obj caller -> Obj callee -> (Obj callee -> (a -> ABS ()) -> ABS ()) -> ABS ()
osync' _ (obj@(ObjectRef _ (COG (chan, _)) _)) mth = lift $ writeChan chan (LocalJob $ mth obj (\ _ -> back__ obj)) -- empty continuation
osync' _ NullRef _ = error "async call to null"


{-# INLINE sync' #-}
-- | The synchronous wrapper that makes the method call
sync' :: Obj caller -> Obj callee -> (res -> ABS ()) -> (Obj callee -> (res -> ABS ()) -> ABS ()) -> ABS ()
sync' (ObjectRef _ thisCOG _) obj@(ObjectRef _ otherCOG _) k mth = if (not (thisCOG == otherCOG)) 
                                                                  then error "Sync Call on a different COG detected"
                                                                  else mth obj k
sync' _ NullRef _ _ = error "sync call to null"


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

{-# INLINE new' #-}
new' :: (Root_ a) => a -> ABS (Obj a)
new' smart = lift $ do 
  c <- newChan
  pid <- forkIO $ myThreadId >>= \ myPid -> S.evalStateT (back__ $ ObjectRef undefined (COG (c,myPid)) 0) (AState 1 M.empty M.empty)
  ioref <- newIORef smart
  let obj = ObjectRef ioref (COG (c,pid)) 0
  writeChan c $ LocalJob $ __init obj $ \ () -> back__ obj
  return obj

-- -- | Each COG is a thread or a process
-- spawnCOG__ :: IO COG -- ^ it returns the created COG-thread ProcessId. This is used to update the location of the 1st created object
-- spawnCOG__ = do
--   c <- newChan
--   pid <- forkIO $ myThreadId >>= \ pid -> S.evalStateT (back__ $ ObjectRef undefined (COG (c,pid)) undefined) (AState 1 M.empty M.empty)
--   return $ COG (c, pid)


{-# INLINE newlocal' #-}
newlocal' :: (Root_ a) => a -> Obj creator -> ABS (Obj a)
newlocal' smart (ObjectRef _ thisCOG _) = do
  ioref <- lift $ newIORef smart
  astate@(AState{aCounter = counter}) <- S.get
  (S.put (astate{aCounter = counter + 1}))
  let obj = ObjectRef ioref thisCOG counter
  __init obj return   -- don't back, we continue anyway from here
  return obj


{-# INLINE set' #-}
set' :: Int -> (v -> a -> a) -> v -> Obj a -> ABS ()
set' i upd v _this@(ObjectRef ioref (COG (chan, _)) oid)  = do 
  astate@(AState _ om fm) <- S.get
  lift $ modifyIORef' ioref (upd v)
  let (maybeWoken, om') = M.updateLookupWithKey (\ _k _v -> Nothing) (oid, i) om
  fm' <- maybe (return fm)
        (\ woken -> lift $ updateWoken chan fm woken)
        maybeWoken
  S.put astate{aSleepingO = om', aSleepingF = fm'}

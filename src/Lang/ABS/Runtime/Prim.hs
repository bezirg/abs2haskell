-- | The ABS-language primitives/statements implemented as a Haskell-embedded DSL (eDSL)
module Lang.ABS.Runtime.Prim
    (
     -- * Basic ABS primitives
     skip, suspend, await, while, get, pro_get, pro_give, pro_new, pro_isempty, ifthenM, ifthenelseM, null
     -- * The run built-in method
     -- * The failure model
    ,throw, catches, finally, Exception, assert
    )
 where

import Lang.ABS.Runtime.Base

import Prelude hiding (null)
import qualified Data.List (null)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Chan (writeChan)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as S (get, put)
import Control.Concurrent.MVar
import Control.Monad.Coroutine hiding (suspend)
import Control.Monad.Coroutine.SuspensionFunctors (yield)
import qualified Control.Monad.Catch
import qualified Control.Exception (fromException, evaluate, AssertionFailed (..))
import qualified Data.Set as S (insert, toList, empty)
import Control.Distributed.Process.Serializable

skip :: ABS ()
skip = return ()

suspend :: ABS ()
suspend = yield S

await :: (Root_ o) => AwaitGuardCompiled -> Obj o -> ABS () 
await (FutureLocalGuard f@(FutureRef mvar _ _ )) _ = do
  empty <- liftIO $ isEmptyMVar mvar
  when empty $ do
    yield (FL f)

await g@(FutureFieldGuard i tg) this  = do
  f@(FutureRef mvar _ _) <- tg
  empty <- liftIO $ isEmptyMVar mvar
  when empty $ do
    yield (FF f i)
    await g this

await g@(AttrsGuard is tg) this = do
  check <- tg
  if not check
    then if Data.List.null is             -- no field-checks, so this will block indefinitely
         then do -- like suspend
           -- throw (return BlockedAwaitException) -- disabled the optimization for now and changed it to busy-waiting
           yield S
           await g this
         else do
           yield (A this is)
           await g this
    else return ()                 -- check succeeded, continue

await (PromiseLocalGuard p@(PromiseRef mvar regsvar _ _ )) (ObjectRef _ hereCOG _)  = do
  empty <- liftIO $ isEmptyMVar mvar
  when empty $ do
    mregs <- liftIO $ takeMVar regsvar
    case mregs of
      Nothing -> return ()        -- there was a race-condition, so we repair it with a maybe check
      Just scogs -> do
                 let scogs' = S.insert hereCOG scogs
                 liftIO $ putMVar regsvar (Just scogs')
                 yield (FL (proToFut p))
  
await g@(PromiseFieldGuard i tg) this@(ObjectRef _ hereCOG _)  = do
  p@(PromiseRef mvar regsvar _ _) <- tg
  empty <- liftIO $ isEmptyMVar mvar
  when empty $ do
    mregs <- liftIO $ takeMVar regsvar
    case mregs of
      Nothing -> return ()        -- there was a race-condition, so we repair it with a maybe check
      Just scogs -> do
                 let scogs' = S.insert hereCOG scogs
                 liftIO $ putMVar regsvar (Just scogs')
                 yield (FF (proToFut p) i)
                 await g this

await (left :&: rest) this = do
  await left this
  await rest this

-- | For use in awaitguard: a p$ becomes f?
proToFut :: Promise a -> Fut a
proToFut (PromiseRef valVar _regsVar creatorCog creatorCounter) = FutureRef valVar creatorCog creatorCounter


while :: ABS Bool -> ABS a -> ABS ()
while predAction loopAction = do
  res <- predAction
  when res (loopAction >> while predAction loopAction)

pro_give :: Serializable a => ABS (Promise a) -> ABS a -> ABS ()
pro_give aP aVal = do
  (PromiseRef valMVar regsMVar _creatorCog _creatorCounter) <- aP
  val <- aVal
  success <- liftIO $ tryPutMVar valMVar val
  unless success $ Control.Monad.Catch.throwM PromiseRewriteException -- already resolved promise
  Just cogs <- liftIO $ takeMVar regsMVar
  liftIO $ mapM_ (\ (COG (fcog, _ftid)) -> writeChan fcog (WakeupSignal val _creatorCog _creatorCounter)) (S.toList cogs)
  liftIO $ putMVar regsMVar Nothing

pro_new :: (Root_ o) => Obj o -> ABS (Promise a)
pro_new (ObjectRef _ hereCOG _) = do
  valMVar <- liftIO $ newEmptyMVar
  regsMVar <-liftIO $ newMVar (Just S.empty)
  astate@(AState{aCounter = __counter}) <- lift S.get
  lift (S.put (astate{aCounter = __counter + 1}))
  return (PromiseRef valMVar regsMVar hereCOG __counter)

get :: Fut f -> ABS f
get (FutureRef mvar _ _) = liftIO $ Control.Exception.evaluate =<< readMVar mvar 

pro_get :: Promise f -> ABS f
pro_get (PromiseRef mvar _ _ _) = liftIO $ Control.Exception.evaluate =<< readMVar mvar 

pro_isempty :: Promise f -> ABS Bool
pro_isempty (PromiseRef mvar _ _ _) = liftIO $ Control.Exception.evaluate =<< isEmptyMVar mvar 

-- forces the reading of the future-box to whnf, so when the future-box is opened (through get) then the remote future exception will be raised

-- for using inside ABS monad
ifthenM :: ABS Bool -> ABS () -> ABS ()
ifthenM texp stm_then = texp >>= (\ e -> when e stm_then)

-- for using inside ABS monad
ifthenelseM :: ABS Bool -> ABS b -> ABS b -> ABS b
ifthenelseM texp stm_then stm_else = texp >>= (\ e -> if e 
                                                     then stm_then
                                                     else stm_else)


-- instances to throwIO/try-catch-finally inside an ABS monad
instance (Functor s, Control.Monad.Catch.MonadThrow m) => Control.Monad.Catch.MonadThrow (Coroutine s m) where
  throwM e = lift $ Control.Monad.Catch.throwM e

instance (Functor s, Control.Monad.Catch.MonadCatch m) => Control.Monad.Catch.MonadCatch (Coroutine s m) where
  catch (Coroutine m) f = Coroutine $  m `Control.Monad.Catch.catch` \ e -> resume (f e)

instance (Functor s, Control.Monad.Catch.MonadMask m) => Control.Monad.Catch.MonadMask (Coroutine s m) where
  mask a = Coroutine $ Control.Monad.Catch.mask $ \u -> resume (a $ q u)
    where q u b = Coroutine $ u (resume b)
  uninterruptibleMask a =
    Coroutine $ Control.Monad.Catch.uninterruptibleMask $ \u -> resume (a $ q u)
      where q u b = Coroutine $ u (resume b)

-- | aliases for easier exporting
throw :: Control.Monad.Catch.Exception e => ABS e -> ABS a
throw e = e >>= Control.Monad.Catch.throwM

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catches :: ABS a -> [Control.Monad.Catch.Handler ABS (Maybe a)] -> ABS a
catches a hs = a `Control.Monad.Catch.catch` handler
  where
    handler e = foldr probe (Control.Monad.Catch.throwM e) hs
      where
        probe (Control.Monad.Catch.Handler h) xs = maybe xs (\ e -> h e >>= (\case
                                                                              Nothing -> xs -- trick to allow actual term pattern-matching
                                                                              Just res -> return res))
                                                                              (Control.Exception.fromException e)


finally :: ABS a -> ABS b -> ABS a
finally = Control.Monad.Catch.finally

type Exception = Control.Monad.Catch.SomeException

-- | The ABS assertions. 
assert :: ABS Prelude.Bool -> ABS ()
assert act = act Prelude.>>= \ pred -> when (Prelude.not pred) 
             (throw $ return $ Control.Exception.AssertionFailed "Assertion Failed")


-- | The reference to a null object
--
-- The class of a null object is "Null".
null :: Obj Null
null = NullRef


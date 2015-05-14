module Lang.ABS.Runtime.Prim
    (thisCOG, readThis, skip, suspend, await, while, get, ifthenM, ifthenelseM,
     throw, catches, finally, Exception,
     run_sync, run_async        -- the run wrappers for any object
    )
 where

import Lang.ABS.Runtime.Base

import Control.Monad (liftM, when)
import Data.IORef (readIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.Chan (writeChan)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.RWS as RWS (ask, get, put, withRWST)
import Control.Concurrent.MVar (isEmptyMVar, readMVar)
import Control.Monad.Coroutine hiding (suspend)
import Control.Monad.Coroutine.SuspensionFunctors (yield)
import qualified Control.Monad.Catch
import qualified Control.Exception (fromException, evaluate)

thisCOG :: (Object__ o) => ABS o COG
thisCOG = do
  t <- liftM aCOG $ lift RWS.ask
  return t

readThis :: (Object__ o) => ABS o o
readThis = do
  ObjectRef ioref _ _ <- liftM aThis $ lift RWS.ask
  liftIO $ readIORef ioref

skip :: (Object__ o) => ABS o ()
skip = return (())

suspend :: ABS o ()
suspend = yield S

await ::  (Object__ o) => AwaitGuard o -> ABS o () 
await (FutureGuard f@(FutureRef mvar _ _ ))  = do
  empty <- liftIO $ isEmptyMVar mvar
  when empty $ do
    yield (F f)

await g@(ThisGuard is tg) = do
  check <- tg
  if not check
    then if null is             -- no field-checks, so this will block indefinitely
         then throw (return BlockedAwaitException)
         else do
           AConf obj _ <- lift $ RWS.ask
           yield (T obj is)
           await g
    else return ()                 -- check succeeded, continue

await (left :&: rest) = do
  await left
  await rest

while :: (Object__ o) => ABS o Bool -> ABS o a -> ABS o ()
while predAction loopAction = do
  res <- predAction
  when res (loopAction >> while predAction loopAction)

get :: (Object__ o) => Fut f -> ABS o f
get (FutureRef mvar _ _) = liftIO $ Control.Exception.evaluate =<< readMVar mvar 
-- forces the reading of the future-box to whnf, so when the future-box is opened (through get) then the remote future exception will be raised

-- for using inside ABS monad
ifthenM :: Monad m => m Bool -> m () -> m ()
ifthenM texp stm_then = texp >>= (\ e -> when e stm_then)

-- for using inside ABS monad
ifthenelseM :: Monad m => m Bool -> m b -> m b -> m b
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

-- aliases for easier exporting
throw :: (Object__ o, Control.Monad.Catch.Exception e) => ABS o e -> ABS o a
throw e = e >>= Control.Monad.Catch.throwM

-- | Catches different sorts of exceptions. See "Control.Exception"'s 'ControlException.catches'
catches :: (Object__ o)  => ABS o a -> [Control.Monad.Catch.Handler (ABS o) (Maybe a)] -> ABS o a
catches a hs = a `Control.Monad.Catch.catch` handler
  where
    handler e = foldr probe (Control.Monad.Catch.throwM e) hs
      where
        probe (Control.Monad.Catch.Handler h) xs = maybe xs (\ e -> h e >>= (\case
                                                                              Nothing -> xs -- trick to allow actual term pattern-matching
                                                                              Just res -> return res))
                                                                              (Control.Exception.fromException e)


finally :: (Object__ o) => ABS o a -> ABS o b -> ABS o a
finally = Control.Monad.Catch.finally

type Exception = Control.Monad.Catch.SomeException


run_sync (AnyObject __obj@(ObjectRef __ioref _ _))
  = do __hereCOG <- thisCOG
       __obj1 <- liftIO (readIORef __ioref)
       otherCOG <- __cog __obj1
       when (not (__hereCOG == otherCOG))
         (error "Sync Call on a different COG detected")
       mapMonad (RWS.withRWST (\ r s -> ((\ aconf -> aconf{aThis = __obj}) r, s))) (__run __obj)
run_sync (AnyObject NullRef) = error "sync call to null"

run_async (AnyObject __obj@(ObjectRef __ioref _ _))
  = do __obj1 <- liftIO (readIORef __ioref)
       COG (__chan, _) <- __cog __obj1
       __mvar <- liftIO newEmptyMVar
       AConf{aCOG = __cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __cog __counter
       liftIO (writeChan __chan (RunJob __obj __f (__run __obj)))
       return __f
run_async (AnyObject NullRef) = error "async call to null"

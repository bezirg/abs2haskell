{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

-- Wrapper Module to include extra functions in the translated code
-- because they are called by the translated code

-- This wrapper module makes the translated code less verbose,
-- because we don't have to import more modules
-- jus this module

module Lang.ABS.Compiler.Include 
    (
     Control.Exception.evaluate, Data.Typeable.Typeable,  Prelude.error,
     Control.Monad.Trans.Class.lift, Control.Monad.liftM, Control.Monad.IO.Class.liftIO,
     Data.IORef.newIORef, Data.IORef.modifyIORef', Data.IORef.readIORef, Control.Monad.when, Control.Monad.Coroutine.mapMonad,
     Control.Concurrent.newChan, Control.Concurrent.writeChan, Control.Concurrent.writeList2Chan, Control.Concurrent.newEmptyMVar,
     Data.Map.Strict.updateLookupWithKey,
     Prelude.undefined,
     Prelude.fromIntegral, Prelude.Show, Prelude.show, Prelude.Eq, (Prelude.$),
     RWS.ask, RWS.get, RWS.put,
     Control.Monad.join, -- for applicative style method (sync and async) applications
     Control.Monad.Catch.Handler (..), PHandler (..), Control.Monad.Catch.Exception, Control.Monad.Catch.SomeException (..), caseEx,
     Control.Monad.Catch.fromException,
     withReaderT,
     newRef,writeRef,readRef, IORef, -- export also the type for type-checking
     empty_fut,
     initRemoteTable
    )
 where


import Prelude
import Control.Monad.Trans.Class
import qualified Control.Exception
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Data.IORef
import Control.Monad.Coroutine
import Control.Concurrent
import qualified Data.Map.Strict
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Typeable
import qualified Control.Monad.Catch
import qualified Control.Exception.Base (PatternMatchFail (..), throw)
import Control.Applicative
import Lang.ABS.Runtime.Base
import Lang.ABS.Runtime.Prim (thisCOG)
import Control.Distributed.Process.Node (initRemoteTable)

-- util function, used in code generation
withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r


-- | this is like a Control.Exception.Handler, but is only for running pure code. Used together with caseEx
data PHandler a = forall e . Control.Monad.Catch.Exception e => PHandler (e -> Maybe a)

-- | taken and modified from Control/Exception.hsa
caseEx :: Control.Monad.Catch.SomeException -> [PHandler a] -> a
caseEx e handlers = foldr tryHandler (Control.Exception.Base.throw $ Control.Exception.Base.PatternMatchFail "exception") handlers
    where tryHandler (PHandler handler) res
              = case Control.Monad.Catch.fromException e of
                Just e' -> case handler e' of -- we simulate pattern-match
                            Nothing -> res -- internal pattern match failed
                            Just x -> x    -- pattern-match succeeded
                Nothing -> res


newRef :: MonadIO m => m a -> m (IORef a)
newRef v = do
  res <- v
  liftIO $ newIORef res

writeRef :: MonadIO m => IORef a -> m a -> m ()
writeRef r v = do
  res <- v
  liftIO $ writeIORef r res

readRef :: MonadIO m => IORef a -> m a
readRef r = liftIO $ readIORef r

-- for easier code generation
empty_fut :: (Object__ o) => ABS o (Fut a)
empty_fut = FutureRef <$> liftIO newEmptyMVar <*> thisCOG <*> pure (-10 :: Int)

-- | Wrapper Module to include extra functions in the translated code
-- because they are called by the translated code
-- 
-- This wrapper module makes the translated code less verbose,
-- because we don't have to import more modules

{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

module Lang.ABS.Compiler.Include 
    (
     -- * Typeable must be automatically derived on all newly-created ABS exceptions (Haskell-compatible)
     Data.Typeable.Typeable,
     Prelude.error,
     -- * lifting State operations and IO operations (for both threads and cloudhaskell) to the ABS monad
     Control.Monad.Trans.Class.lift, Control.Monad.IO.Class.liftIO,

     -- * For sending actor messages (making asynchronous calls)
     Control.Concurrent.newChan, Control.Concurrent.writeChan, Control.Concurrent.newEmptyMVar,
     Data.Map.Strict.updateLookupWithKey,
     -- * mostly used as stub for uninitialized data
     Prelude.undefined,
     Prelude.fromIntegral, Prelude.Show, Prelude.show, Prelude.Eq, (Prelude.$),
     -- * For reading the runtime configuration "AConf" by "Prim"itive statements : this + thisCOG
     S.get, S.put, readThis,
     -- * Applicative style and monad utilities
     Control.Monad.when, Control.Monad.join,
     -- * Exceptions
     Control.Monad.Catch.Handler (..), PHandler (..), Control.Monad.Catch.Exception, Control.Monad.Catch.SomeException (..), caseEx,
     Control.Monad.Catch.fromException,

     -- * For creating objects (by the runtime)
     newIORef, readIORef, writeIORef, modifyIORef', 

     -- * For creating local variable (by the ABS user). It is the same as the above IORef-operations, but lifted for operating in the ABS-monad.
     newRef, readRef, writeRef, IORef,  -- this should remain in MonadIO monad for polymorphic (IO-fimported) code generation

     empty_fut, empty_pro,
     initRemoteTable,
     -- * for shortening code
     Prelude.maybe,
     -- * Haskell's 'negate' (-) unary function used in lifted code
     
     -- | Haskell's unary (-) has the same repr as the binary (-) subtract. When we have to lift code, we cannot use (-),
     -- because Haskell defaults to 'subtract'. Thus, instead, we explicitly lift and call the 'negate' function.
     Prelude.negate
    )
 where


import Prelude
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import Control.Concurrent
import qualified Data.Map.Strict
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Typeable
import qualified Control.Monad.Catch
import qualified Control.Exception.Base (PatternMatchFail (..), throw)
import Control.Applicative
import Lang.ABS.Runtime.Base
import Control.Distributed.Process.Node (initRemoteTable)

-- -- util function, used in code generation
-- withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
-- withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r


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


-- These are kept abstract (MonadIO), because we may have to interface with foreign code of IO
-- TODO: maybe SPECIALIZE

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
empty_fut :: (Root_ o) => Obj o -> ABS (Fut a)
empty_fut (ObjectRef _ hereCOG _) = FutureRef <$> liftIO newEmptyMVar <*> pure hereCOG <*> pure (-10 :: Int)

-- for easier code generation
empty_pro :: (Root_ o) => Obj o -> ABS (Promise a)
empty_pro (ObjectRef _ hereCOG _) = PromiseRef <$> liftIO newEmptyMVar <*> liftIO newEmptyMVar <*> pure hereCOG <*> pure (-11 :: Int)

-- | A way to de-reference an object-ref from the heap and read its attributes
--
-- _NOTE_: assumes the compiler is correct and __only__ passes the _this_ last formal-parameter of the method
readThis :: (Root_ o) => Obj o -> ABS o
readThis (ObjectRef ioref _ _) = liftIO $ readIORef ioref
readThis NullRef = error "Compile Error: this should not happen. Inform the developers"




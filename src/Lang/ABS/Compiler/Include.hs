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
     Control.Concurrent.newChan, Control.Concurrent.writeChan, Control.Concurrent.writeList2Chan, Control.Concurrent.newEmptyMVar,
     Data.Map.Strict.updateLookupWithKey,
     -- * mostly used as stub for uninitialized data
     Prelude.undefined,
     Prelude.fromIntegral, Prelude.Show, Prelude.show, Prelude.Eq, (Prelude.$),
     -- * For reading the runtime configuration "AConf" by "Prim"itive statements : this + thisCOG
     RWS.ask, RWS.get, RWS.put, thisCOG, readThis,
     -- * Applicative style and monad utilities
     Control.Monad.when, Control.Monad.Coroutine.mapMonad, Control.Monad.join,
     -- * Exceptions
     Control.Monad.Catch.Handler (..), PHandler (..), Control.Monad.Catch.Exception, Control.Monad.Catch.SomeException (..), caseEx,
     Control.Monad.Catch.fromException,
     withReaderT,

     -- * For creating objects (by the runtime)
     newIORef, readIORef, writeIORef, modifyIORef', 

     -- * For creating local variable (by the ABS user). It is the same as the above IORef-operations, but lifted for operating in the ABS-monad.
     newRef, readRef, writeRef, IORef,  -- export also the type for type-checking

     empty_fut,
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


newRef :: (Root_ o) => ABS o a -> ABS o (IORef a)
newRef v = do
  res <- v
  liftIO $ newIORef res

writeRef :: (Root_ o) => IORef a -> ABS o a -> ABS o ()
writeRef r v = do
  res <- v
  liftIO $ writeIORef r res

readRef :: (Root_ o) => IORef a -> ABS o a
readRef r = liftIO $ readIORef r

-- for easier code generation
empty_fut :: (Root_ o) => ABS o (Fut a)
empty_fut = FutureRef <$> liftIO newEmptyMVar <*> thisCOG <*> pure (-10 :: Int)


-- | A way to read the current executing COG inside an ABS process 
thisCOG :: (Root_ o) => ABS o COG
thisCOG = do
  t <- aCOG <$> lift RWS.ask
  return t


-- | A way to de-reference an object-ref from the heap and read its attributes
--
-- _NOTE_: assumes the compiler is correct and __only__ passes the _this_ last formal-parameter of the method
readThis :: (Root_ o) => Obj o -> ABS o o
readThis (ObjectRef ioref _ _) = liftIO $ readIORef ioref
readThis NullRef = error "Compile Error: this should not happen. Inform the developers"

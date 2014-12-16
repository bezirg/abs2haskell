{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

-- Wrapper Module to include extra functions in the translated code
-- because they are called by the translated code

-- This wrapper module makes the translated code less verbose,
-- because we don't have to import more modules
-- jus this module

module Lang.ABS.Compiler.Include 
    (
     Control.Exception.evaluate, Data.Typeable.Typeable,  Prelude.error,
     Control.Monad.Trans.Class.lift, Control.Monad.liftM,
     Data.IORef.newIORef, Data.IORef.modifyIORef', Data.IORef.readIORef, Control.Monad.when, Control.Monad.Coroutine.mapMonad,
     Control.Concurrent.newChan, Control.Concurrent.writeChan, Control.Concurrent.writeList2Chan, Control.Concurrent.newEmptyMVar,
     Data.Map.Strict.updateLookupWithKey,
     Prelude.undefined,
     (Prelude.=<<), (Prelude.>>=), Prelude.fromIntegral, Prelude.Show, Prelude.Eq, (Prelude.$),
     RWS.ask, RWS.get, RWS.put,
     Control.Monad.Catch.Handler (..), PHandler (..), Control.Monad.Catch.Exception, Control.Monad.Catch.SomeException (..), caseEx,
     withReaderT
    )
 where


import Prelude
import Control.Monad.Trans.Class
import qualified Control.Exception
import Control.Monad
import Data.IORef
import Control.Monad.Coroutine
import Control.Concurrent
import qualified Data.Map.Strict
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Typeable
import qualified Control.Monad.Catch
import qualified Control.Exception.Base (PatternMatchFail (..), throw)

-- util function, used in code generation
withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r



-- | this is like a Control.Exception.Handler, but is only for running pure code. Used together with caseEx
data PHandler a = forall e . Control.Monad.Catch.Exception e => PHandler (e -> Maybe a)

data InternalPatternMatchException = InternalPatternMatchException

-- | taken and modified from Control/Exception.hsa
caseEx :: Control.Monad.Catch.SomeException -> [PHandler a] -> a
caseEx e handlers = foldr tryHandler (Control.Exception.Base.throw $ Control.Exception.Base.PatternMatchFail "exception") handlers
    where tryHandler (PHandler handler) res
              = case Control.Monad.Catch.fromException e of
                Just e' -> case handler e' of -- we simulate pattern-match
                            Nothing -> res -- internal pattern match failed
                            Just x -> x    -- pattern-match succeeded
                Nothing -> res

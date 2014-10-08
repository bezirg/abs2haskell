{-# LANGUAGE NoImplicitPrelude #-}

-- Wrapper Module to include extra functions in the translated code
-- because they are called by the translated code

-- This wrapper module makes the translated code less verbose,
-- because we don't have to import more modules
-- jus this module

module Lang.ABS.Compiler.Include 
    (
     Prelude.return, Control.Exception.evaluate, Prelude.error,
     Control.Monad.Trans.Class.lift, Control.Monad.liftM,
     Data.IORef.newIORef, Data.IORef.modifyIORef', Data.IORef.readIORef, Control.Monad.when, Control.Monad.Coroutine.mapMonad,
     Control.Concurrent.newChan, Control.Concurrent.writeChan, Control.Concurrent.writeList2Chan, Control.Concurrent.newEmptyMVar,
     Data.Map.Strict.updateLookupWithKey,
     Prelude.undefined,
     (Prelude.=<<), (Prelude.>>=), Prelude.fromIntegral,
     withReaderT
    )
 where


import Prelude
import Control.Monad.Trans.Class
import Control.Exception
import Control.Monad
import Data.IORef
import Control.Monad.Coroutine
import Control.Concurrent
import Data.Map.Strict
import qualified Control.Monad.Trans.RWS as RWS

-- util function, used in code generation
withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r



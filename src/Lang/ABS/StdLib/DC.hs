{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module Lang.ABS.StdLib.DC where
import Lang.ABS.Runtime
import qualified Lang.ABS.Compiler.Include as I__
-- import Lang.ABS.StdLib -- removed
import Lang.ABS.Compiler.Include -- added
import Lang.ABS.StdLib.Prelude -- added
 
class (Object__ a) => IDC_ a where
         
        shutdown :: ObjectRef a -> ABS a Unit
         
        getLoad :: ObjectRef a -> ABS a (Triple Rat Rat Rat)
 
data IDC = forall a . (IDC_ a) => IDC (ObjectRef a)
 
instance Sub IDC IDC where
        up x = x
 
instance Sub (ObjectRef Null) IDC where
        up = IDC
 
instance Sub IDC AnyObject where
        up (IDC a) = AnyObject a
 
instance IDC_ Null where
        shutdown
          = I__.error
              "this should not happen. report the program to the compiler developers"
        getLoad
          = I__.error
              "this should not happen. report the program to the compiler developers"
__eqIDC (IDC NullRef) (IDC NullRef) = True
__eqIDC (IDC (ObjectRef _ id1 tid1)) (IDC (ObjectRef _ id2 tid2))
  = (id1 == id2) && (tid1 == tid2)
__eqIDC _ _ = False
 
instance I__.Eq IDC where
        (==) = __eqIDC
shutdown_sync ((IDC __obj@(ObjectRef __ioref _ _)))
  = do __hereCOG <- thisCOG
       __obj1 <- I__.readRef __ioref
       otherCOG <- __cog __obj1
       I__.when (not (__hereCOG == otherCOG))
         (I__.error "Sync Call on a different COG detected")
       I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
         (shutdown __obj)
shutdown_sync (IDC NullRef) = I__.error "sync call to null"
shutdown_async ((IDC __obj@(ObjectRef __ioref _ _)))
  = do __obj1 <- I__.readRef __ioref
       (__chan, _) <- __cog __obj1
       __mvar <- I__.liftIO I__.newEmptyMVar
       __hereCOG <- thisCOG
       astate@(AState{aCounter = __counter}) <- I__.lift I__.get
       I__.lift (I__.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __hereCOG __counter
       I__.liftIO
         (I__.writeChan __chan (RunJob __obj __f (shutdown __obj)))
       return __f
shutdown_async (IDC NullRef) = I__.error "async call to null"
getLoad_sync ((IDC __obj@(ObjectRef __ioref _ _)))
  = do __hereCOG <- thisCOG
       __obj1 <- I__.readRef __ioref
       otherCOG <- __cog __obj1
       I__.when (not (__hereCOG == otherCOG))
         (I__.error "Sync Call on a different COG detected")
       I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
         (getLoad __obj)
getLoad_sync (IDC NullRef) = I__.error "sync call to null"
getLoad_async ((IDC __obj@(ObjectRef __ioref _ _)))
  = do __obj1 <- I__.readRef __ioref
       (__chan, _) <- __cog __obj1
       __mvar <- I__.liftIO I__.newEmptyMVar
       __hereCOG <- thisCOG
       astate@(AState{aCounter = __counter}) <- I__.lift I__.get
       I__.lift (I__.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __hereCOG __counter
       I__.liftIO
         (I__.writeChan __chan (RunJob __obj __f (getLoad __obj)))
       return __f
getLoad_async (IDC NullRef) = I__.error "async call to null"

{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls, MultiParamTypeClasses, DeriveDataTypeable #-}

module Lang.ABS.Runtime.Base where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Control.Monad.Trans.RWS as RWS (RWST)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield)
import Data.Typeable
import qualified Control.Monad.Catch

-- its object value/memory a triple:
--1) a mutable state
--2) a unique-per-COG ascending counter being the object' identity inside the cog
--3) the thread identifier of its COG
-- Together 2 and 3 makes any object uniquely identified inside the same machine.
data ObjectRef a = ObjectRef (IORef a) Int ThreadId
                 | NullRef
                 deriving Eq

-- a future reference is a triple:
-- 1) The future value as an MVar. 
--    A call to ABS' get will try to read this MVar, making the COG block until the value is available (written back).
-- 2) The COG of the parent caller, to respond to.
-- 3) A unique-per-COG ascending counter being the future's identity inside the cog
-- Together 2 and 3 makes a future uniquely identified inside the same machine.
data Fut a = FutureRef (MVar a) COG Int
           |  TopRef            -- a dummy value to reply back to the main block

-- Subtyping-relation for ABS objects (as multiparam typeclass)
class Sub sub sup where
    up :: sub -> sup

instance Sub AnyObject AnyObject where
    up x = x

-- the root Object interface
-- all user-written interfaces implicitly extend this Object__ interface
class Object__ a where
    new :: (Object__ o) => a -> ABS o (ObjectRef a)
    new_local :: a -> (Object__ o) => ABS o (ObjectRef a)
    __init :: ObjectRef a -> ABS a () 
    __init _ = return (())     -- default implementation of init
    __run :: ObjectRef a -> ABS a () 
    __run _ = return (())        -- default implementation of run
    __cog :: (Object__ o) => a -> ABS o COG -- helper function for the generated code, to easily read from any object its COG location

-- the null class object, 
-- it does not have any constructors
-- it is just for typing
data Null

-- null error-implements the Root interface (and by code-generation all user-written interfaces)
instance Object__ Null where
    new = error "cannot instantiated null"
    new_local = error "cannot instantiated null"
    __cog = error "null is not related to a COG"


---- ABS monad related (object state world) -----------
-------------------------------------------------------

-- ABS pure-code operates in the haskell pure-world
-- whereas ABS effectful-code operates inside this ABS monad-stack
type ABS o = Coroutine (Yield AwaitOn) (RWS.RWST (AConf o) ()  AState IO)

-- every ABS monad (computation) holds a reader AConf and a state AState
data AConf o = AConf {
      aThis :: (Object__ o) => ObjectRef o, -- this object
      aCOG  :: COG                         -- this cog
    }
data AState = AState {
      aCounter :: Int,           -- generate (unique-per-COG) ascending counters
      aSleepingO :: ObjectMap,
      aSleepingF :: FutureMap
    }

-- the input to the await
-- can await on multiple items
data AwaitGuard o = forall b. FutureGuard (Fut b)
                  | ThisGuard [Int] (ABS o Bool)
                  | AwaitGuard o :&: AwaitGuard o

-- the yield result of the coroutine after calling suspend/await
-- the process yields that it awaits on 1 item (left-to-right of the compound awaitguard)
data AwaitOn = S -- suspend is called
             | forall f. F (Fut f) -- await on future
             | forall o. Object__ o => T (ObjectRef o) [Int] -- await on object's fields


--- COG related -----
---------------------

-- a COG is identified by its jobqueue+threadid
type COG = (Chan Job, ThreadId)

-- Incoming jobs to the COG thread
data Job = forall o a . Object__ o => RunJob (ObjectRef o) (Fut a) (ABS o a)
         | forall f . WakeupSignal (Fut f)

-- the two tables of every COG

type FutureMap = M.Map AnyFuture [Job] -- future => jobs

type ObjectMap = M.Map (Int, Int) [Job] -- object-id.field => jobs

-- Existential wrappers to have different futures and objects inside the same map 
data AnyFuture = forall a. AnyFuture (Fut a)
data AnyObject = forall o. Object__ o => AnyObject (ObjectRef o)

-- ordering futures inside the cog table
instance Eq AnyFuture where
    AnyFuture (FutureRef _ cid1 id1) == AnyFuture (FutureRef _ cid2 id2) = id1 == id2 && cid1 == cid2
    _ == _ = error "this should not happen: equality on topref"

instance Ord AnyFuture where
    compare (AnyFuture (FutureRef _ (_, tid1) id1)) (AnyFuture (FutureRef _ (_, tid2) id2)) = compare (tid1,id1) (tid2,id2)
    compare _ _ = error "this should not happen: ordering on topref"

-- ordering objects inside the cog table
-- it maybe can be used for root type equality if we expose to the ABS language the AnyObject interface type
instance Eq AnyObject where
    AnyObject (ObjectRef _ id1 tid1) == AnyObject (ObjectRef _ id2 tid2) = tid1 == tid2 && id1 == id2

-- code generation alias for object-reference equality
__eqAnyObject :: AnyObject -> AnyObject -> Bool
__eqAnyObject = (==)


-- builtin exceptions
data BlockedAwaitException = BlockedAwaitException
    deriving (Eq, Show, Typeable)

instance Control.Monad.Catch.Exception BlockedAwaitException

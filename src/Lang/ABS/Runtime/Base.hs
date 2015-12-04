-- | All the types and datastructures used in the ABS-Haskell runtime
{-# LANGUAGE ExistentialQuantification, EmptyDataDecls, MultiParamTypeClasses, DeriveDataTypeable #-}

module Lang.ABS.Runtime.Base where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Data.Set as S (Set)

-- for the ABS monad
import Control.Monad.Trans.State.Strict (StateT)

-- for exceptions
import qualified Control.Monad.Catch
import Data.Typeable -- exceptions must be typeable

-- * Objects and Futures

-- | A reference to any live object (a datatype record of its fields).
-- The reference is the following triple:
--
-- 1. a heap reference ("IORef") to the mutable state of the object (the record of its fields)
-- 3. a process-identifier for that thread. If the runtime is distributed, then it's the cog-forwarder. Else, parallel-only, so it is the COG itself.
-- 2. a unique-per-COG ascending counter being the object' identity inside the cog

--
-- Together 2 and 3 makes any object uniquely identified accross the network.
data Obj a = ObjectRef (IORef a) !COG !Int -- ^ an actual object reference
           | NullRef                              -- ^ reference to nothing; instead of referring to a predefined constant as done in C-like
             deriving Eq

-- | A reference to any live future.
--
-- The reference has the following structure (a triple):
-- 1. Potentially the resolved value of the future as an "MVar". 
-- A call to ABS' get will try to read this MVar, making the COG block until the value is available (written back).
-- 2. A reference to the COG of its parent caller, to reply the result to.
-- 3. A unique-per-COG ascending counter being the future's identity inside the cog
-- Together 2 and 3 makes a future uniquely identified across the network.
data Fut a = FutureRef (MVar a) !COG !Int -- ^ a reference to a future (created by await)
           | NullFutureRef              -- ^ a dummy (internal-only) future-reference where main-method returns to (when finished)

-- container, waiting-cogs, creator-cog, counter-id when created
data Promise a = PromiseRef (MVar a) (MVar (Maybe (S.Set COG))) !COG !Int 

instance Eq (Promise a) where
    PromiseRef _ _ c1 i1 == PromiseRef _ _ c2 i2 = c1 == c2 && i1 == i2

-- | Equality testing between futures
instance Eq (Fut a) where
    NullFutureRef == NullFutureRef = True
    FutureRef _ cog1 id1 == FutureRef _ cog2 id2 = cog1 == cog2 && id1 == id2
    _ == _ = False


-- | Subtyping-relation for ABS objects (as a multiparam typeclass)
class Sub sub sup where
    -- | The upcasting method from a subtype to a supertype
    up :: sub -> sup

instance Sub Root Root where
    up x = x

-- | The root-interface. (not exposed to the ABS user)
-- 
-- All user-written interfaces implicitly extend this "Root_" interface
--
-- The root interface requires at-least two "methods" to be implemented: 'new' and 'new_local'
-- 
-- Optionally the user can implement two extra "methods": 'init' (which corresponds to the init-block) and 'run'
--
-- NOTE: Although not directly exposed to the ABS user, it may potentially name-clash with a user-written "Root_" interface or class
-- All objects must be serializable (not only their references, but object records too, when we call new)
class Root_ a where
    __init :: Obj a -> (() -> ABS ()) -> ABS () 
    __init _ ret = ret ()       -- default implementation of init

-- | The root-type of all objects
--
-- This is the highest supertype in the subtyping hierarchy.
-- It is used only for object-equality between different-type objects
--
-- It is implemented as an existential-wrapper to hide the real class of the object
--
-- NOTE: Although not directly exposed to the ABS user, it may potentially name-clash with a user-written "Root" interface or class
data Root = forall o. Root_ o => Root (Obj o)

-- | Equality between any object (any class)
instance Eq Root where
    -- it maybe can be used for root type equality if we expose to the ABS language the AnyObject interface type
    Root (ObjectRef _ id1 tid1) == Root (ObjectRef _ id2 tid2) = tid1 == tid2 && id1 == id2
    Root NullRef == Root NullRef = True
    _ == _ = False

-- | Equality between any object (any class)
instance Ord Root where
    Root (ObjectRef _ id1 tid1) `compare` Root (ObjectRef _ id2 tid2) = (tid1,id1) `compare` (tid2,id2)
    Root NullRef `compare` Root NullRef = EQ
    Root NullRef `compare` Root _ = LT
    _ `compare` _ = GT

-- | The null class (datatype)
--
-- It does not have any constructors, thus no value (no fields).
-- It is used only for _typing_ the 'null' object and the the this-context of the main-block-method.
data Null

-- | The null-class error-implements the Root interface (and by code-generation all user-written interfaces)
instance Root_ Null

-- | Lifting un-interfaced objects to the Root interface
--
-- used on method calls. the this object is passed un-interfaced, so it can be upcasted to other interfaces than
-- just the interface belonging to the current method.
instance (Root_ a) => Sub (Obj a) Root where
    up = Root

-- * ABS monad related (object state world)

-- ** The ABS monad

-- | ABS pure-code operates in the haskell pure-world,
-- whereas ABS effectful-code operates inside this ABS monad-stack
--
-- | This "ABS" monad-stack is a pile of sub-monads:
--
-- 1. coroutine
-- 2. a reader configuration "AConf" that holds references to this-object and this-cog
-- 3. the state of the current COG "AState" to modify the COG-state-internals
-- 4. the Process monad (for remotely communicating messages between actors and doing other IO operations) 
type ABS = StateT AState IO

-- | Every ABS monad (computation) holds a state AState
data AState = AState {
      aCounter :: !Int           -- ^ generate (unique-per-COG) ascending counters
    , aSleepingO :: ObjectMap   -- ^ suspended-processes currently sleeping for some object-field
    , aSleepingF :: FutureMap   -- ^ suspended-processes currently sleeping for some future
    } 


-- | The single parameter to an await statement;
-- a non-recursive-datatype that can await on a future , promise or expression
data AwaitGuardCompiled b = FutureLocalGuard (Fut b)
                          | FutureFieldGuard !Int (ABS (Fut b))
                          | PromiseLocalGuard (Promise b)
                          | PromiseFieldGuard !Int (ABS (Promise b))
                          | AttrsGuard [Int] (ABS Bool)

-- * COG related

-- | a COG is identified by its jobqueue+processid
--
-- Implementation: ThreadId is needed to create an Ord instance of COG for the Set in the promise datatype
newtype COG = COG { fromCOG :: (Chan Job, ThreadId)}

instance Eq COG where
    COG (_,pid1) == COG (_,pid2) = pid1 == pid2

-- Used for creating (Set COG) for organizing promises listeners.
instance Ord COG where
    COG (_c1,p1) `compare` COG (_c2,p2) = p1 `compare` p2

-- | Incoming jobs to the COG thread
data Job = LocalJob (ABS ())
         | WakeupSignal !COG !Int

-- ** COG-held datastructures

-- | A mapping of object-fields to list of disabled processes.
--
-- It represents sleeping processes that wait on some object-field to be modified. e.g. await this.x > this.y +1
--
-- The COG will strictly not re-schedule this processes until the object-field is modified.
-- 
-- A mapping of (object-id,field-id) to list of suspended jobs
type ObjectMap = M.Map (Int, Int) [(ABS (), Maybe ((COG,Int), Int))] 

-- | A mapping of futures to list of disabled processes.
--
-- It represents sleeping procesess that wait on some futures to become resolved. e.g. await f?
--
-- The COG will strictly not re-schedule this processes until the future-key is resolved.
type FutureMap = M.Map (COG,Int) [(ABS (), Maybe ((Int, Int), Int))]

-- * Builtin Exception-constructors for ABS
data BlockedAwaitException = BlockedAwaitException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception BlockedAwaitException

-- | Trying to write to an already-resolved promise
data PromiseRewriteException = PromiseRewriteException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception PromiseRewriteException


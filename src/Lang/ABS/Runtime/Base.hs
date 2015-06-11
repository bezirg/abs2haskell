-- | All the types and datastructures used in the ABS-Haskell runtime
{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls, MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}

module Lang.ABS.Runtime.Base where

import Data.IORef (IORef)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Data.Set as S (Set)
import qualified Control.Monad.Trans.RWS as RWS (RWST)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield)
import Data.Typeable
import Control.Monad.Catch
import qualified Control.Distributed.Process as CH
import Control.Exception.Base (throwIO)
import Data.Binary
import GHC.Fingerprint ( Fingerprint(Fingerprint) )
import Control.Distributed.Process.Serializable
             ( fingerprint, Serializable, encodeFingerprint, decodeFingerprint )
import Data.Map ( Map, fromList, lookup )

-- * Objects and Futures

-- | A reference to any live object (a datatype record of its fields).
-- The reference is the following triple:
--
-- 1. a heap reference ("IORef") to the mutable state of the object (the record of its fields)
-- 2. a unique-per-COG ascending counter being the object' identity inside the cog
-- 3. a process-identifier for that thread. If the runtime is distributed, then it's the cog-forwarder. Else, parallel-only, so it is the COG itself.
--
-- Together 2 and 3 makes any object uniquely identified accross the network.
data Obj a = ObjectRef (IORef a) Int CH.ProcessId -- ^ an actual object reference
           | NullRef                              -- ^ reference to nothing; instead of referring to a predefined constant as done in C-like
                 deriving Eq --, Typeable)

-- instance Binary (ObjectRef a) where
--     put (ObjectRef _ i pid) = do
--       put (0 :: Word8) >> put i >> put pid
--     put NullRef = put (1 :: Word8)
--     get = do
--       t <- get :: Get Word8
--       case t of
--         1 -> return NullRef
--         0 -> do
--             i <- get
--             pid <- get
--             return (ObjectRef undefined i pid)

-- | A reference to any live future.
--
-- The reference has the following structure (a triple):
-- 1. Potentially the resolved value of the future as an "MVar". 
-- A call to ABS' get will try to read this MVar, making the COG block until the value is available (written back).
-- 2. A reference to the COG of its parent caller, to reply the result to.
-- 3. A unique-per-COG ascending counter being the future's identity inside the cog
-- Together 2 and 3 makes a future uniquely identified across the network.
data Fut a = FutureRef (MVar a) COG Int -- ^ a reference to a future (created by await)
           | MainFutureRef              -- ^ a dummy (internal-only) future-reference where main-method returns to (when finished)
           deriving Typeable

-- container, waiting-cogs, creator-cog, counter-id when created
data Promise a = PromiseRef (MVar a) (MVar (Maybe (S.Set COG))) COG Int 

instance Eq (Promise a) where
    PromiseRef _ _ c1 i1 == PromiseRef _ _ c2 i2 = c1 == c2 && i1 == i2

-- | Equality testing between futures
instance Eq (Fut a) where
    MainFutureRef == MainFutureRef = True
    FutureRef _ cog1 id1 == FutureRef _ cog2 id2 = cog1 == cog2 && id1 == id2
    _ == _ = False

-- | A future can be serialized.
instance Binary (Fut a) where
    put (FutureRef _ c i) = do
      put (0 :: Word8) >> put c >> put i
    put MainFutureRef = put (1 :: Word8)
    get = do
      t <- get :: Get Word8
      case t of
        1 -> return MainFutureRef
        0 -> do
            c <- get
            i <- get
            return (FutureRef undefined c i)
        _ -> error "binary Fut decoding"

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
class Root_ a where
    new :: (Root_ o) => a -> ABS o (Obj a)
    new_local :: a -> (Root_ o) => ABS o (Obj a)
    __init :: Obj a -> ABS a () 
    __init _ = return (())     -- default implementation of init
    __cog :: (Root_ o) => a -> ABS o COG -- helper function for the generated code, to easily read from any object its COG location

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

-- | (internal) an alias for code-geration for object equality
__eqRoot :: Root -> Root -> Bool
__eqRoot = (==)

-- | The null class (datatype)
--
-- It does not have any constructors, thus no value (no fields).
-- It is used only for _typing_ the 'null' object and the the this-context of the main-block-method.
data Null

-- | The null-class error-implements the Root interface (and by code-generation all user-written interfaces)
instance Root_ Null where
    new = error "cannot instantiated null"
    new_local = error "cannot instantiated null"
    __cog = error "null is not related to a COG"




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
type ABS o = Coroutine (Yield AwaitOn) (RWS.RWST (AConf o) ()  AState CH.Process)

-- | Every ABS monad (computation) holds a reader AConf and a state AState
data AConf o = AConf {
      aThis :: (Root_ o) => Obj o -- ^ this object
    , aCOG  :: COG               -- ^ this cog
    }
data AState = AState {
      aCounter :: Int           -- ^ generate (unique-per-COG) ascending counters
    , aSleepingO :: ObjectMap   -- ^ suspended-processes currently sleeping for some object-field
    , aSleepingF :: FutureMap   -- ^ suspended-processes currently sleeping for some future
    }

-- | The yield result of the a currently-executing coroutine after calling 'suspend' or 'await'
--
-- The running process yields that it awaits on 1 item (left-to-right of the compound awaitguard)
data AwaitOn = S -- suspend is called
             | forall f. Serializable f => FL (Fut f) -- await on a local future
             | forall f. Serializable f => FF (Fut f) Int -- await on field future
             | forall o. Root_ o => A (Obj o) [Int] -- await on object's fields


-- | The single parameter to an await statement;
-- a recursive-datatype that can await on multiple items
data AwaitGuardCompiled o = forall b. (Serializable b) => FutureLocalGuard (Fut b)
                          | forall b. (Serializable b) => FutureFieldGuard Int (ABS o (Fut b))
                          | forall b. (Serializable b) => PromiseLocalGuard (Promise b)
                          | forall b. (Serializable b) => PromiseFieldGuard Int (ABS o (Promise b))
                          | AttrsGuard [Int] (ABS o Bool)
                          | AwaitGuardCompiled o :&: AwaitGuardCompiled o






-- * COG related

-- | a COG is identified by its jobqueue+processid
newtype COG = COG { fromCOG :: (Chan Job, CH.ProcessId)}

instance Eq COG where
    COG (_,pid1) == COG (_,pid2) = pid1 == pid2

instance Binary COG where
    put (COG (_, pid)) = put pid
    get = do
      pid <- get
      return (COG (undefined, pid))

-- Used for creating (Set COG) for organizing promises listeners.
instance Ord COG where
    COG (_c1,p1) `compare` COG (_c2,p2) = p1 `compare` p2

-- | Incoming jobs to the COG thread
data Job = forall o a . (Serializable a, Root_ o) => RunJob (Obj o) (Fut a) (ABS o a)
         | forall f . Serializable f => WakeupSignal (Fut f)


-- | create this stub table
stable :: Map Fingerprint SomeGet
stable = fromList
    [ (mkSMapEntry (undefined :: Fut Bool))
    , (mkSMapEntry (undefined :: Fut [Int]))
    , (mkSMapEntry (undefined :: Fut [String]))
    ]

data SomeGet = forall a. Serializable a => SomeGet (Get (Fut a))

mkSMapEntry :: forall a. Serializable a => a -> (Fingerprint,SomeGet)
mkSMapEntry a = (fingerprint a,SomeGet (get :: Get (Fut a)))

-- | any-futures can be serialized
instance Binary AnyFut where
  put (AnyFut a) = put (encodeFingerprint$ fingerprint a) >> put a
  get = do
      fp<-get
      case Data.Map.lookup (decodeFingerprint fp) stable of
        Just (SomeGet someget) -> fmap (AnyFut) someget
        Nothing -> error "Binary AnyFut: fingerprint unknown"

-- ** COG-held datastructures

-- | A mapping of object-fields to list of disabled processes.
--
-- It represents sleeping procesess that wait on some object-field to be modified. e.g. await this.x > this.y +1
--
-- The COG will strictly not re-schedule this processes until the object-field is modified.
-- 
-- A mapping of (object-id,field-id) to list of suspended jobs
type ObjectMap = M.Map (Int, Int) [(Job, Maybe (AnyFut, Int))] 

-- | A mapping of futures to list of disabled processes.
--
-- It represents sleeping procesess that wait on some futures to become resolved. e.g. await f?
--
-- The COG will strictly not re-schedule this processes until the future-key is resolved.
type FutureMap = M.Map AnyFut [(Job, Maybe ((Int, Int), Int))]

-- | (internal-only) An existential-wrapper of futures to remove their contained type
--
-- Only internally-used as the key of the "FutureMap"
data AnyFut = forall a. Serializable a => AnyFut (Fut a)
               deriving Typeable

-- | Necessary instance for Map: Equality between "AnyFut"s
instance Eq AnyFut where
    AnyFut (FutureRef _ cid1 id1) == AnyFut (FutureRef _ cid2 id2) = id1 == id2 && cid1 == cid2
    _ == _ = error "this should not happen: equality on topref"

-- | Necessary instance for Map: Ordering between "AnyFut"s to use them as keys in the "FutureMap"
instance Ord AnyFut where
    compare (AnyFut (FutureRef _ (COG (_, tid1)) id1)) (AnyFut (FutureRef _ (COG (_, tid2)) id2)) = compare (tid1,id1) (tid2,id2)
    compare _ _ = error "this should not happen: ordering on topref"







-- * Builtin Exception-constructors for ABS
data BlockedAwaitException = BlockedAwaitException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception BlockedAwaitException

-- | Trying to write to an already-resolved promise
data PromiseRewriteException = PromiseRewriteException
    deriving (Eq, Show, Typeable)
instance Control.Monad.Catch.Exception PromiseRewriteException

-- Instances to lift exceptions for the "Process" monad



instance MonadThrow CH.Process where
    throwM = CH.liftIO . throwIO
    
instance MonadCatch CH.Process where
    catch  = CH.catch

instance MonadMask CH.Process where
    mask = CH.mask
    -- uninterruptibleMask, not provided by CH

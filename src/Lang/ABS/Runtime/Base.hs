-- | All the types and datastructures used in the ABS-Haskell runtime
{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls, MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving #-}

module Lang.ABS.Runtime.Base where

import Data.IORef (IORef)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map, fromList, lookup)
import qualified Data.Set as S (Set)
import Control.Monad.Trans.State.Strict (StateT)
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
import Control.Applicative

-- * Objects and Futures

-- | A reference to any live object (a datatype record of its fields).
-- The reference is the following triple:
--
-- 1. a heap reference ("IORef") to the mutable state of the object (the record of its fields)
-- 3. a process-identifier for that thread. If the runtime is distributed, then it's the cog-forwarder. Else, parallel-only, so it is the COG itself.
-- 2. a unique-per-COG ascending counter being the object' identity inside the cog

--
-- Together 2 and 3 makes any object uniquely identified accross the network.
data Obj a = ObjectRef (IORef a) COG Int -- ^ an actual object reference
           | NullRef                              -- ^ reference to nothing; instead of referring to a predefined constant as done in C-like
                 deriving Eq --, Typeable)

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
    new :: Root_ creator => a -> Obj creator -> ABS (Obj a)
    new_local :: Root_ creator => a -> Obj creator -> ABS (Obj a)
    __init :: Obj a -> ABS () 
    __init _ = return (())     -- default implementation of init

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
type ABS = Coroutine (Yield AwaitOn) (StateT AState CH.Process)

-- | Every ABS monad (computation) holds a reader AConf and a state AState
-- data AConf o = AConf {
--       aThis :: (Root_ o) => Obj o -- ^ this object
--     , aCOG  :: COG               -- ^ this cog
--     }
data AState = AState {
      aCounter :: Int           -- ^ generate (unique-per-COG) ascending counters
    , aSleepingO :: ObjectMap   -- ^ suspended-processes currently sleeping for some object-field
    , aSleepingF :: FutureMap   -- ^ suspended-processes currently sleeping for some future
    } deriving Typeable

-- | The yield result of the a currently-executing coroutine after calling 'suspend' or 'await'
--
-- The running process yields that it awaits on 1 item (left-to-right of the compound awaitguard)
data AwaitOn = S -- suspend is called
             | forall f. Serializable f => FL (Fut f) -- await on a local future
             | forall f. Serializable f => FF (Fut f) Int -- await on field future
             | forall o. Root_ o => A (Obj o) [Int] -- await on object's fields


-- | The single parameter to an await statement;
-- a recursive-datatype that can await on multiple items
data AwaitGuardCompiled = forall b. (Serializable b) => FutureLocalGuard (Fut b)
                          | forall b. (Serializable b) => FutureFieldGuard Int (ABS (Fut b))
                          | forall b. (Serializable b) => PromiseLocalGuard (Promise b)
                          | forall b. (Serializable b) => PromiseFieldGuard Int (ABS (Promise b))
                          | AttrsGuard [Int] (ABS Bool)
                          | AwaitGuardCompiled :&: AwaitGuardCompiled






-- * COG related

-- | a COG is identified by its jobqueue+processid
newtype COG = COG { fromCOG :: (Chan Job, CH.ProcessId)}

instance Eq COG where
    COG (_,pid1) == COG (_,pid2) = pid1 == pid2

-- Used for creating (Set COG) for organizing promises listeners.
instance Ord COG where
    COG (_c1,p1) `compare` COG (_c2,p2) = p1 `compare` p2

-- | Incoming jobs to the COG thread
data Job = forall o a . (Serializable a, Root_ o) => RunJob (Obj o) (Fut a) (ABS a)
         | forall a . (Serializable a) => WakeupSignal a COG Int


-- ** COG-held datastructures

-- | A mapping of object-fields to list of disabled processes.
--
-- It represents sleeping procesess that wait on some object-field to be modified. e.g. await this.x > this.y +1
--
-- The COG will strictly not re-schedule this processes until the object-field is modified.
-- 
-- A mapping of (object-id,field-id) to list of suspended jobs
type ObjectMap = M.Map (Int, Int) [(Job, Maybe ((COG,Int), Int))] 

-- | A mapping of futures to list of disabled processes.
--
-- It represents sleeping procesess that wait on some futures to become resolved. e.g. await f?
--
-- The COG will strictly not re-schedule this processes until the future-key is resolved.
type FutureMap = M.Map (COG,Int) [(Job, Maybe ((Int, Int), Int))]

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


instance Binary COG where
    put (COG (_, pid)) = put pid
    get = do
      pid <- get
      return (COG (undefined, pid))


instance Binary (Obj a) where
    put (ObjectRef _ cog i) = do
      put (0 :: Word8) >> put cog >> put i
    put NullRef = put (1 :: Word8)
    get = do
      t <- get :: Get Word8
      case t of
        1 -> return NullRef
        0 -> do
            cog <- get
            i <- get
            return (ObjectRef undefined cog i)
        _ -> error "Binary Obj: cannot decode object-ref"


deriving instance Typeable AwaitOn
deriving instance Typeable Yield
deriving instance Typeable Coroutine
deriving instance Typeable StateT


instance Binary Job where
    put (RunJob o f c) = do
                      put (0 :: Word8)
                      put f
                      -- put c
    put (WakeupSignal a c i) = do
                      put (1 :: Word8) 
                      put (encodeFingerprint$ fingerprint a) >> put a
                      put c
                      put i
    get = do
      t <- get :: Get Word8
      case t of
        0 -> undefined
            -- do
            -- o <- get
            -- f <- get
            -- c <- get
            -- return (RunJob o f undefined)
        1 -> do
            fp<-get
            case M.lookup (decodeFingerprint fp) stable of
                 Just (SomeGet someget) -> WakeupSignal <$> someget <*> get <*> get
                 Nothing -> error "Binary AnyFut: fingerprint unknown"

stable :: M.Map Fingerprint SomeGet
stable = M.fromList
    [ (mkSMapEntry (undefined :: Bool))
    , (mkSMapEntry (undefined :: [Int]))
    , (mkSMapEntry (undefined :: [String]))
    ]
    where
      mkSMapEntry :: forall a. Serializable a => a -> (Fingerprint,SomeGet)
      mkSMapEntry a = (fingerprint a,SomeGet (get :: Get a))
      
data SomeGet = forall a. Serializable a => SomeGet (Get a)


-- -- | any-futures can be serialized
-- instance Binary AnyFut where
--   put (AnyFut a) = put (encodeFingerprint$ fingerprint a) >> put a
--   get = do
--       fp<-get
--       case Data.Map.lookup (decodeFingerprint fp) stable of
--         Just (SomeGet someget) -> fmap (AnyFut) someget
--         Nothing -> error "Binary AnyFut: fingerprint unknown"

-- | create this stub table
-- stable :: Map Fingerprint SomeGet
-- stable = fromList
--     [ (mkSMapEntry (undefined :: Fut Bool))
--     , (mkSMapEntry (undefined :: Fut [Int]))
--     , (mkSMapEntry (undefined :: Fut [String]))
--     ]
--     where
--       mkSMapEntry :: forall a. Serializable a => a -> (Fingerprint,SomeGet)
--       mkSMapEntry a = (fingerprint a,SomeGet (get :: Get (Fut a)))
      
-- data SomeGet = forall a. Serializable a => SomeGet (Get (Fut a))



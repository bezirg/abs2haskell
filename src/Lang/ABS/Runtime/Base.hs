-- | All the types and datastructures used in the ABS-Haskell runtime
{-# LANGUAGE ExistentialQuantification, EmptyDataDecls, MultiParamTypeClasses, StandaloneDeriving, DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}

module Lang.ABS.Runtime.Base where

import Lang.ABS.Runtime.Conf
import Data.IORef (IORef)
import Control.Concurrent.MVar (MVar,newMVar, modifyMVar_, readMVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map, fromList, lookup, lookupGE, empty, insert)
import qualified Data.Set as S (Set)

-- for the ABS monad
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield)
import Control.Distributed.Process.Closure
import Control.Distributed.Static
import qualified Control.Distributed.Process as CH
import Control.Applicative -- for GHC <7.10

-- for exceptions
import Control.Monad.Catch
import Control.Exception.Base (throwIO)

-- for deriving binary,typeable=serializable
import Data.Typeable
import Data.Binary
import GHC.Generics (Generic)

-- for the foreign table trick and existential-wrapper manual binary instances
import Control.Distributed.Process.Serializable (Serializable,fingerprint,encodeFingerprint,decodeFingerprint, showFingerprint)
import Network.Transport (EndPointAddress)
import GHC.Fingerprint (Fingerprint)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Network.Transport.TCP (encodeEndPointAddress)


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
                 deriving (Eq, Typeable)

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
           deriving Typeable

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
class Serializable a => Root_ a where
    __init :: Obj a -> ABS () 
    __init _ = return ()       -- default implementation of init

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
    deriving (Generic, Typeable)

instance Binary Null

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
type ABS = Coroutine (Yield AwaitOn) (StateT AState CH.Process)

-- | Every ABS monad (computation) holds a state AState
data AState = AState {
      aCounter :: !Int           -- ^ generate (unique-per-COG) ascending counters
    , aSleepingO :: ObjectMap   -- ^ suspended-processes currently sleeping for some object-field
    , aSleepingF :: FutureMap   -- ^ suspended-processes currently sleeping for some future
    } deriving Typeable

-- | The yield result of the a currently-executing coroutine after calling 'suspend' or 'await'
--
-- The running process yields that it awaits on 1 item (left-to-right of the compound awaitguard)
data AwaitOn = S -- suspend is called
             | forall f. Serializable f => FL (Fut f) -- await on a local future
             | forall f. Serializable f => FF (Fut f) !Int -- await on field future
             | forall o. Root_ o => A (Obj o) [Int] -- await on object's fields


-- | The single parameter to an await statement;
-- a recursive-datatype that can await on multiple items
data AwaitGuardCompiled = forall b. (Serializable b) => FutureLocalGuard (Fut b)
                          | forall b. (Serializable b) => FutureFieldGuard !Int (ABS (Fut b))
                          | forall b. (Serializable b) => PromiseLocalGuard (Promise b)
                          | forall b. (Serializable b) => PromiseFieldGuard !Int (ABS (Promise b))
                          | AttrsGuard [Int] (ABS Bool)
                          | AwaitGuardCompiled :&: AwaitGuardCompiled

-- * COG related

-- | a COG is identified by its jobqueue+processid
newtype COG = COG { fromCOG :: (Chan Job, CH.ProcessId)}
              deriving Typeable

instance Eq COG where
    COG (_,pid1) == COG (_,pid2) = pid1 == pid2

-- Used for creating (Set COG) for organizing promises listeners.
instance Ord COG where
    COG (_c1,p1) `compare` COG (_c2,p2) = p1 `compare` p2

-- | Incoming jobs to the COG thread
data Job = forall o a . (Serializable a) => LocalJob (Obj o) (Fut a) (ABS a)
         | forall o a . (Serializable a) => RemotJob (Obj o) (Fut a) (CH.Closure (ABS a))
         | forall a . (Serializable a) => WakeupSignal a !COG !Int
         | MachineUp EndPointAddress !Int
         deriving Typeable

-- ** COG-held datastructures

-- | A mapping of object-fields to list of disabled processes.
--
-- It represents sleeping processes that wait on some object-field to be modified. e.g. await this.x > this.y +1
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

deriving instance Typeable AwaitOn
deriving instance Typeable Yield
deriving instance Typeable Coroutine
deriving instance Typeable StateT


instance Binary (Fut a) where
    put (FutureRef m c@(COG (_,pid)) i) = do -- we ignore the mvar with the value, it is a proxy
                          -- adds to the foreign table
                          if CH.processNodeId pid == myNodeId
                           then unsafePerformIO (modifyMVar_ fm (return . M.insert (c,i) (AnyMVar m)))  `seq` return ()
                           else return ()
                          put (0 :: Word8)
                          put pid
                          put i
    put NullFutureRef = put (1 :: Word8)
    get = do
      t <- get :: Get Word8
      case t of
        0 -> do
            unsafePerformIO (print "decoding future") `seq` return ()
            pid <- get
            i <- get
            let c = COG (undefined, pid)
            if CH.processNodeId pid == myNodeId
             then let m = unsafePerformIO (readMVar fm >>= return . M.lookupGE (c,i)) -- we lookup the table
                  in case m of
                  -- or use the safer Data.Typeable.cast, or Data.Typeable.eqT
                  Just ((c',i'), AnyMVar v) -> return (FutureRef (unsafeCoerce v) c' i)
                  _ -> error "foreign table lookup fail"

             else return (FutureRef undefined c i)
        1 -> return NullFutureRef
        _ -> error "Binary Fut: cannot decode future-ref"

instance Binary COG where
    put (COG (_, pid)) = put pid -- we ignore the chan
    get = do
      pid <- get                 -- TODO: here we put the associated chan of the forwarder
      return (COG (undefined, pid))

instance Binary (Obj a) where
    put (ObjectRef m c@(COG (_,pid)) i) = do -- we ignore the ioref with the value, it is a proxy
      -- adds to the foreign table
      if CH.processNodeId pid == myNodeId
        then unsafePerformIO (modifyMVar_ fm (return . M.insert (c,i) (AnyIORef m)))  `seq` return ()
        else return ()
      unsafePerformIO (print "putted objectref") `seq` put (0 :: Word8)
      put pid
      put i
    put NullRef = unsafePerformIO (print "putted nullref") `seq` put (1 :: Word8)
    get = do
      t <- get :: Get Word8
      case t of
        0 -> do
            unsafePerformIO (print "decoding object") `seq` return ()
            pid <- get
            i <- get
            unsafePerformIO (print "still ok") `seq` return ()
            let c = COG (undefined, pid)
            if CH.processNodeId pid == myNodeId
               then let m = unsafePerformIO (readMVar fm >>= return . M.lookupGE (c,i)) -- we lookup the table
                    in case m of
                         -- or use the safer Data.Typeable.cast, or Data.Typeable.eqT
                         Just ((c',i'), AnyIORef v) ->  unsafePerformIO (print "found foreign") `seq` return (ObjectRef (unsafeCoerce v :: IORef a) c' i)
                         _ -> unsafePerformIO (print "foreign table lookup fail") `seq` error "mplo"
               else unsafePerformIO (print "foreign object") `seq` return (ObjectRef undefined c i)
        1 -> unsafePerformIO (print "is nullref") `seq` return NullRef
        _ -> unsafePerformIO (print "Binary Obj: cannot decode object-ref") `seq` error "mplo"

instance Binary Job where
    put (RemotJob o f c) = do
                      put (0 :: Word8)
                      put (encodeFingerprint$ fingerprint c) >> put c
                      put o
                      put f
    put (WakeupSignal a c i) = do
                      put (1 :: Word8) 
                      put (encodeFingerprint$ fingerprint a) >> put a
                      put c
                      put i
    put (MachineUp nid fut_id) = do
      put (2 :: Word8)
      put nid
      put fut_id
    put (LocalJob _ _ _) = error "compiler error, LocalJob should not be transmitted."
    get = do
      t <- get :: Get Word8
      case t of
        0 -> do
            unsafePerformIO (print "decoding remotjob") `seq` return ()
            fp<-get
            unsafePerformIO (print ((showFingerprint (decodeFingerprint fp)) "")) `seq` return ()
            case M.lookup (decodeFingerprint fp) stable2 of
                 Just (SomeGet2 someget) -> do
                      unsafePerformIO (print $ typeRep someget) `seq` return ()
                      c <- someget
                      unsafePerformIO (print "done closure") `seq` return ()
                      o <- get
                      unsafePerformIO (print "done object") `seq` return()
                      f <- get
                      unsafePerformIO (print "done future") `seq` return ()
                      return (RemotJob o f c)
                 Nothing -> unsafePerformIO $ print "Binary remotjob: fingerprint unknown" `seq` error "mplo"
        1 -> do
            unsafePerformIO (print "decoding wakeupsignal") `seq` return ()
            fp<-get
            case M.lookup (decodeFingerprint fp) stable of
                 Just (SomeGet someget) -> do
                      unsafePerformIO (print "ok") `seq` return ()
                      WakeupSignal <$> someget <*> get <*> get
                 Nothing -> unsafePerformIO (print "Binary WakeUpsignal: fingerprint unknown") `seq` error "mplo"
        2 -> MachineUp <$> get <*> get
        _ -> error "Binary Job: cannot decode job"

stable2 :: M.Map Fingerprint SomeGet2
stable2 = M.fromList
    [ (mkSMapEntry (undefined :: Closure (ABS Bool)))
    , (mkSMapEntry (undefined :: Closure (ABS ())))
    , (mkSMapEntry (undefined :: Closure (ABS [Int])))
    , (mkSMapEntry (undefined :: Closure (ABS [String])))
    ]
    where
      mkSMapEntry :: forall a. Serializable a => Closure (ABS a) -> (Fingerprint,SomeGet2)
      mkSMapEntry a = (fingerprint a,SomeGet2 (get :: Get (Closure (ABS a))))

data SomeGet2 = forall a. Serializable a => SomeGet2 (Get (Closure (ABS a)))


stable :: M.Map Fingerprint SomeGet
stable = M.fromList
    [ (mkSMapEntry (undefined :: Bool))
    , (mkSMapEntry (undefined :: [Int]))
    , (mkSMapEntry (undefined :: Int))
    , (mkSMapEntry (undefined :: ()))
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

-- TODO exception existential serialization
instance Data.Binary.Binary SomeException where
    put _ = put "SomeException"
    get = return (Control.Monad.Catch.SomeException PromiseRewriteException) -- stub , TODO: fix


data AnyForeign = forall a. AnyMVar (MVar a)
                | forall a. AnyIORef (IORef a)

type ForeignMap = M.Map (COG,Int) AnyForeign

myNodeId :: CH.NodeId
myNodeId = CH.NodeId $ case (ip conf, port conf) of
             (Just ip', Just port') -> (encodeEndPointAddress ip' (show port') 0)
             _ -> encodeEndPointAddress "127.0.0.1" "9000" 0

fm :: MVar ForeignMap
fm = unsafePerformIO $ newMVar M.empty

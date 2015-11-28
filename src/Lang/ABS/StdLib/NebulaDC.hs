-- | An implementation of deployment components for OpenNebula clouds.
-- 
-- Since it is packaged in the ABS standard-library, the ABS user has to simply import it in an ABS program as:
-- 
-- > import NebulaDC
--
-- More about the open-source OpenNebula project at <http://opennebula.org>
{-# LANGUAGE NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses,
  PatternSignatures, DeriveDataTypeable, DeriveGeneric, InstanceSigs, FlexibleInstances #-}
module Lang.ABS.StdLib.NebulaDC where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import Lang.ABS.Runtime.Prim
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.StdLib
import OpenNebula hiding (main)
import Control.Distributed.Process (NodeId)
import Control.Distributed.Process.Internal.Types (nullProcessId) -- DC is under a null COG-process
import System.IO (readFile)
import Data.List (words)
import Prelude (Double(..), (++), elem, fmap)
import Text.Read (read)
import Prelude (toRational)
import System.Environment (getEnvironment, getArgs, getProgName)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Control.Concurrent (myThreadId)
import Network.Transport.TCP (encodeEndPointAddress)
import Data.IORef (newIORef)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary as B__
import Control.Distributed.Process.Closure
import Control.Distributed.Static
import Data.Rank1Typeable


-- * Internals

data NebulaDC = NebulaDC{
                         nebulaDC_cpu :: Int, nebulaDC_load :: Int, nebulaDC_memory :: Int,
                         nebulaDC_nid :: Fut NodeId, nebulaDC_vmId :: Int}
              deriving (I__.Typeable, I__.Generic)

__nebulaDC cpu memory
  = NebulaDC{nebulaDC_cpu = cpu, nebulaDC_memory = memory}
 
instance I__.Binary NebulaDC

instance I__.Root_ NebulaDC where
        -- new __cont this@(I__.ObjectRef _ __hereCOG _) 
        --   = do 
        --        __new_cog@(I__.COG (__chan,_)) <- I__.lift (I__.lift (I__.spawnCOG))
        --        let load = 0
        --        let vmId = (-1)
        --        let nodeId = Nothing
        --        let __c
        --              = __cont{nebulaDC_load = load, nebulaDC_vmId = vmId,
        --                       nebulaDC_nodeId = nodeId}
        --        __ioref <- I__.liftIO (I__.newIORef __c)
        --        let __obj = I__.ObjectRef __ioref __new_cog 0
        --        do __mvar <- I__.liftIO I__.newEmptyMVar
        --           __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
        --           I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
        --           let __f = I__.FutureRef __mvar __hereCOG __counter
        --           I__.liftIO (I__.writeChan __chan (I__.LocalJob __obj __f (I__.__init __obj)))
        --        return __obj
        -- new_local __cont __this
          -- = 
            -- REMOVED:
            -- do let load = 0
            --    let vmId = (-1)
            --    let nodeId = Nothing
            --    __thisCOG@(_, __tid) <- thisCOG
            --    let __c
            --          = __cont{nebulaDC_load = load, nebulaDC_vmId = vmId,
            --                   nebulaDC_nodeId = nodeId, nebulaDC_loc = return __thisCOG}
            --    __ioref <- I__.liftIO (I__.newIORef __c)
            --    __astate@(AState{aCounter = __counter}) <- I__.lift I__.get
            --    I__.lift (I__.put (__astate{aCounter = __counter + 1}))
            --    let __obj = ObjectRef __ioref __counter __tid
            --    I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
            --      (__init __obj)
            --    I__.mapMonad (I__.withReaderT (\ aconf -> aconf{aThis = __obj}))
            --      (__run __obj)
            --    return __obj
            
            -- ADDED
            -- I__.error "Local DC objects cannot be created (new local)"
        __init this@(I__.ObjectRef _ thisCOG@(I__.COG (_, pid)) _) = do
          NebulaDC { nebulaDC_cpu = newCpu, nebulaDC_memory = newMem } <- I__.readThis this

          -- create the ACK future  
          __mvar <- I__.liftIO I__.newEmptyMVar
          __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
          I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
          let __f = I__.FutureRef __mvar thisCOG __counter
          I__.set 1 (\ v__ c__ -> c__{nebulaDC_nid = v__}) __f this


          myProgName <- I__.liftIO getProgName
          let maybeNewTempl = cloneSlaveTemplate 
                              myTempl
                              newCpu
                              newMem
                              (C8.unpack (BS.toStrict (B64.encode (B__.encode __f)))) -- FROM_PID
                              myRpcServer
                              myRpcProxy
                              mySession
                              myProgName
          (success, vmId, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_allocate (fromJust maybeNewTempl)))
          ifthenM (pure (not success)) (I__.error "Allocating VM failed")
          I__.set 4 (\ v c -> c{nebulaDC_vmId=v}) vmId this
 
instance I__.Sub (I__.Obj NebulaDC) I__.Root where
        up = I__.Root
 
instance I__.Sub (I__.Obj NebulaDC) IDC where
        up = IDC
 
-- REMOVED the stub
-- instance IDC_ NebulaDC where
--         shutdown this = return ()
--         getLoad this
--           = do (pure ((,,)) <*> ((/) <$> pure 1 <*> pure 2) <*>
--                   ((/) <$> pure 1 <*> pure 2)
--                   <*> ((/) <$> pure 1 <*> pure 2))

-- ADDED FROM HERE ON:
instance IDC_ NebulaDC where
        shutdown this = do
                   NebulaDC { nebulaDC_vmId = thisVmId } <- I__.readThis this
                   (True, _, errCode) <- I__.liftIO (xmlrpc myRpcServer mySession (Just myRpcProxy) (vm_action "cancel" thisVmId))
                   return ()
        load this = do 
                   NebulaDC { nebulaDC_vmId = thisVmId } <- I__.readThis this
                   (s1: s5: s15: _) <- I__.liftIO (words <$!> (readFile "/proc/loadavg"))
                   return (toRational (read s1 :: Double), toRational (read s5 :: Double), toRational (read s15 :: Double))
        spawns :: forall o. (I__.Root_ o) => o -> I__.Obj NebulaDC -> I__.ABS (I__.Obj o)
        spawns smart this = do
             fnid <- nebulaDC_nid <$!> I__.readThis this
             nid <- get fnid
             println (pure "before spawn")
             s <- I__.lift (I__.lift (call' nid (I__.spawnClosure (staticLabel ((I__.show (typeOf (I__.undefined :: o))) ++ "__rootDict")) smart)))
             println(pure "after spawn")
             return s

{-# NOINLINE myTyp #-}
{-# NOINLINE myCreatorPid #-}
{-# NOINLINE myRpcServer #-}
{-# NOINLINE mySession #-}
{-# NOINLINE myRpcProxy #-}
{-# NOINLINE myTempl #-}
{-# NOINLINE myArgs #-}
(myTyp:myCreatorPid:myRpcServer:mySession:myRpcProxy:myTempl:myArgs) = 
    let env = unsafePerformIO getEnvironment
        args = unsafePerformIO getArgs
    in fmap (\ v -> fromMaybe "" (lookup v env))   ["TYPE","FROM_PID","RPC_SERVER","SESSION","RPC_PROXY","VM_TEMPLATE"] Prelude.++ args

{-# NONLINE myVmId #-}
myVmId = fromMaybe 
         (-1)                   -- signals erroneous extraction of VM ID
         (templateVmId myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code

-- {-# NONLINE myVmIP #-}
-- myVmIP = if "--distributed" `Prelude.elem` myArgs
--          then fromMaybe
--                   "127.0.0.1"
--                   (templateVmIP myTempl) -- it has to be top-level (global), so it can be used by other parts of the ABS translated code
--          else "127.0.0.1"

{-# NOINLINE thisDC #-}
thisDC = IDC (I__.ObjectRef (unsafePerformIO (newIORef (
                                                        NebulaDC{
                                                                 nebulaDC_cpu = -1, -- TODO
                                                                 nebulaDC_memory = -1, -- TODO
                                                                 nebulaDC_load = -1, -- it's for sim purposes
                                                                 nebulaDC_nid = I__.NullFutureRef, -- TODO
                                                                 nebulaDC_vmId = myVmId}
                                                       ))) 
              (I__.COG (I__.undefined,nullProcessId I__.myNodeId))         -- no processid (ForwarderCOG ID) associated with the DC object
              (-2))                   -- a stub object-id of the DC object

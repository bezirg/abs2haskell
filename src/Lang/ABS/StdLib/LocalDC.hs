{-# LANGUAGE NoImplicitPrelude, 
  ExistentialQuantification, MultiParamTypeClasses,
  PatternSignatures, DeriveDataTypeable, DeriveGeneric, TemplateHaskell, InstanceSigs, ScopedTypeVariables #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module Lang.ABS.StdLib.LocalDC where
import qualified Lang.ABS.Runtime.Base as I__
import Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import System.Process hiding (main)
import System.Environment (getExecutablePath)
import Data.Binary (encode)
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Network.Transport.TCP
import Control.Concurrent
import Control.Distributed.Static
import Data.Rank1Typeable
import Data.List ((++))

data LocalDC = LocalDC{localDC_nid :: NodeId, localDC_port :: Int}
             deriving (I__.Typeable, I__.Generic)

__localDC port = LocalDC{localDC_port = port}

instance I__.Binary LocalDC

instance I__.Root_ LocalDC where
        -- new __cont this@(I__.ObjectRef _ __thisCOG _)
        --   = do 
        --        __new_cog@(I__.COG (__chan,_)) <- I__.lift (I__.lift (I__.spawnCOG))
        --        let nid = I__.undefined
        --        let __c = __cont{localDC_nid = nid}
        --        __ioref <- I__.liftIO (I__.newIORef __c)
        --        let __obj = I__.ObjectRef __ioref __new_cog 0
        --        do __mvar <- I__.liftIO I__.newEmptyMVar
        --           __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
        --           I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
        --           let __f = I__.FutureRef __mvar __thisCOG __counter
        --           I__.liftIO
        --             (I__.writeChan __chan (I__.LocalJob __obj __f (I__.__init __obj)))
        --        return __obj
        -- new_local __cont this@(I__.ObjectRef _ __thisCOG _) = I__.error "Local DC objects cannot be created (new local)"
        --   = do let __c = __cont
        --        __ioref <- I__.liftIO (I__.newIORef __c)
        --        __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
        --        I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
        --        let __obj = I__.ObjectRef __ioref __thisCOG __counter
        --        I__.__init __obj
        --        return __obj
        __init this@(I__.ObjectRef _ (I__.COG (_, pid)) _) = do
          -- the creator runs the init
             port <- localDC_port <$!> I__.readThis this
             d0 <- I__.liftIO getExecutablePath
             I__.liftIO (createProcess ((proc d0 ["--port", I__.show port, "-t"]) { env = Just [("FROM_PID", I__.show (B64.encode (encode pid)))] }))
             return ()

 
instance I__.Sub (I__.Obj LocalDC) I__.Root where
        up = I__.Root
 
instance I__.Sub (I__.Obj LocalDC) IDC where
        up = IDC
 
instance IDC_ LocalDC where
        shutdown this = return ()
        getLoad this
          = do (pure ((,,)) <*> ((/) <$!> pure 1 <*> pure 2) <*>
                  ((/) <$!> pure 1 <*> pure 2)
                  <*> ((/) <$!> pure 1 <*> pure 2))
        spawns :: forall o. (I__.Root_ o) => Static (SerializableDict (I__.Obj o)) -> o -> I__.Obj LocalDC -> I__.ABS (I__.Obj o)
        spawns sdict smart this = do
             println(pure "in spawn")
             nid <- localDC_nid <$!> I__.readThis this
             let epa = encodeEndPointAddress "192.168.0.15" "9001" 1
             s <- I__.lift (I__.lift (call sdict (NodeId epa) (I__.spawnClosure (staticLabel ((I__.show (typeOf (I__.undefined :: o))) ++ "__rootDict")) smart)))
             println(pure "spawn done")
             return s



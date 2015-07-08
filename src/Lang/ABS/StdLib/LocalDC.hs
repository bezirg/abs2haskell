{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}
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
 
data LocalDC = LocalDC{localDC_nid :: NodeId, localDC_port :: Int}
__localDC port = LocalDC{localDC_port = port}

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
             println(pure "creating local node-process")
             println(pure (I__.show pid))
             port <- localDC_port <$> I__.readThis this
             d0 <- I__.liftIO getExecutablePath
             I__.liftIO (createProcess ((proc d0 ["--port", I__.show port]) { env = Just [("FROM_PID", I__.show (B64.encode (encode pid)))] }))
             return ()

 
set_localDC_nid :: NodeId -> I__.Obj LocalDC -> I__.ABS ()
set_localDC_nid v
  this@(I__.ObjectRef __thisIORef (I__.COG (__thisChan, _))
          __thisOid)
  = do __astate@(I__.AState _ om fm) <- I__.lift I__.get
       I__.liftIO
         (I__.modifyIORef' __thisIORef (\ c -> c{localDC_nid = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (__thisOid, 0) om
       fm' <- I__.maybe (return fm)
                (\ woken -> I__.liftIO (I__.updateWoken __thisChan fm woken))
                maybeWoken
       I__.lift
         (I__.put __astate{I__.aSleepingO = om', I__.aSleepingF = fm'})


set_localDC_port :: Int -> I__.Obj LocalDC -> I__.ABS ()
set_localDC_port v
  this@(I__.ObjectRef __thisIORef (I__.COG (__thisChan, _))
          __thisOid)
  = do __astate@(I__.AState _ om fm) <- I__.lift I__.get
       I__.liftIO
         (I__.modifyIORef' __thisIORef (\ c -> c{localDC_port = v}))
       let (maybeWoken, om')
             = I__.updateLookupWithKey (\ k v -> Nothing) (__thisOid, 0) om
       fm' <- I__.maybe (return fm)
                (\ woken -> I__.liftIO (I__.updateWoken __thisChan fm woken))
                maybeWoken
       I__.lift
         (I__.put __astate{I__.aSleepingO = om', I__.aSleepingF = fm'})
 
instance I__.Sub (I__.Obj LocalDC) I__.Root where
        up = I__.Root
 
instance I__.Sub (I__.Obj LocalDC) IDC where
        up = IDC
 
instance IDC_ LocalDC where
        shutdown this = return ()
        getLoad this
          = do (pure ((,,)) <*> ((/) <$> pure 1 <*> pure 2) <*>
                  ((/) <$> pure 1 <*> pure 2)
                  <*> ((/) <$> pure 1 <*> pure 2))
        spawns this obj = do
                        println(pure "in spawn")
                        nid <- localDC_nid <$> I__.readThis this
                        __new_cog@(I__.COG(_,pid)) <- I__.lift (I__.lift (call' nid $(mkStaticClosure 'spawnCOG)))
                        I__.lift (I__.lift (send pid (I__.InitJob obj)))
                        return (I__.ObjectRef I__.undefined __new_cog 0)

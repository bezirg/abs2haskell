{-# LANGUAGE Rank2Types, NoImplicitPrelude, FlexibleInstances,
  ExistentialQuantification, MultiParamTypeClasses,
  ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module LocalDC_stub (module LocalDC_stub) where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import System.Process hiding (main)
import System.Environment (getExecutablePath)
import Data.Binary (encode)
import Control.Distributed.Process (NodeId(..))
 
data LocalDC = LocalDC{localDC_nid :: NodeId, localDC_port :: Int}
__localDC port = LocalDC{localDC_port = port}
 
instance I__.Root_ LocalDC where
        new __cont this@(I__.ObjectRef _ __thisCOG _)
          = do __chan <- I__.liftIO I__.newChan
               __new_tid <- I__.lift (I__.lift (I__.spawnCOG __chan))
               let nid = I__.undefined
               let __c = __cont{localDC_nid = nid}
               __ioref <- I__.liftIO (I__.newIORef __c)
               let __obj = I__.ObjectRef __ioref (I__.COG (__chan, __new_tid)) 0
               do __mvar <- I__.liftIO I__.newEmptyMVar
                  __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
                  I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
                  let __f = I__.FutureRef __mvar __thisCOG __counter
                  I__.liftIO
                    (I__.writeChan __chan (I__.LocalJob __obj __f (I__.__init __obj)))
               return __obj
        new_local __cont this@(I__.ObjectRef _ __thisCOG _)
          = do let nid = I__.undefined
               let __c = __cont{localDC_nid = nid}
               __ioref <- I__.liftIO (I__.newIORef __c)
               __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift I__.get
               I__.lift (I__.put (__astate{I__.aCounter = __counter + 1}))
               let __obj = I__.ObjectRef __ioref __thisCOG __counter
               I__.__init __obj
               return __obj
        __init this = return ()
 
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
             = I__.updateLookupWithKey (\ k v -> Nothing) (__thisOid, 1) om
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
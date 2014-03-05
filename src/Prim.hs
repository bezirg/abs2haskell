module Prim where

import Base

import Control.Monad (liftM, when)
import Data.IORef (readIORef)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.RWS as RWS (ask)
import Control.Concurrent.MVar (isEmptyMVar, readMVar)
import Control.Monad.Coroutine.SuspensionFunctors (yield)

thisCOG :: (Object__ o) => ABS o COG
thisCOG = do
  t <- liftM aCOG $ lift RWS.ask
  return t

readThis :: (Object__ o) => ABS o o
readThis = do
  ObjectRef ioref _ <- liftM aThis $ lift RWS.ask
  lift $ lift $ readIORef ioref

skip :: (Object__ o) => ABS o ()
skip = return (())

suspend :: ABS o ()
suspend = yield S

await ::  (Object__ o) => AwaitGuard o -> ABS o () 
await (FutureGuard f@(FutureRef mvar _ _ _))  = do
  empty <- lift $ lift $ isEmptyMVar mvar
  when empty $ do
    yield (F f)

await (FutureGuard f@(FutureRef mvar _ _ _) :&: gs)  = do
  empty <- lift $ lift $ isEmptyMVar mvar
  when empty $ do
    yield (F f)
  await gs


await g@(ThisGuard is tg) = do
  check <- tg
  when (not check) $ do
       AConf obj _ _ <- lift $ RWS.ask
       yield (T obj is)
       await g

await gs@(ThisGuard is tg :&: rest) = do
  check <- tg
  when (not check) $ do
       AConf obj _ _ <- lift $ RWS.ask
       yield (T obj is)
       await gs
  await rest

while :: ABS o Bool -> ABS o () -> ABS o ()
while pred action = do
  res <- pred
  when res $ do
      action
      while pred action

get :: (Object__ o) => Fut f -> ABS o f
get a = (\ (FutureRef mvar _ _ _) -> lift $ lift $ readMVar mvar) a

-- readObject :: (Object__ o) => ObjectRef f -> ABS o f
-- readObject (ObjectRef ioref _) = lift $ lift $ readIORef ioref

{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, PatternSignatures, FlexibleContexts,
  DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module LB_adap_remote (main) where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import qualified Data.Binary as B__
import NebulaDC hiding (main)
import Control.Concurrent (threadDelay)
import System.IO (readFile)
import Data.List (words)
import Prelude (toRational, Rational, Double, read)
import qualified Control.Distributed.Process as CH
import Control.Distributed.Process.Closure
 
-- added
import qualified Prelude (read, Double,(/),(!!))
import Data.List (words)
import Data.IORef (atomicModifyIORef', readIORef)
import System.IO (readFile)
import Control.Monad(forever)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Distributed.Process.Internal.Types (nullProcessId, NodeId (..))
import Network.Transport.TCP (encodeEndPointAddress)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef)

{-# NOINLINE nid1 #-}
nid1 :: Fut CH.ProcessId
nid1 = I__.FutureRef (unsafePerformIO (newMVar (nullProcessId (NodeId (encodeEndPointAddress "" "9000" 0))))) (I__.COG I__.undefined) (-1)

{-# NOINLINE nid2 #-}
nid2 :: Fut CH.ProcessId
nid2 = I__.FutureRef (unsafePerformIO (newMVar (nullProcessId (NodeId (encodeEndPointAddress "" "9000" 0))))) (I__.COG I__.undefined) (-1)


{-# NOINLINE __allocdcs #-}
__allocdcs :: [IDC]
__allocdcs = [IDC (I__.ObjectRef (unsafePerformIO (newIORef (NebulaDC (-1) (-1) (-1) nid1 (-1)))) (I__.COG I__.undefined) (-1))
             ,IDC (I__.ObjectRef (unsafePerformIO (newIORef (NebulaDC (-1) (-1) (-1) nid2 (-1)))) (I__.COG I__.undefined) (-1))
             ] 

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

type Req = Int
 
type Res = Int
 
data Heartbeater = Heartbeater{heartbeater_farm :: List Server, heartbeater_b :: Balancer}
                 deriving (I__.Typeable, I__.Generic)
__heartbeater farm b = Heartbeater{heartbeater_farm = farm, heartbeater_b = b}

data C = C{}
       deriving (I__.Typeable, I__.Generic)
__c = C{}
 
instance I__.Root_ C
 
instance B__.Binary C

data S = S{s_b :: Balancer}
       deriving (I__.Typeable, I__.Generic)
__s b = S{s_b = b}
 
instance I__.Root_ S
 
instance B__.Binary S


class (I__.Root_ a) => Balancer_ a where
         
        forwardReq :: Client -> Req -> I__.Obj a -> I__.ABS Unit
         
        forwardRes :: Client -> Res -> I__.Obj a -> I__.ABS Unit
         
        addToFarm :: Server -> I__.Obj a -> I__.ABS Unit
 
data Balancer = forall a . (Balancer_ a) => Balancer (I__.Obj a)
 
instance I__.Show Balancer where
        show _ = "Balancer"
 
instance I__.Sub Balancer Balancer where
        up x = x
 
instance (Balancer_ a) => I__.Sub (I__.Obj a) Balancer where
        up = Balancer
 
instance I__.Sub Balancer I__.Root where
        up (Balancer a) = I__.Root a
 
instance Balancer_ I__.Null where
        forwardReq
          = I__.error
              "this should not happen. report the program to the compiler developers"
        forwardRes
          = I__.error
              "this should not happen. report the program to the compiler developers"
        addToFarm
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq Balancer where
        Balancer (I__.ObjectRef _ id1 tid1) ==
          Balancer (I__.ObjectRef _ id2 tid2)
          = (id1 == id2) && (tid1 == tid2)
        Balancer I__.NullRef == Balancer I__.NullRef = True
        _ == _ = False
 
instance I__.Ord Balancer where
        compare (Balancer (I__.ObjectRef _ id1 tid1))
          (Balancer (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (Balancer I__.NullRef) (Balancer I__.NullRef) = I__.EQ
        compare (Balancer I__.NullRef) (Balancer _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary Balancer where
        put (Balancer x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_Balancer of
                   Just (SomeGet_Balancer someget) -> Balancer <$!> someget
                   Nothing -> I__.error "Binary Balancer: fingerprint unknown"
stable_Balancer
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj B))]
  where  
        mkSMapEntry ::
                    forall a . (Balancer_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_Balancer)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_Balancer (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_Balancer = forall a .
                          (Balancer_ a, I__.Serializable a) =>
                          SomeGet_Balancer (B__.Get (I__.Obj a))
forwardReq__remote (c, r, (Balancer this)) = forwardReq c r this
forwardRes__remote (c, r, (Balancer this)) = forwardRes c r this
addToFarm__remote (s, (Balancer this)) = addToFarm s this
 
class (I__.Root_ a) => Server_ a where
         
        serve :: Client -> Req -> I__.Obj a -> I__.ABS Unit
         
        server_load :: I__.Obj a -> I__.ABS Rat
 
data Server = forall a . (Server_ a) => Server (I__.Obj a)
 
instance I__.Show Server where
        show _ = "Server"
 
instance I__.Sub Server Server where
        up x = x
 
instance (Server_ a) => I__.Sub (I__.Obj a) Server where
        up = Server
 
instance I__.Sub Server I__.Root where
        up (Server a) = I__.Root a
 
instance Server_ I__.Null where
        serve
          = I__.error
              "this should not happen. report the program to the compiler developers"
        server_load
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq Server where
        Server (I__.ObjectRef _ id1 tid1) ==
          Server (I__.ObjectRef _ id2 tid2) = (id1 == id2) && (tid1 == tid2)
        Server I__.NullRef == Server I__.NullRef = True
        _ == _ = False
 
instance I__.Ord Server where
        compare (Server (I__.ObjectRef _ id1 tid1))
          (Server (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (Server I__.NullRef) (Server I__.NullRef) = I__.EQ
        compare (Server I__.NullRef) (Server _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary Server where
        put (Server x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_Server of
                   Just (SomeGet_Server someget) -> Server <$!> someget
                   Nothing -> I__.error "Binary Server: fingerprint unknown"
stable_Server
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj S))]
  where  
        mkSMapEntry ::
                    forall a . (Server_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_Server)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_Server (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_Server = forall a . (Server_ a, I__.Serializable a) =>
                        SomeGet_Server (B__.Get (I__.Obj a))
serve__remote (c, r, (Server this)) = serve c r this
server_load__remote ((Server this)) = server_load this
 
class (I__.Root_ a) => Client_ a where
         
        respond :: Res -> I__.Obj a -> I__.ABS Unit
 
data Client = forall a . (Client_ a) => Client (I__.Obj a)
 
instance I__.Show Client where
        show _ = "Client"
 
instance I__.Sub Client Client where
        up x = x
 
instance (Client_ a) => I__.Sub (I__.Obj a) Client where
        up = Client
 
instance I__.Sub Client I__.Root where
        up (Client a) = I__.Root a
 
instance Client_ I__.Null where
        respond
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq Client where
        Client (I__.ObjectRef _ id1 tid1) ==
          Client (I__.ObjectRef _ id2 tid2) = (id1 == id2) && (tid1 == tid2)
        Client I__.NullRef == Client I__.NullRef = True
        _ == _ = False
 
instance I__.Ord Client where
        compare (Client (I__.ObjectRef _ id1 tid1))
          (Client (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (Client I__.NullRef) (Client I__.NullRef) = I__.EQ
        compare (Client I__.NullRef) (Client _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary Client where
        put (Client x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_Client of
                   Just (SomeGet_Client someget) -> Client <$!> someget
                   Nothing -> I__.error "Binary Client: fingerprint unknown"
stable_Client
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj C))]
  where  
        mkSMapEntry ::
                    forall a . (Client_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_Client)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_Client (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_Client = forall a . (Client_ a, I__.Serializable a) =>
                        SomeGet_Client (B__.Get (I__.Obj a))
respond__remote (r, (Client this)) = respond r this
 
data B = B{b_farm :: List Server, b_initFarmSize :: !Int,
           b_rr :: !Int}
       deriving (I__.Typeable, I__.Generic)
__b initFarmSize
  = (\ farm ->
       (\ rr ->
          B{b_farm = farm, b_initFarmSize = initFarmSize, b_rr = rr})
         0)
      []
 
instance I__.Root_ B where
        __init this
          = do i :: I__.IORef Int <- I__.newRef
                                       (I__.readThis this >>=
                                          \ B{b_initFarmSize = __initFarmSize} ->
                                            (pure (I__.fromIntegral __initFarmSize)))
               dcs :: I__.IORef (List IDC) <- I__.newRef (pure [])
               dc :: I__.IORef IDC <- I__.newRef (pure (I__.up null))
               while ((>) <$!> (I__.fromIntegral <$!> I__.readRef i) <*> pure 0)
                 (do I__.writeRef dc (nth <$!> pure __allocdcs <*> I__.readRef i)
                       -- (IDC <$!>
                       --    (I__.join
                       --       (I__.new_local <$!> (pure __nebulaDC <*> pure 1 <*> pure 8192) <*> pure this)))
                     I__.writeRef dcs
                       ((:) <$!> ((I__.up <$!> I__.readRef dc)) <*> ((I__.readRef dcs)))
                     I__.writeRef i
                       ((-) <$!> (I__.fromIntegral <$!> I__.readRef i) <*> pure 1)
                     return ())
               s :: I__.IORef Server <- I__.newRef (pure (I__.up null))
               while (not <$!> ((==) <$!> ((I__.readRef dcs)) <*> pure []))
                 (do I__.writeRef dc ((pure head <*> ((I__.readRef dcs))))
                     I__.writeRef s
                       (Server <$!>
                          I__.join
                            ((\ (IDC __dc) ->
                                (I__.join (spawns <$!> (pure __s <*> pure (I__.up this)))) __dc)
                               <$!> ((I__.up <$!> I__.readRef dc))))
                     (I__.join
                        (I__.set 0 (\ v__ c__ -> c__{b_farm = v__}) <$!>
                           (I__.readThis this >>=
                              \ B{b_farm = __farm} ->
                                ((:) <$!> ((I__.up <$!> I__.readRef s)) <*> (pure __farm)))
                             <*> pure this))
                     I__.writeRef i
                       ((+) <$!> (I__.fromIntegral <$!> I__.readRef i) <*> pure 1)
                     I__.writeRef dcs ((pure tail <*> ((I__.readRef dcs))))
                     return ())
               (I__.readThis this >>=
                  \ B{b_farm = __farm} ->
                    (I__.join (I__.new <$!> (pure __heartbeater <*> (pure __farm) <*> pure (I__.up this)))))
               return ()
 
instance B__.Binary B
 

load__remote :: () -> CH.Process Rat
load__remote () = do
   (s1: s5: s15: _) <- I__.liftIO (words <$!> (readFile "/proc/loadavg"))
   return (toRational (read s1 :: Double))


I__.remotable
  ['forwardReq__remote, 'forwardRes__remote, 'addToFarm__remote,
   'respond__remote, 'serve__remote, 'server_load__remote, 'load__remote]

instance Balancer_ B where
        forwardReq c req this
          = do (I__.readThis this >>=
                  \ B{b_farm = __farm, b_rr = __rr} ->
                    (I__.join
                       ((\ __wrap@(Server __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                           ->
                           if I__.processNodeId __pid == I__.myNodeId then
                             I__.join
                               (this ^!! __obj <$!>
                                  (pure serve <*> (pure (I__.up c)) <*> (pure req)))
                             else
                             do __args <- (,,) <$!>
                                            (pure (c)) <*> (pure req) <*> pure __wrap
                                (^@@) this __obj ($( I__.mkClosure 'serve__remote ) __args))
                          <$!>
                          (((pure nth <*> (pure __farm)) <*>
                              (pure (I__.fromIntegral __rr)))))))
               (I__.join
                  (I__.set 2 (\ v__ c__ -> c__{b_rr = v__}) <$!>
                     (I__.readThis this >>=
                        \ B{b_rr = __rr, b_farm = __farm} ->
                          (ifthenelseM
                             ((>=) <$!> (pure (I__.fromIntegral __rr)) <*>
                                ((-) <$!> ((pure length <*> (pure __farm))) <*> pure 1))
                             (pure 0)
                             ((+) <$!> (pure (I__.fromIntegral __rr)) <*> pure 1)))
                       <*> pure this))
               return ()
        forwardRes c rsp this
          = do (I__.join
                  ((\ __wrap@(Client __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                      ->
                      if I__.processNodeId __pid == I__.myNodeId then
                        I__.join (this ^!! __obj <$!> (pure respond <*> (pure rsp))) else
                        do __args <- (,) <$!> (pure rsp) <*> pure __wrap
                           (^@@) this __obj ($( I__.mkClosure 'respond__remote ) __args))
                     <$!> (pure (I__.up c))))
               return ()
        addToFarm s this
          = do (I__.join
                  (I__.set 0 (\ v__ c__ -> c__{b_farm = v__}) <$!>
                     (I__.readThis this >>=
                        \ B{b_farm = __farm} ->
                          ((:) <$!> (pure (I__.up s)) <*> (pure __farm)))
                       <*> pure this))
               return ()
 
 
instance I__.Root_ Heartbeater where
        __init this
          = do return ()
               (this ^!! this) run
          where run this
                  = do farm' :: I__.IORef (List Server) <- I__.newRef
                                                             (I__.readThis this >>=
                                                                \ Heartbeater{heartbeater_farm =
                                                                                __farm}
                                                                  -> (pure __farm))
                       s :: I__.IORef Server <- I__.newRef (pure (I__.up null))
                       --f :: I__.IORef (Fut Rat) <- I__.newRef (I__.empty_fut this)
                       r :: I__.IORef Rat <- I__.newRef (pure 0)
                       sum :: I__.IORef Rat <- I__.newRef (pure 0)
                       while (pure True)
                         (do 
			     println(pure("beat"))
			     I__.writeRef farm'
                               (I__.readThis this >>=
                                  \ Heartbeater{heartbeater_farm = __farm} -> (pure __farm))
		             I__.writeRef sum (pure 0)
                             while (not <$!> ((==) <$!> ((I__.readRef farm')) <*> pure []))
                               (do I__.writeRef s ((pure head <*> ((I__.readRef farm'))))
                                   --I__.writeRef f
                                   -- (I__.join
                                   --    ((\ __wrap@(Server
                                   --                  __obj@(I__.ObjectRef _ (I__.COG (_, __pid))
                                   --                           _))
                                   --        ->
                                   --        if I__.processNodeId __pid == I__.myNodeId then
                                   --          I__.join (this ^! __obj <$!> (pure server_load)) else
                                   --          do __args <- pure __wrap
                                   --             (^@) this __obj
                                   --               ($( I__.mkClosure 'server_load__remote ) __args))
                                   --       <$!> ((I__.up <$!> I__.readRef s))))
				   I__.writeRef r (I__.join ((\ (Server (I__.ObjectRef _ (I__.COG (_, pid)) _)) -> I__.lift (I__.lift (CH.call $(functionTDict 'load__remote) (CH.processNodeId pid) ($(mkClosure 'load__remote) ())))) <$!> I__.readRef s))
                                   -- I__.join
                                   --   (await <$!>
                                   --      (I__.FutureLocalGuard <$!> ((I__.readRef f))) <*> pure this)
                                   -- I__.writeRef r (get =<< ((I__.readRef f)))
                                   I__.writeRef sum
                                     ((+) <$!> ((I__.readRef sum)) <*> ((I__.readRef r)))
                                   I__.writeRef farm' ((pure tail <*> ((I__.readRef farm'))))
                                   return ())
                             I__.writeRef r
                               (I__.readThis this >>=
                                  \ Heartbeater{heartbeater_farm = __farm} ->
                                    ((/) <$!> ((I__.readRef sum)) <*>
                                       (I__.fromIntegral <$!> (pure length <*> (pure __farm)))))
                             ifthenelseM
                               ((>) <$!> ((I__.readRef r)) <*> ((/) <$!> pure 80 <*> pure 100))
                               (do 
               			       println (pure "needs bigger")
                                       I__.liftIO (atomicModifyIORef' I__.demo_vms (\ x -> (x+1,()))) -- increase demo vms output
                                       dc :: I__.IORef IDC <- I__.newRef (pure (I__.up null))
				       s :: I__.IORef Server <- I__.newRef (pure (I__.up null))
                                       I__.writeRef dc (nth <$!> pure __allocdcs <*> (pure length <*> (I__.readThis this >>=
                                                                                                              \ Heartbeater{heartbeater_farm = __farm} -> (pure __farm))))
				       --I__.writeRef dc (IDC <$!> (I__.join (I__.new_local <$!> (pure __nebulaDC <*> pure 1 <*> pure 8192) <*> pure this)))
				       (I__.readThis this >>= \ Heartbeater{heartbeater_b = __b} -> I__.writeRef s (Server <$!> I__.join((\ (IDC __dc) -> (I__.join (spawns <$!> (pure __s <*> pure __b) <*> pure __dc))) <$!> ((I__.up <$!> I__.readRef dc)))))
				       (I__.readThis this >>= \ Heartbeater{heartbeater_b = __b} -> ((\ __wrap@(Balancer __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
				                                  -> I__.join (this ^!! __obj <$!> (pure addToFarm <*> (I__.readRef s)))) (__b)))
                                       (I__.join (I__.set 0 (\ v__ c__ -> c__{heartbeater_farm = v__}) <$!> (I__.readThis this >>= \ Heartbeater{heartbeater_farm = __farm} -> ((:) <$!> ((I__.up <$!> I__.readRef s)) <*> (pure __farm))) <*> pure this))

			       ) (println (pure "does not need bigger"))
                             return ())
                       return ()
 
instance B__.Binary Heartbeater
 
 
instance Server_ S where
        serve c r this
          = do res :: I__.IORef Res <- I__.newRef ((pure fib <*> pure 32))
               (I__.readThis this >>=
                  \ S{s_b = __b} ->
                    (I__.join
                       ((\ __wrap@(Balancer
                                     __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                           ->
                           if I__.processNodeId __pid == I__.myNodeId then
                             I__.join
                               (this ^!! __obj <$!>
                                  (pure forwardRes <*> (pure (c)) <*> ((I__.readRef res))))
                             else
                             do __args <- (,,) <$!>
                                            (pure (c)) <*>
                                              ((I__.readRef res)) <*> pure __wrap
                                (^@@) this __obj ($( I__.mkClosure 'forwardRes__remote ) __args))
                          <$!> (pure (I__.up __b)))))
               return ()
        server_load this = do
	      return (toRational (0)) -- stub not needed


 
 
instance Client_ C where
        respond r this = do 
          println ((pure toString <*> (pure r)))
 
__rtable
  = __remoteTable I__..
      I__.registerStatic "B__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict B))
      I__..
      I__.registerStatic "Obj B__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj B)))
      I__..
      I__.registerStatic "C__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict C))
      I__..
      I__.registerStatic "Obj C__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj C)))
      I__..
      I__.registerStatic "Heartbeater__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Heartbeater))
      I__..
      I__.registerStatic "Obj Heartbeater__staticDict"
        (I__.toDynamic
           (I__.SerializableDict ::
              I__.SerializableDict (I__.Obj Heartbeater)))
      I__..
      I__.registerStatic "S__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict S))
      I__..
      I__.registerStatic "Obj S__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj S)))
mainABS this
  = do I__.writeRef I__.demo_name (pure "Demo: Load-balancer Adaptive")
       f :: I__.IORef Int <- I__.newRef (pure 1)
       I__.writeRef I__.demo_vms (pure 1) -- update vms counter
       b :: I__.IORef Balancer <- I__.newRef
                                    (Balancer <$!>
                                       (I__.join
                                          (I__.new <$!>
                                             (pure __b <*> (I__.fromIntegral <$!> I__.readRef f)))))
       cs :: I__.IORef Int <- I__.newRef (pure 10000)
       c :: I__.IORef Client <- I__.newRef (pure (I__.up null))
       while ((>) <$!> (I__.fromIntegral <$!> I__.readRef cs) <*> pure 0)
         (do I__.writeRef c
               (Client <$!>
                  (I__.join (I__.new_local <$!> (pure __c) <*> pure this)))
             (I__.join
                ((\ __wrap@(Balancer
                              __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                    ->
                    if I__.processNodeId __pid == I__.myNodeId then
                      I__.join
                        (this ^!! __obj <$!>
                           (pure forwardReq <*> ((I__.up <$!> I__.readRef c)) <*> pure 4))
                      else
                      do __args <- (,,) <$!>
                                     ((I__.readRef c)) <*> pure (4 :: Int) <*> pure __wrap
                         (^@@) this __obj ($( I__.mkClosure 'forwardReq__remote ) __args))
                   <$!> ((I__.up <$!> I__.readRef b))))
             I__.writeRef cs
               ((-) <$!> (I__.fromIntegral <$!> I__.readRef cs) <*> pure 1)
             I__.liftIO (threadDelay 300000)
             suspend
             return ())
       return ()
main = I__.main_is mainABS (__rtable I__.initRemoteTable)

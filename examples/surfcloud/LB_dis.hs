{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, PatternSignatures, FlexibleContexts,
  DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module LB_dis (main) where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import qualified Data.Binary as B__
import NebulaDC hiding (main)
 
type Req = Int
 
type Res = Int
 
class (I__.Root_ a) => Balancer_ a where
         
        forwardReq :: Client -> Req -> I__.Obj a -> I__.ABS Unit
         
        forwardRes :: Client -> Res -> I__.Obj a -> I__.ABS Unit
         
        serverLoaded :: I__.Obj a -> I__.ABS Unit
 
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
        serverLoaded
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
serverLoaded__remote ((Balancer this)) = serverLoaded this
 
class (I__.Root_ a) => Server_ a where
         
        serve :: Client -> Req -> I__.Obj a -> I__.ABS Unit
 
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
                 (do I__.writeRef dc
                       (IDC <$!>
                          (I__.join
                             (I__.new <$!> (pure __nebulaDC <*> pure 1 <*> pure 8192))))
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
               return ()
 
instance B__.Binary B
 
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
                                            (pure (I__.up c)) <*> (pure req) <*> pure __wrap
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
        serverLoaded this = do return ()
 
data S = S{s_b :: Balancer}
       deriving (I__.Typeable, I__.Generic)
__s b = S{s_b = b}
 
instance I__.Root_ S
 
instance B__.Binary S
 
instance Server_ S where
        serve c r this
          = do lastMinLoad :: I__.IORef Rat <- I__.newRef
                                                 ((/) <$!> pure 2 <*> pure 3)
               ifthenM
                 ((>) <$!> ((I__.readRef lastMinLoad)) <*>
                    ((/) <$!> pure 75 <*> pure 100))
                 (do do (I__.readThis this >>=
                           \ S{s_b = __b} ->
                             (I__.join
                                ((\ __wrap@(Balancer
                                              __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                                    ->
                                    if I__.processNodeId __pid == I__.myNodeId then
                                      I__.join (this ^!! __obj <$!> (pure serverLoaded)) else
                                      do __args <- pure __wrap
                                         (^@@) this __obj
                                           ($( I__.mkClosure 'serverLoaded__remote ) __args))
                                   <$!> (pure (I__.up __b)))))
                        return ())
               res :: I__.IORef Res <- I__.newRef ((pure fib <*> pure 38))
               (I__.readThis this >>=
                  \ S{s_b = __b} ->
                    (I__.join
                       ((\ __wrap@(Balancer
                                     __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                           ->
                           if I__.processNodeId __pid == I__.myNodeId then
                             I__.join
                               (this ^!! __obj <$!>
                                  (pure forwardRes <*> (pure (I__.up c)) <*> ((I__.readRef res))))
                             else
                             do __args <- (,,) <$!>
                                            (pure (I__.up c)) <*>
                                              ((I__.readRef res)) <*> pure __wrap
                                (^@@) this __obj ($( I__.mkClosure 'forwardRes__remote ) __args))
                          <$!> (pure (I__.up __b)))))
               return ()
          where cleanup this = do return ()
 
data C = C{}
       deriving (I__.Typeable, I__.Generic)
__c = C{}
 
instance I__.Root_ C
 
instance B__.Binary C
 
instance Client_ C where
        respond r this = do println ((pure toString <*> (pure r)))
 
I__.remotable
  ['forwardReq__remote, 'forwardRes__remote, 'serverLoaded__remote,
   'respond__remote, 'serve__remote]
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
      I__.registerStatic "S__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict S))
      I__..
      I__.registerStatic "Obj S__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj S)))
mainABS this
  = do f :: I__.IORef Int <- I__.newRef (pure 4)
       b :: I__.IORef Balancer <- I__.newRef
                                    (Balancer <$!>
                                       (I__.join
                                          (I__.new <$!>
                                             (pure __b <*> (I__.fromIntegral <$!> I__.readRef f)))))
       cs :: I__.IORef Int <- I__.newRef (pure 1000)
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
                                     ((I__.up <$!> I__.readRef c)) <*> pure 4 <*> pure __wrap
                         (^@@) this __obj ($( I__.mkClosure 'forwardReq__remote ) __args))
                   <$!> ((I__.up <$!> I__.readRef b))))
             I__.writeRef cs
               ((-) <$!> (I__.fromIntegral <$!> I__.readRef cs) <*> pure 1)
             return ())
       return ()
main = I__.main_is mainABS (__rtable I__.initRemoteTable)
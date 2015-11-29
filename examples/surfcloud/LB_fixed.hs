{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, PatternSignatures, FlexibleContexts,
  DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module LB_fixed (main) where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import qualified Data.Binary as B__
import NebulaDC hiding (main)
import Control.Concurrent (threadDelay)
 
type Req = Int
 
type Res = Int
fib (n :: Int)
  = case I__.fromIntegral n of
        0 -> 0
        1 -> 1
        n_ -> (fib (I__.fromIntegral n - 1) + fib (I__.fromIntegral n - 2))
      :: Int
 
class (I__.Root_ a) => IBalancer_ a where
         
        forwardReq :: IClient -> Req -> I__.Obj a -> I__.ABS Unit
         
        forwardRes :: IClient -> Res -> I__.Obj a -> I__.ABS Unit
 
data IBalancer = forall a . (IBalancer_ a) => IBalancer (I__.Obj a)
 
instance I__.Show IBalancer where
        show _ = "IBalancer"
 
instance I__.Sub IBalancer IBalancer where
        up x = x
 
instance (IBalancer_ a) => I__.Sub (I__.Obj a) IBalancer where
        up = IBalancer
 
instance I__.Sub IBalancer I__.Root where
        up (IBalancer a) = I__.Root a
 
instance IBalancer_ I__.Null where
        forwardReq
          = I__.error
              "this should not happen. report the program to the compiler developers"
        forwardRes
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq IBalancer where
        IBalancer (I__.ObjectRef _ id1 tid1) ==
          IBalancer (I__.ObjectRef _ id2 tid2)
          = (id1 == id2) && (tid1 == tid2)
        IBalancer I__.NullRef == IBalancer I__.NullRef = True
        _ == _ = False
 
instance I__.Ord IBalancer where
        compare (IBalancer (I__.ObjectRef _ id1 tid1))
          (IBalancer (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (IBalancer I__.NullRef) (IBalancer I__.NullRef) = I__.EQ
        compare (IBalancer I__.NullRef) (IBalancer _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary IBalancer where
        put (IBalancer x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_IBalancer of
                   Just (SomeGet_IBalancer someget) -> IBalancer <$!> someget
                   Nothing -> I__.error "Binary IBalancer: fingerprint unknown"
stable_IBalancer
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj Balancer))]
  where  
        mkSMapEntry ::
                    forall a . (IBalancer_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_IBalancer)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_IBalancer (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_IBalancer = forall a .
                           (IBalancer_ a, I__.Serializable a) =>
                           SomeGet_IBalancer (B__.Get (I__.Obj a))
forwardReq__remote (c, r, (IBalancer this)) = forwardReq c r this
forwardRes__remote (c, r, (IBalancer this)) = forwardRes c r this
 
class (I__.Root_ a) => IServer_ a where
         
        processRequest :: IClient -> Req -> I__.Obj a -> I__.ABS Unit
 
data IServer = forall a . (IServer_ a) => IServer (I__.Obj a)
 
instance I__.Show IServer where
        show _ = "IServer"
 
instance I__.Sub IServer IServer where
        up x = x
 
instance (IServer_ a) => I__.Sub (I__.Obj a) IServer where
        up = IServer
 
instance I__.Sub IServer I__.Root where
        up (IServer a) = I__.Root a
 
instance IServer_ I__.Null where
        processRequest
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq IServer where
        IServer (I__.ObjectRef _ id1 tid1) ==
          IServer (I__.ObjectRef _ id2 tid2) = (id1 == id2) && (tid1 == tid2)
        IServer I__.NullRef == IServer I__.NullRef = True
        _ == _ = False
 
instance I__.Ord IServer where
        compare (IServer (I__.ObjectRef _ id1 tid1))
          (IServer (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (IServer I__.NullRef) (IServer I__.NullRef) = I__.EQ
        compare (IServer I__.NullRef) (IServer _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary IServer where
        put (IServer x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_IServer of
                   Just (SomeGet_IServer someget) -> IServer <$!> someget
                   Nothing -> I__.error "Binary IServer: fingerprint unknown"

data Server = Server{server_b :: IBalancer}
            deriving (I__.Typeable, I__.Generic)
__server b = Server{server_b = b}

stable_IServer
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj Server))]
  where  
        mkSMapEntry ::
                    forall a . (IServer_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_IServer)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_IServer (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_IServer = forall a .
                         (IServer_ a, I__.Serializable a) =>
                         SomeGet_IServer (B__.Get (I__.Obj a))
processRequest__remote (c, r, (IServer this))
  = processRequest c r this
 
class (I__.Root_ a) => IClient_ a where
         
        processResponse :: Res -> I__.Obj a -> I__.ABS Unit
 
data IClient = forall a . (IClient_ a) => IClient (I__.Obj a)
 
instance I__.Show IClient where
        show _ = "IClient"
 
instance I__.Sub IClient IClient where
        up x = x
 
instance (IClient_ a) => I__.Sub (I__.Obj a) IClient where
        up = IClient
 
instance I__.Sub IClient I__.Root where
        up (IClient a) = I__.Root a
 
instance IClient_ I__.Null where
        processResponse
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq IClient where
        IClient (I__.ObjectRef _ id1 tid1) ==
          IClient (I__.ObjectRef _ id2 tid2) = (id1 == id2) && (tid1 == tid2)
        IClient I__.NullRef == IClient I__.NullRef = True
        _ == _ = False
 
instance I__.Ord IClient where
        compare (IClient (I__.ObjectRef _ id1 tid1))
          (IClient (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (IClient I__.NullRef) (IClient I__.NullRef) = I__.EQ
        compare (IClient I__.NullRef) (IClient _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary IClient where
        put (IClient x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_IClient of
                   Just (SomeGet_IClient someget) -> IClient <$!> someget
                   Nothing -> I__.error "Binary IClient: fingerprint unknown"

data Client = Client{}
            deriving (I__.Typeable, I__.Generic)
__client = Client{}

stable_IClient
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj Client))]
  where  
        mkSMapEntry ::
                    forall a . (IClient_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_IClient)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_IClient (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_IClient = forall a .
                         (IClient_ a, I__.Serializable a) =>
                         SomeGet_IClient (B__.Get (I__.Obj a))
processResponse__remote (r, (IClient this))
  = processResponse r this
 
data Balancer = Balancer{balancer_farm :: List IServer,
                         balancer_initFarmSize :: !Int, balancer_rr :: !Int}
              deriving (I__.Typeable, I__.Generic)
__balancer initFarmSize
  = (\ farm ->
       (\ rr ->
          Balancer{balancer_farm = farm,
                   balancer_initFarmSize = initFarmSize, balancer_rr = rr})
         0)
      []
 
instance I__.Root_ Balancer where
        __init this
          = do i :: I__.IORef Int <- I__.newRef
                                       (I__.readThis this >>=
                                          \ Balancer{balancer_initFarmSize = __initFarmSize} ->
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
               s :: I__.IORef IServer <- I__.newRef (pure (I__.up null))
               while (not <$!> ((==) <$!> ((I__.readRef dcs)) <*> pure []))
                 (do I__.writeRef dc ((pure head <*> ((I__.readRef dcs))))
                     I__.writeRef s
                       (IServer <$!>
                          I__.join
                            ((\ (IDC __dc) ->
                                (I__.join (spawns <$!> (pure __server <*> pure (I__.up this))))
                                  __dc)
                               <$!> ((I__.up <$!> I__.readRef dc))))
                     (I__.join
                        (I__.set 0 (\ v__ c__ -> c__{balancer_farm = v__}) <$!>
                           (I__.readThis this >>=
                              \ Balancer{balancer_farm = __farm} ->
                                ((:) <$!> ((I__.up <$!> I__.readRef s)) <*> (pure __farm)))
                             <*> pure this))
                     I__.writeRef i
                       ((+) <$!> (I__.fromIntegral <$!> I__.readRef i) <*> pure 1)
                     I__.writeRef dcs ((pure tail <*> ((I__.readRef dcs))))
                     return ())
               return ()
 
instance B__.Binary Balancer
 
I__.remotable
  ['forwardReq__remote, 'forwardRes__remote,
   'processResponse__remote, 'processRequest__remote]

instance IBalancer_ Balancer where
        forwardReq c req this
          = do (I__.readThis this >>=
                  \ Balancer{balancer_farm = __farm, balancer_rr = __rr} ->
                    (I__.join
                       ((\ __wrap@(IServer __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                           ->
                           if I__.processNodeId __pid == I__.myNodeId then
                             I__.join
                               (this ^!! __obj <$!>
                                  (pure processRequest <*> (pure (I__.up c)) <*> (pure req)))
                             else
                             do __args <- (,,) <$!>
                                            (pure (c)) <*> (pure req) <*> pure __wrap
                                (^@@) this __obj
                                  ($( I__.mkClosure 'processRequest__remote ) __args))
                          <$!>
                          (((pure nth <*> (pure __farm)) <*>
                              (pure (I__.fromIntegral __rr)))))))
               (I__.join
                  (I__.set 2 (\ v__ c__ -> c__{balancer_rr = v__}) <$!>
                     (I__.readThis this >>=
                        \ Balancer{balancer_rr = __rr, balancer_farm = __farm} ->
                          (ifthenelseM
                             ((>=) <$!> (pure (I__.fromIntegral __rr)) <*>
                                ((-) <$!> ((pure length <*> (pure __farm))) <*> pure 1))
                             (pure 0)
                             ((+) <$!> (pure (I__.fromIntegral __rr)) <*> pure 1)))
                       <*> pure this))
               return ()
        forwardRes c rsp this
          = do (I__.join
                  ((\ __wrap@(IClient __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                      ->
                      if I__.processNodeId __pid == I__.myNodeId then
                        I__.join
                          (this ^!! __obj <$!> (pure processResponse <*> (pure rsp)))
                        else
                        do __args <- (,) <$!> (pure rsp) <*> pure __wrap
                           (^@@) this __obj
                             ($( I__.mkClosure 'processResponse__remote ) __args))
                     <$!> (pure (I__.up c))))
               return ()
 
 
instance I__.Root_ Server
 
instance B__.Binary Server
 
instance IServer_ Server where
        processRequest c r this
          = do res :: I__.IORef Res <- I__.newRef ((pure fib <*> (pure r)))
               (I__.readThis this >>=
                  \ Server{server_b = __b} ->
                    (I__.join
                       ((\ __wrap@(IBalancer
                                     __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                           ->
                           if I__.processNodeId __pid == I__.myNodeId then
                             I__.join
                               (this ^!! __obj <$!>
                                  (pure forwardRes <*> (pure (I__.up c)) <*> ((I__.readRef res))))
                             else
                             do __args <- (,,) <$!>
                                            (pure (c)) <*>
                                              ((I__.readRef res)) <*> pure __wrap
                                (^@@) this __obj ($( I__.mkClosure 'forwardRes__remote ) __args))
                          <$!> (pure (I__.up __b)))))
               return ()
 
 
instance I__.Root_ Client
 
instance B__.Binary Client
 
instance IClient_ Client where
        processResponse r this
          = do println (pure "Job done:")
               println ((pure toString <*> (pure r)))
 
__rtable
  = __remoteTable I__..
      I__.registerStatic "Balancer__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Balancer))
      I__..
      I__.registerStatic "Obj Balancer__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Balancer)))
      I__..
      I__.registerStatic "Client__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Client))
      I__..
      I__.registerStatic "Obj Client__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Client)))
      I__..
      I__.registerStatic "Server__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Server))
      I__..
      I__.registerStatic "Obj Server__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Server)))
mainABS this
  = do fixedFarmSize :: I__.IORef Int <- I__.newRef (pure 4)
       b :: I__.IORef IBalancer <- I__.newRef
                                     (IBalancer <$!>
                                        (I__.join
                                           (I__.new <$!>
                                              (pure __balancer <*>
                                                 (I__.fromIntegral <$!>
                                                    I__.readRef fixedFarmSize)))))
       numberOfClients :: I__.IORef Int <- I__.newRef (pure 1000)
       c :: I__.IORef IClient <- I__.newRef (pure (I__.up null))
       fixedJobSize :: I__.IORef Int <- I__.newRef (pure 38)
       while
         ((>) <$!> (I__.fromIntegral <$!> I__.readRef numberOfClients) <*>
            pure 0)
         (do I__.writeRef c
               (IClient <$!>
                  (I__.join (I__.new_local <$!> (pure __client) <*> pure this)))
             (I__.join
                ((\ __wrap@(IBalancer
                              __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
                    ->
                    if I__.processNodeId __pid == I__.myNodeId then
                      I__.join
                        (this ^!! __obj <$!>
                           (pure forwardReq <*> ((I__.up <$!> I__.readRef c)) <*>
                              (I__.fromIntegral <$!> I__.readRef fixedJobSize)))
                      else
                      do __args <- (,,) <$!>
                                     ((I__.readRef c)) <*>
                                       (I__.readRef fixedJobSize) <*>
                                         pure __wrap
                         (^@@) this __obj ($( I__.mkClosure 'forwardReq__remote ) __args))
                   <$!> ((I__.up <$!> I__.readRef b))))
             I__.writeRef numberOfClients
               ((-) <$!> (I__.fromIntegral <$!> I__.readRef numberOfClients) <*>
                  pure 1)
             return ())
       I__.liftIO (I__.join ((pure threadDelay <*> pure 30000000)))
       return ()
main = I__.main_is mainABS (__rtable I__.initRemoteTable)

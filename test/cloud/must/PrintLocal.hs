{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, PatternSignatures, DeriveDataTypeable,
  DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC
  -w -Werror -fforce-recomp -fwarn-missing-methods -fno-ignore-asserts
  #-}
module PrintLocal (main) where
import qualified Lang.ABS.Runtime.Base as I__
import qualified Lang.ABS.Runtime.Core as I__
import qualified Lang.ABS.Compiler.Include as I__
import Lang.ABS.Runtime.Prim
import Lang.ABS.StdLib
import qualified Data.Binary as B__
import LocalDC hiding (main)
import Control.Distributed.Process
import qualified Control.Distributed.Static as Static
import Control.Distributed.Process.Closure
import Data.Rank1Dynamic
import Data.Rank1Typeable
import Prelude ((.))
import Network.Transport.TCP
import Prelude (putStr)
import Control.Concurrent (threadDelay)
import qualified Control.Monad.Trans.State.Strict as S (runStateT, modify, get, put)
 
class (I__.Root_ a) => IPrint_ a where
         
        print :: String -> I__.Obj a -> I__.ABS Unit
 
data IPrint = forall a . (IPrint_ a) => IPrint (I__.Obj a)
 
instance I__.Show IPrint where
        show _ = "IPrint"
 
instance I__.Sub IPrint IPrint where
        up x = x
 
instance (IPrint_ a) => I__.Sub (I__.Obj a) IPrint where
        up = IPrint
 
instance I__.Sub IPrint I__.Root where
        up (IPrint a) = I__.Root a
 
instance IPrint_ I__.Null where
        print
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq IPrint where
        IPrint (I__.ObjectRef _ id1 tid1) ==
          IPrint (I__.ObjectRef _ id2 tid2) = (id1 == id2) && (tid1 == tid2)
        IPrint I__.NullRef == IPrint I__.NullRef = True
        _ == _ = False
 
instance B__.Binary IPrint where
        put (IPrint x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_IPrint of
                   Just (SomeGet_IPrint someget) -> IPrint <$!> someget
                   Nothing -> I__.error "Binary IPrint: fingerprint unknown"
stable_IPrint
  = I__.fromList [(mkSMapEntry (I__.undefined :: I__.Obj Print))]
  where  
        mkSMapEntry ::
                    forall a . (IPrint_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_IPrint)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_IPrint (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_IPrint = forall a . (IPrint_ a, I__.Serializable a) =>
                        SomeGet_IPrint (B__.Get (I__.Obj a))
 
data Print = Print{}
           deriving (I__.Typeable, I__.Generic)
__print = Print{}
 
instance I__.Root_ Print where
        __init this = do println (pure "print is up")
 
instance B__.Binary Print
 
instance IPrint_ Print where
        print b this
          = do println
                 (((pure concatenate <*> pure "remote send") <*> (pure b)))

-- fst_ :: (Int, Int) -> Process Int
-- fst_ (x,y) = return x

-- hello :: () -> Process ()
-- hello () = I__.liftIO (putStr "hello")
-- remotable ['fst_]
-- r <- I__.lift (I__.lift (call $(functionTDict 'fst_) (NodeId epa) ($(mkClosure 'fst_) (2 :: Int, 3 :: Int))))
-- println (toString <$!> (pure r))
-- --I__.lift (I__.lift (spawn (NodeId epa) ($(mkClosure 'hello) ())))
       

print_ :: (String,I__.Obj Print) -> I__.ABS ()
print_ (s,t) = print s t

remotable ['print_]

mainABS this@(I__.ObjectRef _ thisCOG _)
  = do println (pure "main started")
       dc :: I__.IORef IDC <- I__.newRef
                                (IDC <$!>
                                   (I__.join
                                      (I__.new <$!> (pure __localDC <*> pure 9001) <*> pure this)))
       I__.liftIO (threadDelay 100000)

       __mvar <- liftIO I__.newEmptyMVar
       __astate@(I__.AState{I__.aCounter = __counter}) <- I__.lift S.get
       I__.lift (S.put (__astate{I__.aCounter = __counter + 1}))
       let __f = I__.FutureRef __mvar thisCOG __counter

       o :: I__.IORef IPrint <- I__.newRef (IPrint <$!> I__.join ((\ (IDC x) -> spawns staticPrint __print x) <$!> (I__.readRef dc)))
       
       vo :: IPrint <- I__.readRef o
       ( \ (IPrint __obj@(I__.ObjectRef _ (I__.COG (_,pid)) _)) -> do
                     println(pure(toString(pid)))
                     --let x = B__.encode (I__.RemotJob __obj __f  ($(mkClosure 'print_) ("hello", __obj)))
                     --println(pure(toString(x)))
                     I__.lift (I__.lift (send pid (I__.RemotJob __obj __f  ($(mkClosure 'print_) ("hello", __obj))) ))) vo
                     -- I__.lift (I__.lift (send pid (I__.WakeupSignal (3 :: Int) thisCOG (4 :: Int))))) vo
       await (I__.FutureLocalGuard __f) this
       res <- get __f
       println(pure(toString(res)))
       println (pure "main finished")
       
       return ()
main = I__.main_is mainABS (__remoteTable (rtable (I__.initRemoteTable)))


rootPrint :: Static.Static (I__.RootDict Print)
rootPrint = Static.staticLabel "Print__rootDict"

staticPrint :: Static.Static (I__.SerializableDict (I__.Obj Print))
staticPrint = Static.staticLabel "Obj Print__staticDict"

rtable :: Static.RemoteTable -> Static.RemoteTable
rtable = Static.registerStatic "Print__rootDict" (toDynamic (I__.RootDict :: I__.RootDict Print))
       . Static.registerStatic "Obj Print__staticDict" (toDynamic (I__.SerializableDict :: I__.SerializableDict (I__.Obj Print)))


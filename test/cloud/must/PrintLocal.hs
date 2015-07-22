{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification,
  MultiParamTypeClasses, PatternSignatures, FlexibleContexts,
  DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
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
import qualified Control.Distributed.Static as Static
import Data.Rank1Dynamic
import Prelude ((.))
import Control.Concurrent (threadDelay)
 
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
 
instance I__.Ord IPrint where
        compare (IPrint (I__.ObjectRef _ id1 tid1))
          (IPrint (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (IPrint I__.NullRef) (IPrint I__.NullRef) = I__.EQ
        compare (IPrint I__.NullRef) (IPrint _) = I__.LT
        compare _ _ = I__.GT
 
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
print__remote (b, (IPrint this)) = print b this
 
data Print = Print{}
           deriving (I__.Typeable, I__.Generic)
__print = Print{}
 
instance I__.Root_ Print where
        __init this = do println (pure "print is up")
 
instance B__.Binary Print
 
instance IPrint_ Print where
        print b this
          = do println
                 (((pure concatenate <*> pure "remote has sent") <*> (pure b)))
 
I__.remotable ['print__remote]
mainABS this
  = do println (pure "mplo")
       dc :: I__.IORef IDC <- I__.newRef
                                (IDC <$!> (I__.join (I__.new <$!> (pure __localDC <*> pure 9001))))
       I__.liftIO (threadDelay 100000)
       o :: I__.IORef IPrint <- I__.newRef
                                  (IPrint <$!>
                                     I__.join
                                       ((\ (IDC __dc) ->
                                           (I__.join (spawns <$!> (pure __print))) __dc)
                                          <$!> ((I__.up <$!> I__.readRef dc))))
       (I__.join
          ((\ __wrap@(IPrint __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
              ->
              if I__.processNodeId __pid == I__.myNodeId then
                I__.join (this ^! __obj <$!> (pure print <*> pure "hello")) else
                do __args <- (,) <$!> pure "hello" <*> pure __wrap
                   (^@) this __obj ($( I__.mkClosure 'print__remote ) __args))
             <$!> ((I__.up <$!> I__.readRef o))))
       return ()
main = I__.main_is mainABS (__remoteTable (rtable I__.initRemoteTable))


rtable :: Static.RemoteTable -> Static.RemoteTable
rtable = Static.registerStatic "Print__rootDict" (toDynamic (I__.RootDict :: I__.RootDict Print))
       . Static.registerStatic "Obj Print__staticDict" (toDynamic (I__.SerializableDict :: I__.SerializableDict (I__.Obj Print)))

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
  = I__.fromList
      [(mkSMapEntry (I__.undefined :: I__.Obj Print2)),
       (mkSMapEntry (I__.undefined :: I__.Obj Print1))]
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
 
data Print1 = Print1{}
            deriving (I__.Typeable, I__.Generic)
__print1 = Print1{}
 
instance I__.Root_ Print1 where
        __init this = do println (pure "print1 is up")
 
instance B__.Binary Print1
 
instance IPrint_ Print1 where
        print b this
          = do println
                 (((pure concatenate <*> pure "print1 says") <*> (pure b)))
 
data Print2 = Print2{}
            deriving (I__.Typeable, I__.Generic)
__print2 = Print2{}
 
instance I__.Root_ Print2 where
        __init this = do println (pure "print2 is also up")
          where localMethod b this
                  = do println (pure b)
                       println (pure "in local method")
 
instance B__.Binary Print2
 
instance IPrint_ Print2 where
        print b this
          = do println
                 (((pure concatenate <*>
                      (((pure concatenate <*> pure "print2 says") <*> (pure b))))
                     <*> pure "!!"))
               (I__.join (this ^. this <$!> (pure localMethod <*> (pure b))))
               return ()
          where localMethod b this
                  = do println (pure b)
                       println (pure "in local method")
 
I__.remotable ['print__remote]
__rtable
  = __remoteTable I__..
      I__.registerStatic "Print1__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Print1))
      I__..
      I__.registerStatic "Obj Print1__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Print1)))
      I__..
      I__.registerStatic "Print2__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Print2))
      I__..
      I__.registerStatic "Obj Print2__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Print2)))
mainABS this
  = do println (pure "mplo")
       dc :: I__.IORef IDC <- I__.newRef
                                (IDC <$!>
                                   (I__.join
                                      (I__.new_local <$!>
                                         (pure __localDC <*> pure 9001) <*> pure this)))
       o :: I__.IORef IPrint <- I__.newRef
                                  (IPrint <$!>
                                     I__.join
                                       ((\ (IDC __dc) ->
                                           (I__.join (spawns <$!> (pure __print1))) __dc)
                                          <$!> ((I__.up <$!> I__.readRef dc))))
       (I__.join
          ((\ __wrap@(IPrint __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
              ->
              if I__.processNodeId __pid == I__.myNodeId then
                I__.join (this ^!! __obj <$!> (pure print <*> pure "hello")) else
                do __args <- (,) <$!> pure "hello" <*> pure __wrap
                   (^@@) this __obj ($( I__.mkClosure 'print__remote ) __args))
             <$!> ((I__.up <$!> I__.readRef o))))
       I__.writeRef o
         (IPrint <$!>
            I__.join
              ((\ (IDC __dc) -> (I__.join (spawns <$!> (pure __print2))) __dc)
                 <$!> ((I__.up <$!> I__.readRef dc))))
       f :: I__.IORef (Fut Unit) <- I__.newRef
                                      (I__.join
                                         ((\ __wrap@(IPrint
                                                       __obj@(I__.ObjectRef _ (I__.COG (_, __pid))
                                                                _))
                                             ->
                                             if I__.processNodeId __pid == I__.myNodeId then
                                               I__.join
                                                 (this ^! __obj <$!>
                                                    (pure print <*> pure "hi there"))
                                               else
                                               do __args <- (,) <$!> pure "hi there" <*> pure __wrap
                                                  (^@) this __obj
                                                    ($( I__.mkClosure 'print__remote ) __args))
                                            <$!> ((I__.up <$!> I__.readRef o))))
       I__.join
         (await <$!>
            (I__.FutureLocalGuard <$!> ((I__.readRef f))) <*> pure this)
main = I__.main_is mainABS (__rtable I__.initRemoteTable)
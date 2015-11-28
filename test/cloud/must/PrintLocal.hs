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
 
class (I__.Root_ a) => IPrinter_ a where
         
        print :: String -> I__.Obj a -> I__.ABS Unit
 
data IPrinter = forall a . (IPrinter_ a) => IPrinter (I__.Obj a)
 
instance I__.Show IPrinter where
        show _ = "IPrinter"
 
instance I__.Sub IPrinter IPrinter where
        up x = x
 
instance (IPrinter_ a) => I__.Sub (I__.Obj a) IPrinter where
        up = IPrinter
 
instance I__.Sub IPrinter I__.Root where
        up (IPrinter a) = I__.Root a
 
instance IPrinter_ I__.Null where
        print
          = I__.error
              "this should not happen. report the program to the compiler developers"
 
instance I__.Eq IPrinter where
        IPrinter (I__.ObjectRef _ id1 tid1) ==
          IPrinter (I__.ObjectRef _ id2 tid2)
          = (id1 == id2) && (tid1 == tid2)
        IPrinter I__.NullRef == IPrinter I__.NullRef = True
        _ == _ = False
 
instance I__.Ord IPrinter where
        compare (IPrinter (I__.ObjectRef _ id1 tid1))
          (IPrinter (I__.ObjectRef _ id2 tid2))
          = I__.compare (tid1, id1) (tid2, id2)
        compare (IPrinter I__.NullRef) (IPrinter I__.NullRef) = I__.EQ
        compare (IPrinter I__.NullRef) (IPrinter _) = I__.LT
        compare _ _ = I__.GT
 
instance B__.Binary IPrinter where
        put (IPrinter x)
          = do B__.put (I__.encodeFingerprint (I__.fingerprint x))
               B__.put x
        get
          = do fp <- B__.get
               case I__.lookup (I__.decodeFingerprint fp) stable_IPrinter of
                   Just (SomeGet_IPrinter someget) -> IPrinter <$!> someget
                   Nothing -> I__.error "Binary IPrinter: fingerprint unknown"
stable_IPrinter
  = I__.fromList
      [(mkSMapEntry (I__.undefined :: I__.Obj Printer2)),
       (mkSMapEntry (I__.undefined :: I__.Obj Printer1))]
  where  
        mkSMapEntry ::
                    forall a . (IPrinter_ a, I__.Serializable a) =>
                      I__.Obj a -> (I__.Fingerprint, SomeGet_IPrinter)
        mkSMapEntry a
          = (I__.fingerprint a,
             SomeGet_IPrinter (B__.get :: B__.Get (I__.Obj a)))
 
data SomeGet_IPrinter = forall a .
                          (IPrinter_ a, I__.Serializable a) =>
                          SomeGet_IPrinter (B__.Get (I__.Obj a))
print__remote (b, (IPrinter this)) = print b this
 
data Printer1 = Printer1{}
              deriving (I__.Typeable, I__.Generic)
__printer1 = Printer1{}
 
instance I__.Root_ Printer1 where
        __init this = do println (pure "print1 is up")
 
instance B__.Binary Printer1
 
instance IPrinter_ Printer1 where
        print b this
          = do println
                 (((pure concatenate <*> pure "print1 says") <*> (pure b)))
 
data Printer2 = Printer2{}
              deriving (I__.Typeable, I__.Generic)
__printer2 = Printer2{}
 
instance I__.Root_ Printer2 where
        __init this = do println (pure "print2 is also up")
          where localMethod b this
                  = do println (pure b)
                       println (pure "in local method")
 
instance B__.Binary Printer2
 
instance IPrinter_ Printer2 where
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
      I__.registerStatic "Printer1__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Printer1))
      I__..
      I__.registerStatic "Obj Printer1__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Printer1)))
      I__..
      I__.registerStatic "Printer2__rootDict"
        (I__.toDynamic (I__.RootDict :: I__.RootDict Printer2))
      I__..
      I__.registerStatic "Obj Printer2__staticDict"
        (I__.toDynamic
           (I__.SerializableDict :: I__.SerializableDict (I__.Obj Printer2)))
mainABS this
  = do println (pure "mplo")
       dc :: I__.IORef IDC <- I__.newRef
                                (IDC <$!>
                                   (I__.join
                                      (I__.new_local <$!>
                                         (pure __localDC <*> pure 9001) <*> pure this)))
       o :: I__.IORef IPrinter <- I__.newRef
                                    (IPrinter <$!>
                                       I__.join
                                         ((\ (IDC __dc) ->
                                             (I__.join (spawns <$!> (pure __printer1))) __dc)
                                            <$!> ((I__.up <$!> I__.readRef dc))))
       (I__.join
          ((\ __wrap@(IPrinter
                        __obj@(I__.ObjectRef _ (I__.COG (_, __pid)) _))
              ->
              if I__.processNodeId __pid == I__.myNodeId then
                I__.join (this ^!! __obj <$!> (pure print <*> pure "hello")) else
                do __args <- (,) <$!> pure "hello" <*> pure __wrap
                   (^@@) this __obj ($( I__.mkClosure 'print__remote ) __args))
             <$!> ((I__.up <$!> I__.readRef o))))
       I__.writeRef o
         (IPrinter <$!>
            I__.join
              ((\ (IDC __dc) -> (I__.join (spawns <$!> (pure __printer2))) __dc)
                 <$!> ((I__.up <$!> I__.readRef dc))))
       f :: I__.IORef (Fut Unit) <- I__.newRef
                                      (I__.join
                                         ((\ __wrap@(IPrinter
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
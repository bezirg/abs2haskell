{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}

module Lang.ABS.StdLib.Prelude 
    (
     -- Number operations
     (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (/), (%),
     -- Bool operations
     (Prelude.||), (Prelude.&&), (Prelude.==), Prelude.not,
     -- ABS builtin types
     Int, Rat, Prelude.Bool (..) , Prelude.Eq, Unit, List, Prelude.String,
     -- tuples datatypes and functions
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd,
     -- Maybe, Either datatypes and functions
     Prelude.Maybe (..), Prelude.Either (..), left, right, Prelude.maybe, fromJust,
     -- List and array functions
     list, length, listArray, replace, elemAt, Prelude.repeat, Array,
     -- ABS Map datatypes and functions
     M.Map, M.empty, put, insertAssoc, lookupUnsafe, removeKey,
     -- other
     assert, null
    )
        where

import qualified Prelude as Prelude
import Lang.ABS.Runtime.Base

import Control.Monad.Trans.Class (lift)
import Control.Monad (when, liftM)
import qualified Control.Exception.Base as Exception (evaluate)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Control.Concurrent (newChan, writeChan, writeList2Chan, newEmptyMVar)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Control.Monad.Coroutine (mapMonad)
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (listArray)
import Data.List (length)

class IntOrRational a where
    (/) :: a -> a -> a

instance IntOrRational Int where
    (/) = Prelude.div

instance IntOrRational Rat where
    (/) = (Prelude./)

x % y = Prelude.fromIntegral (x `Prelude.mod` y)

type Unit = ()

type Int = Prelude.Int

type Rat = Prelude.Rational

type List = [] -- data List a = Nil|Cons a (List a)  -- not this, we want to map to actual Haskell lists

assert :: (Object__ o) => ABS o Prelude.Bool -> ABS o ()
assert act = act Prelude.>>= \ pred -> when (Prelude.not pred) (Prelude.error "Assertion failed")

put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

-- turned an unsafe to a safe operation
insertAssoc :: Prelude.Ord k => (k,v) -> M.Map k v -> M.Map k v
insertAssoc (k,v) m = M.insert k v m

lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k

removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

type Pair a b = (a,b)

type Triple a b c = (a,b,c)

left (Prelude.Left a ) = a
right (Prelude.Right a) = a

fstT (a,_,_) = a
sndT (_,b,_) = b
trd (_,_,c) = c

-- arrays

replace a cs = a UArray.// cs
elemAt(a, i) = a UArray.! i

type Array = UArray.UArray

-- a reference to a null object
null :: ObjectRef Null
null = NullRef

-- Dummy for list n-ary constructors
list :: [a] -> [a]
list = Prelude.id




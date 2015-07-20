{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude, CPP #-}

module Lang.ABS.StdLib.Prelude 
    (
     -- * ABS builtin types

     -- | All of them synonyms to standard Haskell datastructures.
     Int, Rat, Prelude.Bool (..) , Unit, List, Lang.ABS.Runtime.Base.Fut, Lang.ABS.Runtime.Base.Promise, Prelude.String, Array,
     -- * Operations on numbers
     (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (/), (%), Prelude.abs, pow, Prelude.truncate,
     -- * Boolean Operations 
     (Prelude.||), (Prelude.&&), (Prelude.==), Prelude.not,
     -- * Built-in Pairs and Triples and their functions
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd,
     -- * Maybe, Either datatypes and their functions
     Prelude.Maybe (..), fromJust, isJust,
     Prelude.Either (..), left, right, isLeft, isRight,
     -- * Functions for "List" datastructures
     list, Prelude.tail, Prelude.head, length, isEmpty, nth, concatenate, appendright, without, Prelude.repeat, Prelude.reverse, copy,
     -- * Functions for boxed "Array" datastructures
     listArray, replace, elemAt,
     -- * The ABS Map datatype and its functions
     M.Map, map, _emptyMap, put, insert, lookupUnsafe, lookupMaybe, lookupDefault, removeKey, keys, values,
     -- * The ABS Set datatype and its functions
     S.Set, set, _emptySet, emptySet, S.size, contains, S.union, S.intersection, S.difference, insertElement, remove, take,
     -- * Printing to Strings and to standard-output
     toString, intToString, substr, strlen, println, readln,
     -- * Lifting ABS pure code to ABS object layer

     -- | Haskell is pure by default. These are necessary functions for lifting pure ABS expressions (of the functional core) to the ABS' object layer (monadic statements).

     -- ** Haskell's return 

     -- | is an expression taking a pure value and lifting it to the monadic world.

     Prelude.return,
     -- ** Sequencing ABS statements
     (Prelude.>>=), (Prelude.=<<),
     -- ** Applicative-style for easier functional application of pure ABS expressions
     pure, (<$!>), (<*>)
    )
        where

import qualified Prelude as Prelude
import Lang.ABS.Runtime.Base

import Control.Applicative (pure, (<*>))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import qualified Data.Array.IArray as BArray
import Data.Array.Unboxed (listArray)
import Data.List (length)
import Control.Monad.IO.Class (liftIO)

#if __GLASGOW_HASKELL__ >= 710
import Control.Monad ((<$!>))
#endif

#if __GLASGOW_HASKELL__ >= 780
import Data.Either (isLeft, isRight)
#else
isLeft :: Prelude.Either a b -> Bool
isLeft (Prelude.Left  _) = Prelude.True
isLeft (Prelude.Right _) = Prelude.False

isRight :: Prelude.Either a b -> Bool
isRight (Prelude.Left  _) = Prelude.False
isRight (Prelude.Right _) = Prelude.True
#endif

#if __GLASGOW_HASKELL__ < 710
-- strict fmap (applicative), taken from base-4.8
-- TODO: specialize
{-# INLINE (<$!>) #-}
(<$!>) :: Prelude.Monad m => (t -> b) -> m t -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` Prelude.return z
#endif



-- | ABS Number are either 'Int'egers or 'Rat'ionals
class (Prelude.Num a) => Number a where
    (/) :: a -> a -> a

instance Number Int where
    (/) = Prelude.div

instance Number Rat where
    (/) = (Prelude./)

-- | Modulo operation. Takes strictly two integers
-- and returns a polymorphic number (either Int or Rational, based on the followup computation).
{-# INLINE (%) #-}
(%) :: Number b => Int -> Int -> b
x % y = Prelude.fromIntegral (x `Prelude.mod` y)

-- | Raising a number to a non-negative integer power
{-# INLINE pow #-}
pow :: Number b => b -> Int -> b
pow x y | y Prelude.< 0     = 0 -- TODO: this should normally return an error
        | Prelude.otherwise = x Prelude.^ y

type Unit = ()

type Int = Prelude.Int

type Bool = Prelude.Bool

type Rat = Prelude.Rational

-------- LISTS--------------
----------------------------

-- | An ABS synonym to underneath Haskell lists. It is the same as defining in ABS:
--
-- > data List a = Nil | Cons a (List a)
type List = [] 

-- | Returns the element of the list positioned at the given index.
{-# INLINE nth #-}
nth :: List a -> Int -> a
nth = (Prelude.!!)

-- | Checks if the list is empty.
{-# INLINE isEmpty #-}
isEmpty :: List a -> Bool
isEmpty = Prelude.null

-- | Replicating an element 'n' times, forming a list of length n.
{-# INLINE copy #-}
copy :: a -> Int -> List a
copy = Prelude.flip Prelude.replicate

-- | Removes all occurences of an element from a list
{-# INLINE without #-}
without :: Prelude.Eq t => [t] -> t -> [t]
without [] _ = []
without (x:xs) a | x Prelude.== a = without xs a 
                 | Prelude.otherwise = x : without xs a

{-# INLINE concatenate #-}
concatenate :: [a] -> [a] -> [a]
concatenate = (Prelude.++)

{-# INLINE appendright #-}
appendright :: [a] -> a -> [a]
appendright l p = l Prelude.++ [p]

-- | dummy function for ABS n-ary constructors
{-# INLINE list #-}
list :: [a] -> [a]
list = Prelude.id

-------- MAPS---------------
----------------------------

{-# INLINE put #-}
put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

{-# INLINE insert #-}
insert :: Prelude.Ord k => M.Map k v -> (k,v) -> M.Map k v
insert m (k,v) = M.insert k v m

{-# INLINE lookupUnsafe #-}
lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k

{-# INLINE lookupMaybe #-}
lookupMaybe :: Prelude.Ord k => M.Map k v -> k -> Prelude.Maybe v
lookupMaybe = Prelude.flip M.lookup

-- | Returns the value associated with key 'k' in map 'ms', or the value 'd'
-- if 'k' has no entry in 'ms'.
{-# INLINE lookupDefault #-}
lookupDefault :: Prelude.Ord k => M.Map k a -> k -> a -> a
lookupDefault ms k d = M.findWithDefault d k ms

{-# INLINE removeKey #-}
removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

-- | Constructing maps from an association list.
{-# INLINE map #-}
map :: Prelude.Ord k => List (Pair k a) -> M.Map k a
map = M.fromList

{-# INLINE values #-}
values :: M.Map k a -> (List a)
values = M.elems

-- | Constructor of empty Maps, 'EmptyMap' in ABS
{-# INLINE _emptyMap #-}
_emptyMap :: M.Map k a
_emptyMap = M.empty


{-# INLINE keys #-}
keys :: M.Map k a -> S.Set k
keys = M.keysSet

-------- SETS---------------
----------------------------

{-# INLINE set #-}
set :: Prelude.Ord a => List a -> S.Set a
set = S.fromList

{-# INLINE contains #-}
contains :: Prelude.Ord a => S.Set a -> a -> Bool
contains = Prelude.flip S.member

{-# INLINE emptySet #-}
emptySet :: S.Set a -> Bool
emptySet = S.null

{-# INLINE _emptySet #-}
_emptySet :: S.Set a
_emptySet = S.empty

{-# INLINE insertElement #-}
insertElement :: Prelude.Ord a => S.Set a -> a -> S.Set a
insertElement = Prelude.flip S.insert

{-# INLINE remove #-}
remove :: Prelude.Ord a => S.Set a -> a -> S.Set a
remove = Prelude.flip S.delete

-- | Returns one (arbitrary) element from a set.
-- To iterate over a set, take one element and remove it from the set.
-- Repeat until set is empty.
{-# INLINE take #-}
take :: Prelude.Ord a => S.Set a -> a
take = S.findMin

-------- TUPLES-------------
----------------------------

type Pair a b = (a,b)

type Triple a b c = (a,b,c)
fstT :: Triple a b c -> a
fstT (a,_,_) = a
sndT :: Triple a b c -> b
sndT (_,b,_) = b
trd :: Triple a b c -> c
trd (_,_,c) = c

-- | Deconstructs _unsafely_ the left part of an Either
left :: Prelude.Either a b -> a
left (Prelude.Left a ) = a
left _ = Prelude.error "not a left-Either"

-- | Deconstructs _unsafely_  the right part of an Either
right :: Prelude.Either a b -> b
right (Prelude.Right a) = a
right _ = Prelude.error "not a right-Either"

-------- PURE ARRAYS--------
----------------------------

-- | A pure (persistent) array with O(1) random access
type Array = BArray.Array

{-# INLINE replace #-}
replace :: (BArray.IArray a e, BArray.Ix i) => a i e -> [(i, e)] -> a i e
replace a cs = a BArray.// cs

{-# INLINE elemAt #-}
elemAt :: (BArray.IArray a e, BArray.Ix i) => (a i e, i) -> e
elemAt(a, i) = a BArray.! i


-------- STRINGS------------
----------------------------


println :: ABS Prelude.String -> ABS ()
println act = act Prelude.>>= \ s -> liftIO (Prelude.putStrLn s)

readln :: ABS Prelude.String
readln = liftIO Prelude.getLine

{-# INLINE toString #-}
toString :: (Prelude.Show a) => a -> Prelude.String
toString = Prelude.show

-- | Returns a string with the base-10 textual representation of 'n'.
-- Note: Will work the same as toString. Just a carry-over from the other frontend.
{-# INLINE intToString #-}
intToString :: Int -> Prelude.String
intToString = Prelude.show

-- | Returns a substring of string str of the given length starting from start (inclusive)
-- Where the first character has index 0
-- 
-- Example:
--    substr("abcde",1,3) => "bcd"
{-# INLINE substr #-}
substr :: Prelude.String -> Int -> Int -> Prelude.String
substr str d len = Prelude.take len (Prelude.drop d str)

{-# INLINE strlen #-}
strlen :: Prelude.String -> Int
strlen = Prelude.length


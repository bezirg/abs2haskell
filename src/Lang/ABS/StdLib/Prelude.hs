{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}

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
     list, length, isEmpty, nth, concatenate, appendright, without, Prelude.repeat, Prelude.reverse, copy,
     -- * Functions for "Array" datastructures
     listArray, replace, elemAt,
     -- * The ABS Map datatype and its functions
     M.Map, map, M.empty, put, insert, lookupUnsafe, lookupMaybe, lookupDefault, removeKey, M.keys, values,
     -- * The ABS Set datatype and its functions
     S.Set, set, emptySet, S.size, contains, S.union, S.intersection, S.difference, insertElement, remove, take,
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
     pure, (<$>), (<*>)
    )
        where

import qualified Prelude as Prelude
import Lang.ABS.Runtime.Base

import Control.Applicative

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Data.Either (isLeft, isRight)
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (listArray)
import Data.List (length)
import Control.Monad.IO.Class (liftIO)

-- | ABS Number are either 'Int'egers or 'Rat'ionals
class (Prelude.Num a) => Number a where
    (/) :: a -> a -> a

instance Number Int where
    (/) = Prelude.div

instance Number Rat where
    (/) = (Prelude./)

-- | Modulo operation. Takes strictly two integers
-- and returns a polymorphic number (either Int or Rational, based on the followup computation).
(%) :: Number b => Int -> Int -> b
x % y = Prelude.fromIntegral (x `Prelude.mod` y)

-- | Raising a number to a non-negative integer power
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
nth :: List a -> Int -> a
nth = (Prelude.!!)

-- | Checks if the list is empty.
isEmpty :: List a -> Bool
isEmpty = Prelude.null

-- | Replicating an element 'n' times, forming a list of length n.
copy :: a -> Int -> List a
copy = Prelude.flip Prelude.replicate

-- | Removes all occurences of an element from a list
without :: Prelude.Eq t => [t] -> t -> [t]
without [] _ = []
without (x:xs) a | x Prelude.== a = without xs a 
                 | Prelude.otherwise = x : without xs a

concatenate :: [a] -> [a] -> [a]
concatenate = (Prelude.++)

appendright :: [a] -> a -> [a]
appendright l p = l Prelude.++ [p]

-- | dummy function for ABS n-ary constructors
list :: [a] -> [a]
list = Prelude.id

-------- MAPS---------------
----------------------------

put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

insert :: Prelude.Ord k => M.Map k v -> (k,v) -> M.Map k v
insert m (k,v) = M.insert k v m

lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k

lookupMaybe :: Prelude.Ord k => M.Map k v -> k -> Prelude.Maybe v
lookupMaybe = Prelude.flip M.lookup

-- | Returns the value associated with key 'k' in map 'ms', or the value 'd'
-- if 'k' has no entry in 'ms'.
lookupDefault :: Prelude.Ord k => M.Map k a -> k -> a -> a
lookupDefault ms k d = M.findWithDefault d k ms

removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

-- | Constructing maps from an association list.
map :: Prelude.Ord k => List (Pair k a) -> M.Map k a
map = M.fromList

values :: Prelude.Ord k => M.Map k v -> List v
values = M.elems


-------- SETS---------------
----------------------------

set :: Prelude.Ord a => List a -> S.Set a
set = S.fromList

contains :: Prelude.Ord a => S.Set a -> a -> Bool
contains = Prelude.flip S.member

emptySet :: S.Set a -> Bool
emptySet = S.null

insertElement :: Prelude.Ord a => S.Set a -> a -> S.Set a
insertElement = Prelude.flip S.insert

remove :: Prelude.Ord a => S.Set a -> a -> S.Set a
remove = Prelude.flip S.delete

-- | Returns one (arbitrary) element from a set.
-- To iterate over a set, take one element and remove it from the set.
-- Repeat until set is empty.
take :: Prelude.Ord a => S.Set a -> a
take = S.elemAt 0

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
type Array = UArray.UArray

replace :: (UArray.IArray a e, UArray.Ix i) => a i e -> [(i, e)] -> a i e
replace a cs = a UArray.// cs

elemAt :: (UArray.IArray a e, UArray.Ix i) => (a i e, i) -> e
elemAt(a, i) = a UArray.! i


-------- STRINGS------------
----------------------------


println :: (Root_ o) => ABS o Prelude.String -> ABS o ()
println act = act Prelude.>>= \ s -> liftIO (Prelude.putStrLn s)

readln :: (Root_ o) => ABS o Prelude.String
readln = liftIO Prelude.getLine

toString :: (Prelude.Show a) => a -> Prelude.String
toString = Prelude.show

-- | Returns a string with the base-10 textual representation of 'n'.
-- Note: Will work the same as toString. Just a carry-over from the other frontend.
intToString :: Int -> Prelude.String
intToString = Prelude.show

-- | Returns a substring of string str of the given length starting from start (inclusive)
-- Where the first character has index 0
-- 
-- Example:
--    substr("abcde",1,3) => "bcd"
substr :: Prelude.String -> Int -> Int -> Prelude.String
substr str d len = Prelude.take len (Prelude.drop d str)

strlen :: Prelude.String -> Int
strlen = Prelude.length


%%[99
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Arr
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- EHC\'s array implementation.
-- 
-----------------------------------------------------------------------------

module UHC.Array where

import UHC.Base
import UHC.Show
import UHC.ST
import UHC.Ix
import UHC.BoxArray
-- import Debug.Trace

infixl 9  !, //
%%]

Array types

%%[99
type IPr = (Int, Int)

-- | The type of immutable non-strict (boxed) arrays
-- with indices in @i@ and elements in @e@.
-- The Int is the number of elements in the Array.
data Ix i => Array i e
                 = Array !i         -- the lower bound, l
                         !i         -- the upper bound, u
                         !Int       -- a cache of (rangeSize (l,u))
                                    -- used to make sure an index is
                                    -- really in range
                         (BoxArray e) -- The actual elements

-- | Mutable, boxed, non-strict arrays in the 'ST' monad.  The type
-- arguments are as follows:
--
--  * @s@: the state variable argument for the 'ST' type
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.
--
data STArray s i e
         = STArray !i                  -- the lower bound, l
                   !i                  -- the upper bound, u
                   !Int                -- a cache of (rangeSize (l,u))
                                       -- used to make sure an index is
                                       -- really in range
                   (MutableBoxArray s e) -- The actual elements
        -- No Ix context for STArray.  They are stupid,
        -- and force an Ix context on the equality instance.

-- Just pointer equality on mutable arrays:
instance Eq (STArray s i e) where
    STArray _ _ _ arr1 == STArray _ _ _ arr2 =
        sameMutableArray arr1 arr2
%%]

Operations on immutable arrays

%%[99
{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"

{-# INLINE array #-}
-- | Construct an array with the specified bounds and containing values
-- for given indices within these bounds.
--
-- The array is undefined (i.e. bottom) if any index in the list is
-- out of bounds.  The Haskell 98 Report further specifies that if any
-- two associations in the list have the same index, the value at that
-- index is undefined (i.e. bottom).  However in GHC's implementation,
-- the value at such an index is the value part of the last association
-- with that index in the list.
--
-- Because the indices must be checked for these errors, 'array' is
-- strict in the bounds argument and in the indices of the association
-- list, but nonstrict in the values.  Thus, recurrences such as the
-- following are possible:
--
-- > a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])
--
-- Not every index within the bounds of the array need appear in the
-- association list, but the values associated with indices that do not
-- appear will be undefined (i.e. bottom).
--
-- If, in any dimension, the lower bound is greater than the upper bound,
-- then the array is legal, but empty.  Indexing an empty array always
-- gives an array-bounds error, but 'bounds' still yields the bounds
-- with which the array was constructed.
array :: Ix i
        => (i,i)        -- ^ a pair of /bounds/, each of the index type
                        -- of the array.  These bounds are the lowest and
                        -- highest indices in the array, in that order.
                        -- For example, a one-origin vector of length
                        -- '10' has bounds '(1,10)', and a one-origin '10'
                        -- by '10' matrix has bounds '((1,1),(10,10))'.
        -> [(i, e)]     -- ^ a list of /associations/ of the form
                        -- (/index/, /value/).  Typically, this list will
                        -- be expressed as a comprehension.  An
                        -- association '(i, x)' defines the value of
                        -- the array at index 'i' to be 'x'.
        -> Array i e
array (l,u) ies
    = let n = safeRangeSize (l,u)
      in unsafeArray' (l,u) n
                      [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeArray #-}
unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray b ies = letstrict x = unsafeArray' b (rangeSize b) ies in x

{-# INLINE unsafeArray' #-}
unsafeArray' :: Ix i => (i,i) -> Int -> [(Int, e)] -> Array i e
unsafeArray' (l,u) n ies = runST (ST $ \s1 ->
    case newArray n arrEleBottom s1 of
        (s2, marr) ->
            foldr (fill marr) (done l u n marr) ies s2)

{-
unsafeArray' :: Ix i => (i,i) -> Int -> [(Int, e)] -> Array i e
unsafeArray' (l,u) n ies = runST (ST $ \s1 ->
    case trace "unsafeArray'" $ newArray (trace "unsafeArray'.n" n) (trace "unsafeArray'.bot" arrEleBottom) (trace "unsafeArray'.s1" s1) of
        (s2, marr) ->
            trace "unsafeArray'.foldr" $ foldr (trace "unsafeArray'.fill" $ fill marr) (trace "unsafeArray'.done" $ done l u n marr) (trace "unsafeArray'.ies" ies) (trace "unsafeArray'.s2"  s2))
-}

{-# INLINE fill #-}
fill :: MutableBoxArray s e -> (Int, e) -> STRep s a -> STRep s a
fill marr (i, e) next s1 =
    letstrict w = writeArray marr i e s1 in case w of { s2 ->
    next s2 }

{-# INLINE done #-}
done :: Ix i => i -> i -> Int -> MutableBoxArray s e -> STRep s (Array i e)
done l u n marr s1 =
    case unsafeFreezeArray marr s1 of
        (s2, arr) -> (s2, Array l u n arr)

-- This is inefficient and I'm not sure why:
-- listArray (l,u) es = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es)
-- The code below is better. It still doesn't enable foldr/build
-- transformation on the list of elements; I guess it's impossible
-- using mechanisms currently available.

listArray :: Ix i => (i,i) -> [e] -> Array i e
listArray (l,u) es = letstrict x = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es) in x

{-# INLINE listArray #-}
{-
-- | Construct an array from a pair of bounds and a list of values in
-- index order.
listArray :: Ix i => (i,i) -> [e] -> Array i e
listArray (l,u) es = runST (ST $ \s1# ->
    case safeRangeSize (l,u)            of { n@(I# n#) ->
    case newArray n# arrEleBottom s1#  of { (# s2#, marr# #) ->
    let fillFromList i# xs s3# | i# ==# n# = s3#
                               | otherwise = case xs of
            []   -> s3#
            y:ys -> case writeArray marr# i# y s3# of { s4# ->
                    fillFromList (i# +# 1#) ys s4# } in
    case fillFromList 0# es s2#         of { s3# ->
    done l u n marr# s3# }}})
-}

{-# INLINE (!) #-}
-- | The value at the given index in an array.
(!) :: Ix i => Array i e -> i -> e
arr@(Array l u n _) ! i = unsafeAt arr $ safeIndex (l,u) n i

{-# INLINE safeRangeSize #-}
safeRangeSize :: Ix i => (i, i) -> Int
safeRangeSize (l,u) = let r = rangeSize (l, u)
                      in if r < 0 then error "Negative range size"
                                  else r

{-# INLINE safeIndex #-}
safeIndex :: Ix i => (i, i) -> Int -> i -> Int
safeIndex (l,u) n i = let i' = unsafeIndex (l,u) i
                      in if (0 <= i') && (i' < n)
                         then i'
                         else error "Error in array index"

{-# INLINE unsafeAt #-}
unsafeAt :: Ix i => Array i e -> Int -> e
unsafeAt (Array _ _ _ arr) i =
    indexArray arr i

{-# INLINE bounds #-}
-- | The bounds with which an array was constructed.
bounds :: Ix i => Array i e -> (i,i)
bounds (Array l u _ _) = (l,u)

{-# INLINE numElements #-}
-- | The number of elements in the array.
numElements :: Ix i => Array i e -> Int
numElements (Array _ _ n _) = n

{-# INLINE indices #-}
-- | The list of indices of an array in ascending order.
indices :: Ix i => Array i e -> [i]
indices (Array l u _ _) = range (l,u)

{-# INLINE elems #-}
-- | The list of elements of an array in index order.
elems :: Ix i => Array i e -> [e]
elems arr@(Array _ _ n _) =
    [unsafeAt arr i | i <- [0 .. n - 1]]

{-# INLINE assocs #-}
-- | The list of associations of an array in index order.
assocs :: Ix i => Array i e -> [(i, e)]
assocs arr@(Array l u _ _) =
    [(i, arr ! i) | i <- range (l,u)]

{-# INLINE accumArray #-}
-- | The 'accumArray' deals with repeated indices in the association
-- list using an /accumulating function/ which combines the values of
-- associations with the same index.
-- For example, given a list of values of some index type, @hist@
-- produces a histogram of the number of occurrences of each index within
-- a specified range:
--
-- > hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
-- > hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]
--
-- If the accumulating function is strict, then 'accumArray' is strict in
-- the values, as well as the indices, in the association list.  Thus,
-- unlike ordinary arrays built with 'array', accumulated arrays should
-- not in general be recursive.
accumArray :: Ix i
        => (e -> a -> e)        -- ^ accumulating function
        -> e                    -- ^ initial value
        -> (i,i)                -- ^ bounds of the array
        -> [(i, a)]             -- ^ association list
        -> Array i e
accumArray f initial (l,u) ies =
    let n = safeRangeSize (l,u)
    in unsafeAccumArray' f initial (l,u) n
                         [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeAccumArray #-}
unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(Int, a)] -> Array i e
unsafeAccumArray f initial b ies = unsafeAccumArray' f initial b (rangeSize b) ies

{-# INLINE unsafeAccumArray' #-}
unsafeAccumArray' :: Ix i => (e -> a -> e) -> e -> (i,i) -> Int -> [(Int, a)] -> Array i e
unsafeAccumArray' f initial (l,u) n ies = runST (ST $ \s1 ->
    case newArray n initial s1          of { (s2, marr) ->
    foldr (adjust f marr) (done l u n marr) ies s2 })

{-# INLINE adjust #-}
adjust :: (e -> a -> e) -> MutableBoxArray s e -> (Int, a) -> STRep s b -> STRep s b
adjust f marr (i, new) next s1 =
    case readArray marr i s1 of
        (s2, old) ->
            letstrict w = writeArray marr i (f old new) s2 in case w of
                s3 -> next s3

{-# INLINE (//) #-}
-- | Constructs an array identical to the first argument except that it has
-- been updated by the associations in the right argument.
-- For example, if @m@ is a 1-origin, @n@ by @n@ matrix, then
--
-- > m//[((i,i), 0) | i <- [1..n]]
--
-- is the same matrix, except with the diagonal zeroed.
--
-- Repeated indices in the association list are handled as for 'array':
-- Haskell 98 specifies that the resulting array is undefined (i.e. bottom),
-- but GHC's implementation uses the last association for each index.
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
arr@(Array l u n _) // ies =
    unsafeReplace arr [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeReplace #-}
unsafeReplace :: Ix i => Array i e -> [(Int, e)] -> Array i e
unsafeReplace arr ies = runST (do
    STArray l u n marr <- thawSTArray arr
    ST (foldr (fill marr) (done l u n marr) ies))

{-# INLINE accum #-}
-- | @'accum' f@ takes an array and an association list and accumulates
-- pairs from the list into the array with the accumulating function @f@.
-- Thus 'accumArray' can be defined using 'accum':
--
-- > accumArray f z b = accum f (array b [(i, z) | i <- range b])
--
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f arr@(Array l u n _) ies =
    unsafeAccum f arr [(safeIndex (l,u) n i, e) | (i, e) <- ies]

{-# INLINE unsafeAccum #-}
unsafeAccum :: Ix i => (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f arr ies = runST (do
    STArray l u n marr <- thawSTArray arr
    ST (foldr (adjust f marr) (done l u n marr) ies))

{-# INLINE amap #-}
amap :: Ix i => (a -> b) -> Array i a -> Array i b
amap f arr@(Array l u n _) =
    unsafeArray' (l,u) n [(i, f (unsafeAt arr i)) | i <- [0 .. n - 1]]

{-# INLINE ixmap #-}
-- | 'ixmap' allows for transformations on array indices.
-- It may be thought of as providing function composition on the right
-- with the mapping that the original array embodies.
--
-- A similar transformation of array values may be achieved using 'fmap'
-- from the 'Array' instance of the 'Functor' class.
ixmap :: (Ix i, Ix j) => (i,i) -> (i -> j) -> Array j e -> Array i e
ixmap (l,u) f arr =
    array (l,u) [(i, arr ! f i) | i <- range (l,u)]

{-# INLINE eqArray #-}
eqArray :: (Ix i, Eq e) => Array i e -> Array i e -> Bool
eqArray arr1@(Array l1 u1 n1 _) arr2@(Array l2 u2 n2 _) =
    if n1 == 0 then n2 == 0 else
    l1 == l2 && u1 == u2 &&
    and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. n1 - 1]]

{-# INLINE cmpArray #-}
cmpArray :: (Ix i, Ord e) => Array i e -> Array i e -> Ordering
cmpArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntArray #-}
cmpIntArray :: Ord e => Array Int e -> Array Int e -> Ordering
cmpIntArray arr1@(Array l1 u1 n1 _) arr2@(Array l2 u2 n2 _) =
    if n1 == 0 then
        if n2 == 0 then EQ else LT
    else if n2 == 0 then GT
    else case compare l1 l2 of
             EQ    -> foldr cmp (compare u1 u2) [0 .. (n1 `min` n2) - 1]
             other -> other
  where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpArray/Int" cmpArray = cmpIntArray #-}
%%]

Array instances

%%[99
instance Ix i => Functor (Array i) where
    fmap = amap

instance (Ix i, Eq e) => Eq (Array i e) where
    (==) = eqArray

instance (Ix i, Ord e) => Ord (Array i e) where
    compare = cmpArray

instance (Ix a, Show a, Show b) => Show (Array a b) where
    showsPrec p a =
        showParen (p > appPrec) $
        showString "array " .
        showsPrec appPrec1 (bounds a) .
        showChar ' ' .
        showsPrec appPrec1 (assocs a)
        -- Precedence of 'array' is the precedence of application

-- The Read instance is in GHC.Read
%%]

Operations on mutable arrays

%%[99
{-# INLINE newSTArray #-}
newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
newSTArray (l,u) initial = ST $ \s1 ->
    case safeRangeSize (l,u)            of { n ->
    case newArray n initial s1       of { ( s2, marr ) ->
    ( s2, STArray l u n marr ) }}

{-# INLINE boundsSTArray #-}
boundsSTArray :: STArray s i e -> (i,i)  
boundsSTArray (STArray l u _ _) = (l,u)

{-# INLINE numElementsSTArray #-}
numElementsSTArray :: STArray s i e -> Int
numElementsSTArray (STArray _ _ n _) = n

{-# INLINE readSTArray #-}
readSTArray :: Ix i => STArray s i e -> i -> ST s e
readSTArray marr@(STArray l u n _) i =
    unsafeReadSTArray marr (safeIndex (l,u) n i)

{-# INLINE unsafeReadSTArray #-}
unsafeReadSTArray :: Ix i => STArray s i e -> Int -> ST s e
unsafeReadSTArray (STArray _ _ _ marr) i
    = ST $ \s1 -> readArray marr i s1

{-# INLINE writeSTArray #-}
writeSTArray :: Ix i => STArray s i e -> i -> e -> ST s () 
writeSTArray marr@(STArray l u n _) i e =
    unsafeWriteSTArray marr (safeIndex (l,u) n i) e

{-# INLINE unsafeWriteSTArray #-}
unsafeWriteSTArray :: Ix i => STArray s i e -> Int -> e -> ST s () 
unsafeWriteSTArray (STArray _ _ _ marr) i e = ST $ \s1 ->
    letstrict s2 = writeArray marr i e s1 in ( s2, () )
%%]

Moving between mutable and immutable

%%[99
freezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
freezeSTArray (STArray l u n marr) = ST $ \s1 ->
    case newArray n arrEleBottom s1  of { ( s2, marr' ) ->
    let copy i s3 | i == n = s3
                  | otherwise =
            case readArray marr i s3 of { ( s4, e ) ->
            letstrict s5 = writeArray marr' i e s4 in 
            copy (i + 1) s5 } in
    -- This evaluates too strict, but must be done because of (later) side effects in the original array.
    -- Looks like the dual problem of thawSTArray.
    letstrict s3 = copy 0 s2 in
    case unsafeFreezeArray marr' s3  of { ( s4, arr ) ->
    ( s4, Array l u n arr ) }}

{-# INLINE unsafeFreezeSTArray #-}
unsafeFreezeSTArray :: Ix i => STArray s i e -> ST s (Array i e)
unsafeFreezeSTArray (STArray l u n marr) = ST $ \s1 ->
    case unsafeFreezeArray marr s1   of { ( s2, arr ) ->
    ( s2, Array l u n arr ) }

thawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
thawSTArray (Array l u n arr) = ST $ \s1 ->
    case newArray n arrEleBottom s1  of { ( s2, marr ) ->
    let copy i s3 | i == n = s3
                  | otherwise =
            -- There is currently no way (by lack of (un)boxing notation) to indicate we only want to evaluate indexArray, not the content.
            -- Either the closure, or the evaluated value is put in the new array.
            let e = indexArray arr i in
            letstrict s4 = writeArray marr i e s3 in 
            copy (i + 1) s4  in
    letstrict s3 = copy 0 s2 in
    ( s3, STArray l u n marr ) }

{-# INLINE unsafeThawSTArray #-}
unsafeThawSTArray :: Ix i => Array i e -> ST s (STArray s i e)
unsafeThawSTArray (Array l u n arr) = ST $ \s1 ->
    case unsafeThawArray arr s1      of { ( s2, marr ) ->
    ( s2, STArray l u n marr ) }
%%]

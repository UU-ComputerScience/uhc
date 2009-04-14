%%[99

module UHC.Ix
  ( Ix         (range, index, unsafeIndex, inRange, rangeSize)
  )
where

import UHC.Base
import UHC.Ord
import UHC.Show

%%]

%%[99
-- | The 'Ix' class is used to map a contiguous subrange of values in
-- a type onto integers.  It is used primarily for array indexing
-- (see the array package).
--
-- The first argument @(l,u)@ of each of these operations is a pair
-- specifying the lower and upper bounds of a contiguous subrange of values.
--
-- An implementation is entitled to assume the following laws about these
-- operations:
--
-- * @'inRange' (l,u) i == 'elem' i ('range' (l,u))@
--
-- * @'range' (l,u) '!!' 'index' (l,u) i == i@, when @'inRange' (l,u) i@
--
-- * @'map' ('index' (l,u)) ('range' (l,u))) == [0..'rangeSize' (l,u)-1]@
--
-- * @'rangeSize' (l,u) == 'length' ('range' (l,u))@
--
-- Minimal complete instance: 'range', 'index' and 'inRange'.
--
class (Ord a) => Ix a where
    -- | The list of values in the subrange defined by a bounding pair.
    range               :: (a,a) -> [a]
    -- | The position of a subscript in the subrange.
    index               :: (a,a) -> a -> Int
    -- | Like 'index', but without checking that the value is in range.
    unsafeIndex         :: (a,a) -> a -> Int
    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange             :: (a,a) -> a -> Bool
    -- | The size of the subrange defined by a bounding pair.
    rangeSize           :: (a,a) -> Int
    -- | like 'rangeSize', but without checking that the upper bound is
    -- in range.
    unsafeRangeSize     :: (a,a) -> Int

        -- Must specify one of index, unsafeIndex
    index b i | inRange b i = unsafeIndex b i   
              | otherwise   = error "Error in array index"
    unsafeIndex b i = index b i

    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
                       | otherwise   = 0        -- This case is only here to
                                                -- check for an empty range
        -- NB: replacing (inRange b h) by (l <= h) fails for
        --     tuples.  E.g.  (1,2) <= (2,1) but the range is empty

    unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1

%%]

Note that the following is NOT right
        rangeSize (l,h) | l <= h    = index b h + 1
                        | otherwise = 0

Because it might be the case that l<h, but the range
is nevertheless empty.  Consider
        ((1,2),(2,1))
Here l<h, but the second index ranges from 2..1 and
hence is empty


%%[99
-- abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp
  = error (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
           showString " out of range " $
           showParen True (showsPrec 0 rng) "")

----------------------------------------------------------------------
instance  Ix Char  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = fromEnum i - fromEnum m

    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Char"

    inRange (m,n) i     =  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Int  where
    {-# INLINE range #-}
        -- The INLINE stops the build in the RHS from getting inlined,
        -- so that callers can fuse with the result of range
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = i - m

    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Int"

    {-# INLINE inRange #-}
    inRange (m,n) i =  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Integer  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i   = fromInteger (i - m)

    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Integer"

    inRange (m,n) i     =  m <= i && i <= n

----------------------------------------------------------------------
instance Ix Bool where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Bool"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    index b i | inRange b i =  unsafeIndex b i
              | otherwise   =  indexError b i "Ordering"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE unsafeIndex #-}
    unsafeIndex   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True
    {-# INLINE index #-}
    index b i = unsafeIndex b i

%%]

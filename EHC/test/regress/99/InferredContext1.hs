{- ----------------------------------------------------------------------------------------
   what    : test for a previous bug: combi of inferred context, its usage, recusive setting, for nub'
   expected: ok now, before that: use of context for nub' was not/inconsistently propagated
---------------------------------------------------------------------------------------- -}

module InferredContext1 where

main = print (show (nub test))

test :: [Int]
test = [1,2,1,3]
{-
class Eq a where
  (==) :: a -> a -> Bool
data Bool = False | True
data ''[]'' a = a : [a] | ''[]''
elem :: Eq a => a -> [a] -> Bool
otherwise = True
-}

nub    :: (Eq a) => [a] -> [a]
nub xs  = nub' xs []
  where nub' [] _           = []
        nub' (x:xs) ys
            | x `elem` ys   = nub' xs ys
            | otherwise     = x : nub' xs (x:ys)


{-
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
-}

module Main where

-- nub :: Eq a => [a] -> [a]
nub :: {! Eq a !} -> [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

eqMod2 :: Int -> Int -> Bool
eqMod2 x y = x `mod` 2 == y `mod` 2

instance dEqInt = Eq Int where
  (==) = eqMod2

eqTrue :: a -> a -> Bool
eqTrue _ _ = True

instance dEqIntTrue = Eq Int where
  (==) = eqTrue

eqHead :: Eq a => [a] -> [a] -> Bool
eqHead (x:_) (y:_) = x == y
eqHead _     _     = False

instance dEqList = Eq a => Eq [a] where
  (==) = eqHead

instance dEqList2 = Eq [a] where
  [] == [] = False
  _  == _  = True

doEq :: Eq a => a -> a -> Bool
-- doEq {! Eq a = dEq !} a b = a == b
doEq = \{! Eq a = dEq !} a b -> (==) {! Eq a = dEq !} a b

{-
doEq2 :: Eq a => a -> a -> Bool
doEq2 = \{! Eq a = dEq !} a b -> (==) {! Eq a = dEq {(==) = eqTrue} !} a b
-- Issue: default values, not updated
-- Bug when dEq is not used?
-- Function equations not yet impl

doEq3 :: Eq a => a -> a -> Bool
doEq3 = \{! Eq a = dEq@(_'Eq { (==) = eq }) !} a b -> eq a b
-- Pattern match, allow operators also as fields
-- Name of dict type, denotable, convention for it
-}

-- Bug: cannot do
-- doEq4 :: (forall b . Eq b => Eq [b]) => a -> a -> Bool
-- doEq4 :: {! forall b . Eq b => Eq [b] !} -> a -> a -> Bool
-- doEq4 :: {! Eq b => Eq [b] !} -> a -> a -> Bool
doEq4 :: {! Eq a => Eq [a] !} -> a -> a -> Bool
-- Bug: wildcard pattern should not be necessary,
--   ends up as forall a . (  forall b . {! UHC.Base.Eq b !} ...
--   instead of forall a . {! forall b . {! UHC.Base.Eq b !} ...
-- doEq4 = \_ a b -> True
-- Not impl: wildcard
-- doEq4 = \{! ... = dEqL !} a b -> True
doEq4 = \_ a b -> True

-- Bug: omission of value is not complained about
-- Issue: partial sigs, predicate could be omitted?

-- Issue: higher order, GRose

twoEq a b c d = (a == b, c == d)

twoEq2 :: forall a . Eq a => a -> a -> forall b . Eq b => b -> b -> (Bool, Bool)
twoEq2 a b c d = (a == b, c == d)

main = do
  let l  = [1 :: Int, 2, 2, 3, 1, 0]
      l2 = [1 :: Int, 2, 2, 3, 1, 4]
      l3 = [0 :: Int, 2, 2, 3, 1, 4]
  print $ nub l
  print $ let instance Eq Int = dEqInt
          in  nub l
  print $ nub {! Eq Int = dEqInt !} l
  print $ (==) l l2
  print $ (==) {! Eq [Int] = dEqList dEqInt !} l l2
  print $ (==) l l3
  print $ (==) {! Eq [Int] = dEqList dEqInt !} l l3
  print $ (==) {! Eq [Int] = dEqList2 !} l l3
  print $ doEq l l2
  -- print $ doEq4 l l2
  print $ twoEq  3 4 5 6
  print $ twoEq2 3 4 {! Eq Int = dEqIntTrue !} 5 6



{-
let  data List a  = Nil | Cons a (List a)                                            
     class Eq a where
       eq :: a -> a -> Bool                                                         
       ne :: a -> a -> Bool
     instance dEqInt <: Eq Int where                                                -- (1)
       eq = primEqInt
       ne = \x y -> not (eq x y)
     nub     ::  forall a . Eq a => List a -> List a
     nub     =   \xx ->  case xx of
                           Nil        -> Nil
                           Cons x xs  -> Cons x (nub (filter (ne x) xs))
     eqMod2 :: Int -> Int -> Bool
     eqMod2 = \x y -> eq (mod x 2) (mod y 2)
     n1 = nub  {!  dEqInt <: Eq Int !}                                              -- (2)
               (Cons 3 (Cons 3 (Cons 4 Nil)))
     n2 = nub  {!  ( eq =  eqMod2                                                   -- (2)
                   , ne =  \x y -> not (eqMod2 x y)
                   )  <: Eq Int
               !}
               (Cons 3 (Cons 3 (Cons 4 Nil)))
in   ...
-}

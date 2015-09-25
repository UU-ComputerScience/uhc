module Main where

import Data.List (nubBy)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

{-
nub2 :: {! Eq a !} -> [a] -> [a]
nub2 = \{! Eq a = dEq !} l -> case l of
  [] -> []
  (x:xs) -> x : nub2 (filter ((/=) {! Eq a = dEq !} x) xs)
-- Bug at grin level
-}

-- nub3 :: {! Eq a !} -> [a] -> [a]
nub3 = \{! Eq a = dEq !} -> nubBy ((==) {! Eq a = dEq !})
nub4 = nubBy (==)

eqMod2 :: Int -> Int -> Bool
-- eqMod2 :: (Eq a, Integral a) => a -> a -> Bool
-- eqMod2 :: Integral a => a -> a -> Bool
eqMod2 x y = x `mod` 2 == y `mod` 2

-- Issue: signature is required to avoid late resolution in the context of the instance
--  Ok sig :: Integral a => a -> a -> Bool
--  Not ok sig :: (Eq a, Integral a) => a -> a -> Bool
-- Bug: actually is inferred
--  :: {! UHC.Base.Integral a !} -> {! UHC.Base.Eq a !} -> {! UHC.Base.Num a !} -> a -> a -> UHC.Base.Bool

instance dEqInt = Eq Int where
  (==) = eqMod2

-- causes overlap:
-- instance Eq Int = dEqInt

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

twoEq3 :: forall a b . (Eq a, Eq b) => a -> a -> b -> b -> (Bool, Bool)
twoEq3 a b c d = (a == b, c == d)

twoEq4 :: forall a b . (Eq b, Eq a) => a -> a -> b -> b -> (Bool, Bool)
twoEq4 a b c d = (a == b, c == d)

-- >>> Paper example
f  ::    Eq a =>  a ->  a ->  Int
f  =   \          x     y ->  if x == y then 3 else 4

v1 = f  2 4
v2 = f  {! Eq Int = dEqInt !}
        2 4
-- <<< Paper example

p msg x = putStrLn $ msg ++ ": " ++ show x

main = do
  let l  = [1 :: Int, 2, 2, 3, 1, 0]
      l2 = [1 :: Int, 2, 2, 3, 1, 4]
      l3 = [0 :: Int, 2, 2, 3, 1, 4]
{-
-}
  p "nub                " $ nub l
  p "nub local inst     " $ let instance Eq Int = dEqInt
                            in  nub l
  p "nub expl           " $ nub {! Eq Int = dEqInt !} l
  p "nub3               " $ nub3 l
  p "nub3 expl          " $ nub3 {! Eq Int = dEqInt !} l
  p "nub4               " $ nub4 l
  p "nub4 expl          " $ nub4 {! Eq Int = dEqInt !} l
  p "l == l2            " $ (==) l l2
  p "l == l2 expl       " $ (==) {! Eq [Int] = dEqList dEqInt !} l l2
  p "l == l3            " $ (==) l l3
  p "l == l3 expl       " $ (==) {! Eq [Int] = dEqList dEqInt !} l l3
  p "l == l3 expl       " $ (==) {! Eq [Int] = dEqList2 !} l l3
  p "doEq l l2          " $ doEq l l2
  -- p "nub" $ doEq4 l l2
  p "twoEq  3 4      5 6" $ twoEq  3 4 5 6
  p "twoEq2 3 4 expl 5 6" $ twoEq2 3 4 {! Eq Int = dEqIntTrue !} 5 6
  p "twoEq3 3 4 expl 5 6" $ twoEq3 {! Eq Int = dEqIntTrue !} 3 4 5 6
  p "twoEq4 3 4 expl 5 6" $ twoEq4 {! Eq Int = dEqIntTrue !} 3 4 5 6
  putStrLn $ show v1 ++ "," ++ show v2



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

-- Other issues:
--  interaction with name analysis, i.e. explicit relying per module on implicit.
--  per module scoping as a weaker form

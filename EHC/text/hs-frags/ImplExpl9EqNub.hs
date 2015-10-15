{-# LANGUAGE NoGenericDeriving #-}

module Main where

class CTst a where
  ctst1 :: a -> Int
  ctst2 :: Bool

data DTst a
  = Dtst1 {dtstfld1 :: a}
  | Dtst2
{-
data DTst a where
  Dtst1 :: {dtstfld1 :: a} -> DTst a
  Dtst2 :: DTst a
-}

eqInteger :: Integer -> Integer -> Bool
eqInteger = (==)
eqInteger2 = eqInteger

instance dEqInt :: Eq Integer where
  i1 == i2 = (i1 `mod` 2) `eqInteger` (i2 `mod` 2)
  
-- instance dEqInt <: Eq Integer
  
testInt = do
  print $ 1 == 3
{-
-}
  print $ let instance Eq Integer where                                                -- (2)
                i1 == i2 = (i1 `mod` 2) `eqInteger` (i2 `mod` 2)
          in  1 == 3
  print $ let instance dEqInt <: Eq Integer
          in  1 == 3

newtype I a = I a

instance dEqI <: Eq (I Int) where                                                -- (1)
  I i1 == I i2 = i1 == i2

instance dEqI2 :: Eq (I Int) where                                                -- (2)
  I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2

{-

{-
f1 x y = let instance Eq (I Int) where                                                -- (2)
               I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
         in  I x == I y
-}
-- f2 :: Int -> Int -> Bool
-- f2 :: (Integral a, Eq a) => a -> a -> Bool
f2 x y = let instance dEqI3 <: (Eq a, Integral a) => Eq (I a) where                                                -- (2)
               I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
         in  I x == I y
-- variations that should work?
-- omission of (Eq a, Integral a) and let tyvar a be brought into scope via x

testI = do
  let o = 1 :: Int
      t = 3 :: Int
  print $ I o == I t
  print $ let instance dEqI4 <: Eq (I Int) where                                                -- (2)
                I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
          in  I o == I t
  -- print $ f1 o t
  print $ f2 o t
-}

main = do
  -- print dEqInt
  testInt

-- main = return ()

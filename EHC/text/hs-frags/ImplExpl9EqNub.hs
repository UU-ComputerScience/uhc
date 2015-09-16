{-# LANGUAGE NoGenericDeriving #-}

module Main where

newtype I a = I a

instance dEqI <: Eq (I Int) where                                                -- (1)
  I i1 == I i2 = i1 == i2

instance dEqI2 :: Eq (I Int) where                                                -- (2)
  I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2

{-
f1 x y = let instance Eq (I Int) where                                                -- (2)
               I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
         in  I x == I y
-}
-- f2 :: Int -> Int -> Bool
f2 :: (Integral a, Eq a) => a -> a -> Bool
f2 x y = let instance dEqI3 <: (Eq a, Integral a) => Eq (I a) where                                                -- (2)
               I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
         in  I x == I y

main = do
  let o = 1 :: Int
      t = 3 :: Int
  print $ I o == I t
  print $ let instance dEqI4 <: Eq (I Int) where                                                -- (2)
                I i1 == I i2 = i1 `mod` 2 == i2 `mod` 2
          in  I o == I t
  -- print $ f1 o t
  print $ f2 o t

{-# LANGUAGE RankNTypes #-}

module TestLude where

-- data List a = Cons a (List a)
        -- | Nil
data Bool = True | False

-- infixr 1 $

foreign import ccall "Prelude" (+)     :: Int -> Int -> Int
foreign import ccall "Prelude" (&&)     :: Bool -> Bool -> Bool
-- foreign import ccall "Prelude" (-)     :: Int -> Int -> Int
-- foreign import ccall "Prelude" (==)     :: a -> a -> Bool
-- foreign import ccall "Prelude" (<)     :: a -> a -> Bool

-- otherwise :: Bool
-- otherwise = True

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- (>=) :: a -> a -> Bool
-- a >= b = not $ a < b

-- not :: Bool -> Bool
-- not True = False
-- not False = True

-- (++) :: List a -> List a -> List a
-- Nil         ++ ys = ys
-- (Cons x xs) ++ ys = Cons x $ xs ++ ys

-- filter :: (a -> Bool) -> List a -> List a
-- filter _pred Nil        = Nil
-- filter pred (Cons x xs) 
  -- | pred x    = Cons x $ filter pred xs
  -- | otherwise = filter pred xs
  
-- qsort :: List a -> List a
-- qsort Nil         = Nil
-- qsort (Cons x xs) = qsort (filter (< x) xs) ++ (Cons x Nil) ++ qsort (filter (>= x) xs)

-- map f Nil         = Nil
-- map f (Cons x xs) = Cons (f x) (map f xs)

id x = x
-- g (True ,x) = id x
-- g (False,x) = (+x)
q True  x = id x
q False x = (+x)

f True = False
f x    = x && True

-- otherwise = True

-- filter :: (a -> Bool) -> List a -> List a
-- filter _pred Nil        = Nil
-- filter pred (Cons x xs) 
  -- | pred x    = Cons x (filter pred xs)
  -- | otherwise = filter pred xs

-- foldr :: (a -> b -> b) -> b -> List a -> b
-- foldr :: forall a . forall b . (a -> (b -> b)) -> b -> (List a) -> b
-- foldr k z = go
          -- where
            -- go Nil     = z
            -- go (Cons y ys) = y `k` go ys
      
-- oi :: (a -> (a -> a))      
-- oi x y = x

-- oi = \f -> (\x -> f x) (\x -> f x)
            
--  This is crashing, but it should not.. Look at it later please.
-- fix f = (\x -> f x x) (\x -> f x x)

-- fix f = f (fix f)
-- apply f v t = if t == 0 then v else apply f (f v) (t - 1)
            
-- len Nil = 0
-- len (Cons _ xs) = 1 + len xs

-- main = qsort

fix f = f (fix f)
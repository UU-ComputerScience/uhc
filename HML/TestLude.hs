{-# LANGUAGE RankNTypes #-}

module TestLude where

data List a = Cons a (List a)
        | Nil
data Bool = True | False

infixr 1 $

foreign import ccall "Prelude" (<)     :: a -> a -> Bool

otherwise :: Bool
otherwise = True

($) :: (a -> b) -> a -> b
f $ x = f x

(>=) :: a -> a -> Bool
a >= b = not $ a < b

not :: Bool -> Bool
not True = False
not False = True

(++) :: List a -> List a -> List a
Nil         ++ ys = ys
(Cons x xs) ++ ys = Cons x $ xs ++ ys

filter :: (a -> Bool) -> List a -> List a
filter _pred Nil        = Nil
filter pred (Cons x xs) 
  | pred x    = Cons x $ filter pred xs
  | otherwise = filter pred xs
  
qsort :: List a -> List a
qsort Nil         = Nil
qsort (Cons x xs) = qsort (filter (< x) xs) ++ (Cons x Nil) ++ qsort (filter (>= x) xs)

-- main = qsort
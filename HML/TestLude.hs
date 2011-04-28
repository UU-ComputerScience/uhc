{-# LANGUAGE RankNTypes #-}

module TestLude where

-- import Prelude hiding (map)

-- data List a = Cons a (List a)
        -- | Nil

-- choose    :: forall a. a -> a -> a
-- choose = undefined

-- inc  :: Int -> Int
-- inc = undefined

-- -- polymorphic functions
-- ids      :: List (forall a. a -> a)
-- ids = undefined

-- single :: forall a. a -> List a
-- single = undefined

-- map    :: forall a b. (a -> b) -> List a -> List b
-- map = undefined

-- append :: forall a. List a -> List a -> List a
-- append = undefined

poly = \(f::forall a. a -> a) -> (f 1, f 'a')

infixr 1 $

data List a = Cons a (List a)
        | Nil
data Bool = True | False
-- data Tuple2 a b = Tuple2 a b
-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just a
-- data Ordering = LT | EQ | GT
         
foreign import ccall "Prelude" (-)     :: Int -> Int -> Int
foreign import ccall "Prelude" (+)     :: Int -> Int -> Int
foreign import ccall "Prelude" inc     :: Int -> Int
-- foreign import ccall "Prelude" map     :: forall a b. (a -> b) -> List a -> List b
-- foreign import ccall "Prelude" foldr   :: forall a b. (a -> b -> b) -> b -> List a -> b
-- foreign import ccall "Prelude" foldl   :: forall a b. (a -> b -> a) -> a -> List b -> a
-- foreign import ccall "Prelude" zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c    
-- foreign import ccall "Prelude" zip     :: List a -> List b -> List (Tuple2 a b)
-- foreign import ccall "Prelude" fst     :: Tuple2 a b -> a
-- foreign import ccall "Prelude" snd     :: Tuple2 a b -> b
-- foreign import ccall "Prelude" (!!)    :: List a -> Int -> a
-- foreign import ccall "Prelude" (&&)    :: Bool -> Bool -> Bool
-- foreign import ccall "Prelude" not     :: Bool -> Bool
-- foreign import ccall "Prelude" (++)    :: List a -> List a -> List a
-- foreign import ccall "Prelude" filter  :: (a -> Bool) -> List a -> List a
-- foreign import ccall "Prelude" all     :: (a -> Bool) -> List a -> Bool
-- foreign import ccall "Prelude" and     :: List Bool -> Bool 
-- foreign import ccall "Prelude" ($) :: forall a b.(a -> b) -> a -> b   

foreign import ccall "Prelude" (==)     :: a -> a -> Bool
foreign import ccall "Prelude" (<)     :: a -> a -> Bool
foreign import ccall "Prelude" single     :: forall a. a -> List a

-- foreign import ccall "Prelude" (>=)    :: a -> a -> Bool

-- ($) :: (a -> b) -> a -> b
f $ x = f x

-- (>=) :: a -> a -> Bool
a >= b = not  $ a < b

-- const a b = a

-- not :: Bool -> Bool
not True = False
not False = True

-- (++) :: List a -> List a -> List a
Nil         ++ ys = ys
(Cons x xs) ++ ys = Cons x $ xs ++ ys

(f . g) x = f (g x)
-- id :: a -> a
id x = x    
f $ x = f x
const a b = a

app = \(f :: forall a. a -> Int) -> Cons (f 0) (Cons (f 'a') Nil)

-- foo :: a -> (Int, a, Int)
-- foo :: (Int, a, Int)
-- foo = (1, 'C', 2)

map f Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

-- g (True ,x) = id x
-- g (False,x) = (+x)

otherwise = True

-- filter :: (a -> Bool) -> List a -> List a
filter _pred Nil        = Nil
filter pred (Cons x xs) 
  | pred x    = Cons x (filter pred xs)
  | otherwise = filter pred xs

-- q True  x = id x
-- q False x = (+x)

-- f True = False
-- f x    = x && True

-- qsort :: List a -> List a
qsort Nil         = Nil
qsort (Cons x xs) = qsort (filter (< x) xs) ++ (Cons x Nil) ++ qsort (filter (>= x) xs)

-- foldr _ z Nil         =  z
-- foldr f z (Cons x xs) =  f x (foldr f z xs)
-- foldr :: (a -> b -> b) -> b -> List a -> b
-- foldr :: forall a . forall b . (a -> (b -> b)) -> b -> (List a) -> b
foldr k z = go
          where
            go Nil     = z
            go (Cons y ys) = y `k` go ys

-- oi = \f -> (\x -> f x) (\x -> f x)

fix f = f (fix f)
apply f v t = if t == 0 then v else apply f (f v) (t - 1)
            
len Nil = 0
len (Cons _ xs) = 1 + len xs
module TestLude where

data List a = Cons a (List a)
        | Nil
data Bool = True | False
-- data Tuple2 a b = Tuple2 a b
-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just a
-- data Ordering = LT | EQ | GT
         
foreign import ccall "Prelude" (+)     :: Int -> Int -> Int
-- foreign import ccall "Prelude" map     :: forall a b. (a -> b) -> List a -> List b
-- foreign import ccall "Prelude" foldr   :: forall a b. (a -> b -> b) -> b -> List a -> b
-- foreign import ccall "Prelude" foldl   :: forall a b. (a -> b -> a) -> a -> List b -> a
-- foreign import ccall "Prelude" zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c    
-- foreign import ccall "Prelude" zip     :: List a -> List b -> List (Tuple2 a b)
-- foreign import ccall "Prelude" fst     :: Tuple2 a b -> a
-- foreign import ccall "Prelude" snd     :: Tuple2 a b -> b
-- foreign import ccall "Prelude" (!!)    :: List a -> Int -> a
foreign import ccall "Prelude" (&&)    :: Bool -> Bool -> Bool
-- foreign import ccall "Prelude" (++)    :: List a -> List a -> List a
-- foreign import ccall "Prelude" all     :: (a -> Bool) -> List a -> Bool
-- foreign import ccall "Prelude" and     :: List Bool -> Bool 

-- -- (f . g) x = f (g x)
-- id :: a -> a
id x = x    
-- f $ x = f x
-- const a b = a

-- app = \(f :: forall a. a -> Int) -> Cons (f 0) (Cons (f 'a') Nil)

-- foo :: a -> (Int, a, Int)
-- foo = (1, 'C', 2)

-- tmap f Nil         = Nil
-- tmap f (Cons x xs) = Cons (f x) (tmap f xs)

-- g (True ,x) = id x
-- g (False,x) = (+x)

-- filter :: (a -> Bool) -> List a -> List a
-- otherwise = True
-- filter _pred Nil        = Nil
-- filter pred (Cons x xs) 
  -- | pred x    = Cons x (filter pred xs)
  -- | otherwise = filter pred xs

-- q True  x = id x
-- q False x = (+x)

-- f True = False
-- f x    = x && True

Nil         ++ ys = ys
(Cons x xs) ++ ys = Cons x (xs ++ ys)
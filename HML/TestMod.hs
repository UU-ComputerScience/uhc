-- > 0
{-# LANGUAGE RankNTypes #-}

module TestMod where

infixr 1 $

data List a = Cons a (List a)
        | Nil
data Bool = True | False
data ST s a  = ST (State s -> (State s, a))
data State s = State s
data Ref s a = Ref s a
data Tup2 a b = Tup2 a b

foreign import ccall "Prelude" (+)     :: Int -> Int -> Int
-- < 0

-- > 1
-- builtin functions
foreign import ccall "Prelude" ($) :: forall a b. (a -> b) -> a -> b
foreign import ccall "Prelude" iff :: forall a. Bool -> a -> a -> a
 
-- standard functions
foreign import ccall "Prelude" id        :: forall a. a -> a
foreign import ccall "Prelude" apply     :: forall a b. (a -> b) -> a -> b
foreign import ccall "Prelude" const     :: forall a b. a -> b -> a
foreign import ccall "Prelude" choose    :: forall a. a -> a -> a
foreign import ccall "Prelude" revapp    :: forall a b. a -> (a -> b) -> b
foreign import ccall "Prelude" undefined :: forall a. a

--  integers
foreign import ccall "Prelude" plus :: Int -> Int -> Int
foreign import ccall "Prelude" lt   :: Int -> Int -> Bool
foreign import ccall "Prelude" gt   :: Int -> Int -> Bool
foreign import ccall "Prelude" inc  :: Int -> Int

-- polymorphic functions
foreign import ccall "Prelude" ids      :: List (forall a. a -> a)
foreign import ccall "Prelude" auto     :: (forall a. a -> a) -> (forall a. a -> a)
foreign import ccall "Prelude" xauto    :: forall a. (forall b. b -> b) -> a -> a
foreign import ccall "Prelude" takeAuto :: ((forall a. a -> a) -> (forall a. a -> a)) -> (forall a. a -> a)

-- lists
foreign import ccall "Prelude" single :: forall a. a -> List a
foreign import ccall "Prelude" head   :: forall a. List a -> a
foreign import ccall "Prelude" tail   :: forall a. List a -> List a
foreign import ccall "Prelude" map    :: forall a b. (a -> b) -> List a -> List b
foreign import ccall "Prelude" length :: forall a. List a -> Int
foreign import ccall "Prelude" null   :: forall a. List a-> Bool
foreign import ccall "Prelude" append :: forall a. List a -> List a -> List a

-- tuples
foreign import ccall "Prelude" fst :: forall a b. Tup2 a b -> a
foreign import ccall "Prelude" snd :: forall a b. Tup2 a b -> b

-- ST monad
foreign import ccall "Prelude" runST    :: forall a. (forall s. ST s a) -> a
foreign import ccall "Prelude" newRef   :: forall a s. a -> ST s (Ref s a)
foreign import ccall "Prelude" returnST :: forall a s. a -> ST s a
-- < 1

-- > 2
foreign import ccall "Prelude" foldr   :: forall a b. (a -> b -> b) -> b -> List a -> b
foreign import ccall "Prelude" foldl   :: forall a b. (a -> b -> a) -> a -> List b -> a
foreign import ccall "Prelude" zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c    
foreign import ccall "Prelude" zip     :: List a -> List b -> List (Tup2 a b)
foreign import ccall "Prelude" (!!)    :: List a -> Int -> a
foreign import ccall "Prelude" (&&)    :: Bool -> Bool -> Bool
foreign import ccall "Prelude" not     :: Bool -> Bool
foreign import ccall "Prelude" (++)    :: List a -> List a -> List a
foreign import ccall "Prelude" filter  :: (a -> Bool) -> List a -> List a
foreign import ccall "Prelude" all     :: (a -> Bool) -> List a -> Bool
foreign import ccall "Prelude" and     :: List Bool -> Bool 
-- < 2

-- > 3
foreign import ccall "Prelude" (<)     :: a -> a -> Bool
-- < 3

-- & Standard Prelude Test

-- # 1 "($)" {} []
-- @
($) :: (a -> b) -> a -> b
-- @ OK
f $ x = f x
-- # OK

-- # 2 "not" {0} []
-- @
not :: Bool -> Bool
-- @ OK
not True = False
not False = True
-- # OK

-- # 3 "(>=)" {0,3} [1,2] 
-- @
(>=) :: a -> a -> Bool
-- @ OK
a >= b = not  $ a < b
-- # OK


-- # 4 "(++)" {0} []
-- @
(++) :: List a -> List a -> List a
-- @ OK
Nil         ++ ys = ys
(Cons x xs) ++ ys = Cons x $ xs ++ ys
-- # OK

-- # 5 "(.)" {} []
(f . g) x = f (g x)
-- # OK

-- # 6 "id" {} []
-- @
id :: a -> a
-- @ OK
id x = x    
-- # OK

-- # 7 "($)" {} [1]
f $ x = f x
-- # OK

-- # 8 "const" {} []
const a b = a
-- # OK

-- # 9 "app" {0} []
app = \(f :: forall a. a -> Int) -> Cons (f 0) (Cons (f 'a') Nil)
-- # OK

-- # 10 "app id" {} [6,9]
-- # OK

-- # 11 "app (const 1)" {} [8,9]
-- # OK

-- & Tuple Test

-- # 12 "foo" {0} []
-- @
foo :: a -> (Int, a, Int)
-- @ FAIL
-- @
foo :: (Int, a, Int)
-- @ FAIL
foo = (1, 'C', 2)
-- # OK

-- # 13 "\x -> (x, x)" {} []
-- # OK

-- # 14 "(id, id)" {} [6]
-- # OK

-- # 15 "(id, id)" {1} []
-- # OK

-- # 16 "(Nil, Nil)" {0} []
-- # OK

-- & Case Unification Test

-- # 17 "map" {0} []
map f Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)
-- # OK

-- # 18 "g" {0} [6]
g (True ,x) = id x
g (False,x) = (+x)
-- # OK

-- # 19 "q" {0} [6]
q True  x = id x
q False x = (+x)
-- # OK

-- # 20 "otherwise" {0} []
otherwise = True
-- # OK

-- # 21 "filter" {0} [20]
-- @
filter :: (a -> Bool) -> List a -> List a
-- @ OK
filter _pred Nil        = Nil
filter pred (Cons x xs) 
  | pred x    = Cons x (filter pred xs)
  | otherwise = filter pred xs
-- # OK

-- # 22 "f" {0,2} []
f True = False
f x    = x && True
-- # OK

-- # 23 "qsort" {0} [3,21]
-- @
qsort :: List a -> List a
-- @ OK
qsort Nil         = Nil
qsort (Cons x xs) = qsort (filter (< x) xs) ++ (Cons x Nil) ++ qsort (filter (>= x) xs)
-- # OK

-- # 24 "foldr" {0} []
-- @
foldr :: (a -> b -> b) -> b -> List a -> b
-- @ OK
foldr _ z Nil         =  z
foldr f z (Cons x xs) =  f x (foldr f z xs)
-- # OK

-- # 25 "foldr" {0} []
-- @
foldr :: (a -> b -> b) -> b -> List a -> b
-- @ OK
foldr k z = go
          where
            go Nil     = z
            go (Cons y ys) = y `k` go ys
-- # OK

-- & Standard H&M Application test
            
-- # 26 "map (id id) (Cons 1 Nil)" {0} [6]
-- # OK
     
-- # 27 "((+1). id)" {0} [5, 6]
-- # OK

-- # 28 "(m . n)" {0,2} [5]
m = foldr map
n =  \t -> zipWith map t Nil
-- # OK

-- # 29 "map (id id)" {} [5  ,17]
-- # OK

-- # 30 "id id" {} [5]
-- # OK

-- # 31 "\f -> Cons (f 0) (Cons (f 'a') Nil)" {0} []
-- # OK

-- # 32 "Cons 1 Nil" {0} []
-- # OK

-- # 33 "Cons id Nil" {0} [5]
-- # OK

-- # 34 "map map" {} [17]
-- # OK

-- # 35 "idF" {} []
idF (x::forall a. a->a) = x
-- # OK

-- # 36 "\a -> Nil" {0} []
-- # OK

-- & Mutual recursion Test

-- # 37 "foo" {} []
foo = (bar :: Char)
bar = foo
-- # OK "Char"

-- & Checking Simple lambdas

-- # 38 "\a b -> a" {} []
-- # OK

-- # 39 "\a b c -> a" {} []
-- # OK

-- # 40 "\_ -> 'o'" {} []
-- # OK

-- & Standard Hindley-Milner Test

-- # 41 "\x -> x" {0,1} []
-- # OK

-- # 42 "\f x ->f x" {0,1} []
-- # OK

-- # 43 "inc True" {0,1} []
-- # FAIL

-- # 44 "let i = \x -> x in i i" {0,1} []
-- # OK

-- # 45 "\i -> i i" {0,1} []
-- # FAIL

-- # 46 "\i -> (i 1, i True)" {0,1} []
-- # FAIL

-- # 47 "single id" {0,1} []
-- # OK

-- # 48 "choose (\x y-> x) (\x y-> y)" {0,1} []
-- # OK

-- # 49 "choose id" {0,1} []
-- # OK

-- & Impredicative application & Higher rank arguments

-- # 50 "xauto" {0,1} []
-- # OK

-- # 51 "auto" {0,1} []
-- # OK

-- # 52 "\(i:: forall a. a -> a) -> i i" {0,1} []
-- # OK

-- # 53 "auto id" {0,1} []
-- # OK

-- # 54 "apply auto id" {0,1} []
-- # OK

-- # 55 "(single :: (forall a. a -> a) -> List (forall a. a -> a)) id" {0,1} []
-- # OK

-- # 56 "runST (returnST 1)" {0,1} []
-- # OK

-- # 57 "runST (newRef 1)" {0,1} []
-- # FAIL

-- # 58 "apply runST (returnST 1)" {0,1} []
-- # OK

-- # 59 "map xauto ids" {0,1} []
-- # OK

-- # 60 "map xauto (map xauto ids)" {0,1} []
-- # FAIL

-- # 61 "map auto ids" {0,1} []
-- # OK

-- # 62 "map auto (map auto ids)" {0,1} []
-- # OK

-- # 63 "head ids" {0,1} []
-- # OK

-- # 64 "tail ids" {0,1} []
-- # OK

-- # 65 "apply tail ids" {0,1} []
-- # OK

-- # 66 "map head (single ids)" {0,1} []
-- # OK

-- # 67 "apply (map head) (single ids)" {0,1} []
-- # OK

-- & Infinite poly types Test

-- # 68 "(undefined :: some a. List (a -> a) -> Int) (undefined :: some c. List ((forall d. d -> c) -> c))" {0,1} []
-- # FAIL

-- # 69 "(undefined :: some a. List (a -> a) -> Int) (undefined :: List ((forall d. d -> d) -> (Int -> Int)))" {0,1} []
-- # FAIL

-- # 70 "(undefined :: some a. List (a -> (forall b. b -> b)) -> Int) (undefined :: some c. List ((forall d. d -> d) -> c))" {0,1} []
-- # FAIL

-- & GHC choke test

-- # 71 "choose id auto" {0,1} []
-- # OK

-- # 72 "choose auto id" {0,1} []
-- # OK

-- # 73 "choose xauto xauto" {0,1} []
-- # OK

-- # 74 "choose id xauto" {0,1} []
-- # FAIL

-- # 75 "choose xauto id" {0,1} []
-- # FAIL

-- & These fail in ghc :)

-- # 76 "choose Nil ids" {0,1} []
-- # OK

-- # 77 "choose xauto ids" {0,1} []
-- # OK

-- & Escaping Skolems Test

-- # 78 "\x -> auto x" {0,1} []
-- # FAIL

-- # 79 "let poly (xs :: List (forall a. a -> a)) = 1 in \x -> poly x" {0,1} []
-- # FAIL

-- # 80 "\x -> (x :: List (forall a. a -> a))" {0,1} []
-- # FAIL

-- # 81 "\x -> let polys (xs :: List (forall a. a -> a)) = 1; f y = x in polys (Cons (f::some a. forall b. b -> a) Nil)" {0,1} []
-- # FAIL

-- # 82 "ids :: forall b. List (forall a. a -> b)" {0,1} []
-- # FAIL

-- & co/contra Variance Test

-- # 83 "let g (x::(forall a. a -> a) -> Int) = x id; f (x :: Int -> Int) = x 1 in g f" {0,1} []
-- # FAIL

-- # 84 "let g (x::(forall a. a -> a) -> Int) = x id; f (x :: Int -> Int) = x 1 in g (\\(x :: forall a. a -> a) -> f x)" {0,1} []
-- # OK

-- & Shared Polymorphism Test

-- # 85 "let f (x :: List (forall a.a -> a)) = x in let g (x :: List (Int -> Int)) = x in let ids = Cons id Nil in (f ids, g ids)" {0,1} []
-- # OK

-- & Rigid annotations Test
 
-- # 86 "single (id :: forall a. a -> a)" {0,1} []
-- # OK

-- # 87 "(id :: forall a. a -> a) 1" {0,1} []
-- # OK

-- # 88 "(id :: some a. a -> a) 1" {0,1} []
-- # OK

-- # 89 "\x -> ((\y -> x) :: some a. forall b. b -> a)" {0,1} []
-- # OK

-- # 90 "\(f :: forall a. a -> a) -> ((f f) :: forall a. a -> a)" {0,1} []
-- # OK

-- # 91 "revapp (id :: forall a. a -> a) auto" {0,1} []
-- # OK

-- # 92 "choose inc id" {0,1} []
-- # OK

-- # 93 "choose inc (id :: forall a. a -> a)" {0,1} []
-- # OK

-- # 94 "choose inc (id :: some a. a -> a)" {0,1} []
-- # OK

-- & N-ary Application Tests

-- # 95 "revapp id auto" {0,1} []
-- # OK

-- # 96 "let f = revapp id in f auto" {0,1} []
-- # OK

-- # 97 "let f = revapp (id :: forall a. a -> a) in f auto" {0,1} []
-- # OK

-- # 98 "head ids 1" {0,1} []
-- # OK

-- # 99 "auto id 1" {0,1} []
-- # OK

-- & Sharing of Polymorphic types Test

-- # 100 "let ids = single id in (map auto ids, append (single inc) ids)" {0,1} []
-- # OK

-- # 101 "single id" {0,1} []
-- # OK

-- # 102 "choose id" {0,1} []
-- # OK

-- # 103 "choose id inc" {0,1} []
-- # OK

-- # 104 "choose id auto" {0,1} []
-- # OK

-- # 105 "\x y -> x" {0,1} []
-- #  OK

-- & Type Propagation Tests

-- # 106 "single id :: List (forall a. a -> a)" {0,1} []
-- # OK

-- # 107 "returnST 1 :: forall s. ST s Int" {0,1} []
-- # OK

-- # 108 "auto id :: Int -> Int" {0,1} []
-- # OK

-- # 109 "head ids 1 :: Int" {0,1} []
-- # OK

-- # 110 "head ids :: Int -> Int" {0,1} []
-- # OK

-- & Eta Poly Tests

-- # 111 "\x -> auto x" {0,1} []
-- # OK

-- # 112 "\x -> (auto x, x 1)" {0,1} []
-- # FAIL

-- # 113 "\x -> (auto x, (x :: forall a. a -> a) 1)" {0,1} []
-- # OK 

-- # 114 "\x -> (auto x, (x :: Int -> Int) 1)" {0,1} []
-- # FAIL

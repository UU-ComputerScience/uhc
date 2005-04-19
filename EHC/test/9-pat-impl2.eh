let  class Eq a where
       eq :: a -> a -> Int
     class Eq a => Ord a where
       lt :: a -> a -> Int
     c :: Int -> Int -> Int
in
let  f = \(! d <: Ord a !) (x::a) y -> c (eq x y) (lt x y)
in   5

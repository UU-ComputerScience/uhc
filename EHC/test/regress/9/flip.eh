let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
     foreign import jazy "primMulInt" primMulInt :: Int -> Int -> Int
     class P a where
       p :: a -> a
     class Q a where
       q :: a -> a
     instance P Int where
       p = \x -> primMulInt x 10
     instance Q Int where
       q = \x -> primMulInt x 100
in
let  f :: P a => a -> Q b => b -> (a,b)
     f = \x y -> (p x,q y)
     flip = \f a b -> f b a
in
let  g1 = \a b -> flip f a b
     g2 = flip f
in
let  (a1,b1) = g1 5 6
     (a2,b2) = g2 500 600
     v = primAddInt (primAddInt a1 b1) (primAddInt a2 b2)
in   v

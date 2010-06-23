let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
in
let  class A a where
       aa :: a -> a
in
let  f :: (x :: A a => a -> a) -> a
     r = (x = \y -> y)
in
let  v1 = f r
     f2 :: r\x => (r|x::a) -> b
     f2 = \r -> f (r|x := \x -> x)
in   v1


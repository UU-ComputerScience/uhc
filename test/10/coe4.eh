let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
in
let  f :: (r\y) => (r|y::Int) -> Int
     f = \r -> r#y
     g :: (r\x,r\y,r\z) => (r|x::Int,y::Int,z::Int) -> Int
     g = \r -> f r
     v1 = g (a=3,x=4,y=5,z=6,xx=44)
in   v1


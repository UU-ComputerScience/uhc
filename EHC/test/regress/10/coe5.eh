let  foreign import jazy "primAddInt" primAddInt :: Int -> Int -> Int
in
let  g :: (r\x,r\y,r\z) => (r|x::Int,y::Int,z::Int) -> (x::Int,z::Int)
     g = \r -> r
     v1 = g (a=3,x=4,y=5,z=6,xx=44)
in   v1


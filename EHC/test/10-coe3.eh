let  f :: (x::Int) -> Int
     f = \r -> r.x
     g :: (r\x) => (r|x::Int) -> Int
     g = \r -> f r
     v1 = g (a=3,x=4)
in   v1


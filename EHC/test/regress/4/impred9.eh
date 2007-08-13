let  g :: (exists a . (a,a->Int)) -> Int
     id :: a -> a
in
let  v1 = (3,id)
     v2 = g v1
in   v1

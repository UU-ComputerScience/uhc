let  id   ::  forall a . a -> a
     xy   ::  exists a . (a, a->Int)
     xy   =   (3,id)
     ixy  ::  (exists a . (a, a->Int)) -> Int
     ixy  =   \(v,f) -> f v
     xy'  =   ixy xy
     pq   ::  exists a . (a, a->Int)
     pq   =   ('x',id)							-- ERROR
in   xy'

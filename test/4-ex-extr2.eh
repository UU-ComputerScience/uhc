let  id   ::  a -> a
     ixy  =   \(v,f) -> f v
     pq   =   ('x',id)
     xy   ::  (a,a->Int)
     xy   =   (3,id)
in   let  xy'  =   ixy xy
          pq'  =   ixy pq
     in   xy'

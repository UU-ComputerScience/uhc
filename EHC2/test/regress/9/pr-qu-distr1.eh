let  data L a = N | C a (L a)
     data Bool = False | True
in
let  class A a where
       aa :: a -> Bool
     instance A Int where
       aa = \x -> True
     instance A a => A (L a) where
       aa = \x -> True
in
let  f = \x -> C (aa x) (C (aa (C x N)) N)
in
let  v = f 3
in   v

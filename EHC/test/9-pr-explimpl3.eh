let  data L a = N | C a (L a)
in
let  class A a where
       aa :: a -> a
     instance dAL1 <~ A a => A (L a) where
       aa = \x -> x
in
let  v = aa (# A (L Int) ~> dAL1 (aa = \x -> x) #) (C 3 N)
in   v


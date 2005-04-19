let  class A a where
       aa :: a -> a
     dAInt2 = (aa = \x -> x)
in
let  v = aa (! dAInt2 <: A Int !) 3
in   v


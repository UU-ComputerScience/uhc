let  class A a where
       aa :: a -> a
     dAInt2 = (aa = \x -> x)
in
let  v = aa (# A Int :> dAInt2 #) 3
in   v


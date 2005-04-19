let  class A a where
       aa :: a -> a
     dAInt3 :: (aa :: Char -> Char)
in
let  v = aa (! dAInt3 <: A Int !) 3
in   v


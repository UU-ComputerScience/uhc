let  class A a where
       aa :: a -> a
     dAInt3 :: (aa :: Char -> Char)
in
let  v = aa (# A Int :> dAInt3 #) 3
in   v


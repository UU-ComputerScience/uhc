let  class A a where
       aa :: a -> Int
     class A a => B a where
       bb :: a -> Int
in
let  f5 = \x -> (aa x,bb x)
in   3

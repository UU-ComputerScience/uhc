let  class A a where
       aa :: a -> a
     class A a => B a where
       bb :: a -> a
     class B a => C a where
       cc :: a -> a
     c :: forall a . forall b . forall c . a -> b -> c
in
let  f  =   \x y z -> c (c (aa x) (bb x)) (c (aa y) (c (aa z) (cc z)))
in   f

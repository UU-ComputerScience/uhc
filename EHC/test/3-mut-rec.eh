let  f1 = \x -> g1 x
     g1 = \y -> f1 y
     f2 :: a -> a
     f2 = \x -> g2 x
     g2 = \y -> f2 y
in   3

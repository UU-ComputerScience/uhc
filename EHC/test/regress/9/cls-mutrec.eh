let class A a => B a where
      bb :: a -> a
    class B a => A a where
      aa :: a -> a
    instance A Int where
      aa = \x -> x
    instance B Int where
      bb = \x -> x
in
let
    main = aa 3
in  main


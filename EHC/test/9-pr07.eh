let  class Ct a b where
       cast :: a -> b
     instance (Ct a1 a2, Ct a2 a3) => Ct a1 a3 where
       cast = \a1 -> cast (cast a1)
in   3
{-
     instance Ct a a where
       cast = \x -> x
     instance (Ct b1 a1, Ct a2 b2) => Ct (a1 -> a2) (b1 -> b2) where
       cast = \f x -> cast (f (cast x))
-}

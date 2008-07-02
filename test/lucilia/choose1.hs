{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity, various choose examples
   expected: types of cons1 & cons3 the same, cons2 & cons4 too
   but     : cons2 & cons4 are not the same, cons2 should have cons4's type.
             This is an instance of the inference order problem
---------------------------------------------------------------------------------------- -}

module Main where

choose :: a -> a -> a
choose a b = a

chid1 :: (forall a . a -> a) -> forall b . b -> b
chid1 = choose id
chid2 :: forall a . (a -> a) -> a -> a
chid2 = choose id

main = return ()

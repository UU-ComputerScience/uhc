{- ----------------------------------------------------------------------------------------
   what    : kind signatures
   expected: ok
---------------------------------------------------------------------------------------- -}

module KindSig1 where

data Eq1 a b = Eq1 (forall f . f a -> f b)

Eq2 :: * -> * -> *
data Eq2 a b = Eq2 (forall f . f a -> f b)

main :: IO ()
main = return ()

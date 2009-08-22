{- ----------------------------------------------------------------------------------------
   what    : partial type signature
   expected: ok
---------------------------------------------------------------------------------------- -}

module PartialTypeSig1 where

f1 :: (forall a . a->a) -> ... 
f1 i = (i 'a', i True)

f2 :: (forall a . a->a) -> %b
f2 i = (i 'a', i True)

main :: IO ()
main = return ()

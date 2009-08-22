{- ----------------------------------------------------------------------------------------
   what    : partial type signature
   expected: error
---------------------------------------------------------------------------------------- -}

module PartialTypeSig2 where

f3 :: (forall a . a->a) -> (%b,%b)
f3 i = (i 'a', i True)

main :: IO ()
main = return ()

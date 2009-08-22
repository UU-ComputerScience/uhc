{- ----------------------------------------------------------------------------------------
   what    : lazy instanstiation of exist
   expected: ok
---------------------------------------------------------------------------------------- -}

module TestLazyExist where

f :: forall a . (a -> exists b . (b, a, b -> b -> Int))
f i = (3, i, (+))

main = print (let  (b, a, g) = f b
              in   g b a)

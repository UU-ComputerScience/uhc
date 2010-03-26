{- ----------------------------------------------------------------------------------------
   what    : bugfix: unqualified operator with '.' inside: was interpreted as qualified
   expected: ok
---------------------------------------------------------------------------------------- -}

module NameOperator1 where 

(<.>) :: Int -> Int -> Int
(<.>) = (+)

{- This did not not compile -}
f :: Int
f = 1 <.> 1

(<+>) :: Int -> Int -> Int
(<+>) = (+)

{- This compiles -}
g :: Int
g = 1 <+> 1

main = return ()    

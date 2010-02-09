{- ----------------------------------------------------------------------------------------
   what    : existential type
   expected: error for mixy
---------------------------------------------------------------------------------------- -}

module ExistentialType2 where

x2 :: exists a . (a, a -> Int)
x2 = (3 :: Int, id)

mkx :: Bool -> exists a . (a, a -> Int)
mkx b = if b then x2 else ('a',ord)

y1 = mkx True
y2 = mkx False

mixy = let (v1,f1) = y1
           (v2,f2) = y2
       in  f1 v2

main :: IO ()
main
  = return ()

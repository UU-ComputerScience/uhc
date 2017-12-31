
t6 :: Int -> Int
t6 x = fo (+) 0 [1 .. x]

fo :: (a -> b -> a) -> a -> [b] -> a
fo = \f z0 xs0 -> let
  lgo = \z ys -> case ys of
    [] -> z
    (x:xs) -> let z' = f z x in lgo z' xs
  in lgo z0 xs0

main = print (sum (replicate 100 (t6 1000000)))
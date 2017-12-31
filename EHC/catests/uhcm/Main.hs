{-# LANGUAGE BangPatterns #-}
module M 
() where

-- main = let !x = fo const True [] in undefined
-- main = app3 undefined undefined
-- import Test hiding (fo, t6)
-- main = app3 undefined undefined undefined

-- main = f undefined
-- f x = x

main :: IO ()
main = do
--   -- print (maxBound :: Int)
--   -- -- print $ t6 100000000
--   -- print $ t5 100000000
--   -- -- print $ t6 10000000
--   -- print $ t5 10000000
  print $ t6 1000000
--   -- print $ t5 1000000
--   -- print $ t6 100000
--   -- print $ t5 100000
--   -- print $ t6 10000
--   -- print $ t5 10000
--   -- print $ t6 1000
--   -- print $ t5 1000
--   -- print $ t6 100
--   -- print $ t5 100
--   -- print $ t6 10
--   -- print $ t5 10
--   -- print $ t6 1
--   -- print $ t5 1


fo :: (a -> b -> a) -> a -> [b] -> a
fo = \f z0 xs0 -> let
  lgo = \z ys -> case ys of
    [] -> z
    (x:xs) -> let z' = f z x in lgo z' xs
  in lgo z0 xs0

-- app3 x y f = f x y

-- fo = \f z ys -> case ys of
--   [] -> z
--   (x:xs) -> let z' = f z x in fo f z' xs

-- fo = \f z ys -> case ys of
--   [] -> z
--   (_:xs) -> fo f z xs

-- fo f = 
--   let lgo = \z ys -> case ys of
--               [] -> z
--               (_:xs) -> let z' = f z in lgo z' xs
--   in lgo

-- test f x = let y = f x in x

-- t2 f = let !g = f in test g
t6 :: Int -> Int
t6 x = fo (+) 0 [1 .. x]

-- t6 :: Int -> Int
-- t6 x = fo plus 0 [1 .. x]

-- plus :: Int -> Int -> Int
-- plus n m = let !n' = n in let !m'= m in n' + m'

-- t5 :: Int -> Int
-- t5 x = (x * x + x) `div` 2

-- t6 :: Int -> Bool
-- t6 x = fo andTest True $ replicate x True

-- main = print $ andTest True True

-- andTest :: Bool -> Bool -> Bool
-- andTest True True = True
-- andTest _ _ = False
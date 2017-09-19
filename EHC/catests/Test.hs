module Test where

t1 :: Int
t1 = let y = 3 in 5

-- co :: a -> b -> a
-- co x y = x

-- fa :: a -> b
-- fa x = undefined

-- data C a = C
--   { fc :: a -> a -> a
--   , gc :: a -> a -> a
--   }

-- func :: C a -> a -> a -> a -> a
-- func d x y z = f' (f' x y) z
--   where f' = fc d

-- data B = T | F

-- bNot T = F
-- bNot F = T

-- bAnd T T = T
-- bAnd _ _ = F

-- fac :: Int -> Int 
-- fac n = if n <= 1 then 1 else n * fac (n - 1)

-- t2 :: a -> Int
-- t2 = let x = 1 + 2 in \y -> x + 5

-- t3 :: a -> Int
-- t3 = \y -> let x = 1 + 2 in x + 5

-- t4 :: Int
-- t4 = let x = 1 + 2 in let y = (\z -> z) x in y + y

-- le :: [a] -> Int
-- le [] = 0
-- le (x:xs) = 1 + length xs

-- addLength :: [Int] -> [Int]
-- addLength xs = map (+ (length xs)) xs

-- t5 :: Bool -> (a -> a) -> a -> a
-- t5 b g x = if b then x else g (g x)

-- fo :: (a -> b -> a) -> a -> [b] -> a
-- fo = \f z0 xs0 -> let
--   lgo = \z ys -> case ys of
--     [] -> z
--     (x:xs) -> let z' = f z x in lgo z' xs
--   in lgo z0 xs0

-- t6 :: Int
-- t6 = fo (+) 0 [1 .. 10000000]
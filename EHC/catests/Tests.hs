{-# LANGUAGE BangPatterns #-}
module Main where

  -- main = putStr "Hello World"
-- main = let !x = fo const True [] in undefined
-- main = app3 undefined undefined
-- import Test hiding (fo, t6)
-- main = app3 undefined undefined undefined

-- main = f undefined
-- f x = x

-- main :: IO ()
-- main = do
--   print (maxBound :: Int)
--   -- -- -- print $ t6 100000000
--   -- print $ t5 100000000
--   -- -- print $ t6 10000000
--   -- print $ t5 10000000
--   -- print (maxBound :: Int)
--   -- print $ t6 1000000
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
--   -- print $ app3 True False (&&)
--   -- print $ fo1 (&&) True $ replicate 10000 True
--   -- print $ fo2 (&&) True $ replicate 100000 True
--   -- print $ fo3 not True $ replicate 1000000 True
--   -- print $ test fac $ fact 20
--   -- print $ t2 fac $ fact 20
--   -- print $ t6 1000000
--   -- print $ t62 1000000
--   -- print $ plus t1 $ fact 20
--   -- print $ t61 1000000
--   -- print $ andTest t0 True
--   -- print $ testMap not tmap
--   -- print $ u6 tFail
--   -- print $ tmapbase tfailbase
--   print $ testBinTreeNoInternals
--   print $ length $ flatten $ mirror $ mapTreeUnitToFac facn $ genBalancedBinTree binn
--   print $ testBinTreeInternals

-- fo :: (a -> b -> a) -> a -> [b] -> a
-- fo = \f z0 xs0 -> let
--   lgo = \z ys -> case ys of
--     [] -> z
--     (x:xs) -> let z' = f z x in lgo z' xs
--   in lgo z0 xs0

-- app3 :: a -> b -> (a -> b -> c) -> c
-- app3 x y f = f x y

-- fo1 :: (a -> b -> a) -> a -> [b] -> a
-- fo1 = \f z ys -> case ys of
--   [] -> z
--   (x:xs) -> let z' = f z x in fo f z' xs

-- fo2 :: (a -> b -> a) -> a -> [b] -> a
-- fo2 = \f z ys -> case ys of
--   [] -> z
--   (_:xs) -> fo f z xs

-- fo3 :: (a -> a) -> a -> [b] -> a
-- fo3 f = 
--   let lgo = \z ys -> case ys of
--               [] -> z
--               (_:xs) -> let z' = f z in lgo z' xs
--   in lgo

-- test :: (a -> b) -> a -> a
-- test f x = let _ = f x in x

-- t2 :: (a -> b) -> a -> a
-- t2 f = let !g = f in test g
-- t6 :: Int -> Int
-- t6 x = fo (+) 0 [1 .. x]

-- t62 :: Int -> Int
-- t62 x = fo plus 0 [1 .. x]

-- plus :: Int -> Int -> Int
-- plus n m = let !n' = n in let !m'= m in n' + m'

-- t5 :: Int -> Int
-- t5 x = (x * x + x) `div` 2

-- t61 :: Int -> Bool
-- t61 x = fo andTest True $ replicate x True

-- -- main = print $ andTest True True

-- andTest :: Bool -> Bool -> Bool
-- andTest True True = True
-- andTest _ _ = False

-- t0 :: Bool
-- t0 = True

-- t1 :: Int
-- t1 = let _ = 3 :: Int in 5

-- tmap :: [Bool]
-- tmap = map not (True:[])

-- testMap :: (a -> b) -> [a] -> [b]
-- testMap _ [] = []
-- testMap f (x:xs) = f x : testMap f xs

-- tFail :: [Bool]
-- tFail = map not $ []

-- tfailbase :: [Bool]
-- tfailbase = let u61 = map not in ($) u61 []

-- u6 :: [Bool] -> [Bool]
-- u6 = map not
-- tFail2 :: [Bool]
-- tFail2 = ($) u6 []

-- tfailOpt :: [Bool]
-- tfailOpt = let !u63 = map not in let !_ = [] in 
--   let !_ = u63 in ($) u63 []

-- tmapbase :: [Bool] -> [Bool]
-- tmapbase = \xs -> let u64 = True : xs in
--   let u5 = map not in u5 $ u64

-- tmapopt :: [Bool]
-- tmapopt = let u65 = True : [False] in
--   let u5 = map not in 
--     let !u3 = u65 in 
--       let !u4 = u5 
--       in u4 $ u3

-- x1 :: Bool
-- x1 = if True || False then False else True

-- f1 :: (a -> a -> Bool) -> a -> Bool -> Bool
-- f1 c y z = if c y y then not x1 && True else not z || False

-- co :: a -> b -> a
-- co x _ = x

-- fa :: a -> b
-- fa _ = undefined

-- -- works
-- class E a where
--   fce :: a
--   -- gc :: a -> a -> a
  

-- funca :: E a => a
-- funca = fce


-- class D a where
--   fcd :: a -> a
--   -- gc :: a -> a -> a
  

-- func :: D a => a -> a
-- func x = f x
--   where f = fcd

-- data C a = C
--   { fc :: a -> a -> a
--   , gc :: a -> a -> a
--   }

-- funcd :: C a -> a -> a -> a -> a
-- funcd d x y z = f' (f' x y) z
--   where f' = fc d


-- data B = T | F

-- bNot :: B -> B
-- bNot T = F
-- bNot F = T

-- bAnd :: B -> B -> B
-- bAnd T T = T
-- bAnd _ _ = F

-- fact :: Int -> Int 
-- fact n = if n <= 1 then 1 else n * fact (n - 1)

-- fibt :: Int -> Int
-- fibt x | x < 1 = 0
-- fibt 1 = 1
-- fibt n = fibt (n-1) + fibt (n-2)

-- t21 :: a -> Int
-- t21 = let x = 1 + 2 in \_ -> x + 5

-- t3 :: a -> Int
-- t3 = \_ -> let x = 1 + 2 in x + 5

-- t4 :: Int
-- t4 = let x = 1 + 2 in let y = (\z -> z) x in y + y

-- le :: [a] -> Int
-- le [] = 0
-- le (_:xs) = 1 + length xs

-- addLength :: [Int] -> [Int]
-- addLength xs = map (+ (length xs)) xs

-- t52 :: Bool -> (a -> a) -> a -> a
-- t52 b g x = if b then x else g (g x)

-- fo4 :: (a -> b -> a) -> a -> [b] -> a
-- fo4 = \f z0 xs0 -> let
--   lgo = \z ys -> case ys of
--     [] -> z
--     (x:xs) -> let z' = f z x in lgo z' xs
--   in lgo z0 xs0

-- t65 :: Int
-- t65 = fo (+) 0 [1 .. 10000000]


-- voorbeelden
-- bintree, mirror, sum, countleafs

-- facn, binn :: Int
-- facn = 12
-- binn = 20

-- testBinTreeNoInternalsViaList :: Int
-- testBinTreeNoInternalsViaList = length $ flatten $ mirror $ mapTreeUnitToFac facn $ genBalancedBinTree binn

-- testBinTreeNoInternals :: Int
-- testBinTreeNoInternals = countLeafs $ mirror $ mapTreeUnitToFac facn $ genBalancedBinTree binn

-- testBinTreeInternals :: Int
-- testBinTreeInternals = sumTree $ mirror $ mapTreeUnitToFac facn $ genBalancedBinTree binn

-- testBinTree :: Int
-- testBinTree = countLeafs testTree + length (flatten testTree) + sumTree testTree

-- testTree :: BinTree Int
-- testTree = mirror $ mapTreeUnitToFac facn $ genBalancedBinTree binn

-- data BinTree a = Leaf a | BinTree (BinTree a) a (BinTree a)
--   deriving (Show)

-- -- mapTree :: (a -> b) -> BinTree a -> BinTree b
-- -- mapTree f (Leaf a) = Leaf $ f a
-- -- mapTree f (BinTree l a r) = BinTree (mapTree f l) (f a) $ mapTree f r

-- mapTreeUnitToFac :: Int -> BinTree () -> BinTree Int
-- mapTreeUnitToFac n (Leaf _) = Leaf $ fact n
-- mapTreeUnitToFac n (BinTree l _ r) = BinTree l' (fact n) r'
--   where l' = mapTreeUnitToFac n l
--         r' = mapTreeUnitToFac n r

-- genBalancedBinTree :: Int -> BinTree ()
-- genBalancedBinTree n | n < 1 = Leaf ()
-- genBalancedBinTree n = BinTree sub () sub
--     where sub = genBalancedBinTree $ n - 1

-- mirror :: BinTree a -> BinTree a
-- mirror (BinTree l a r) = BinTree r a l
-- mirror x = x

-- sumTree :: BinTree Int -> Int
-- sumTree (Leaf x) = x
-- sumTree (BinTree l x r) = sumTree l + x + sumTree r

-- countLeafs :: BinTree a -> Int
-- countLeafs (Leaf _) = 1
-- countLeafs (BinTree l _ r) = countLeafs l + 1 + countLeafs r

-- flatten :: BinTree a -> [a]
-- flatten (BinTree l a r) = flatten l ++ a:flatten r
-- flatten (Leaf a) = [a]

-- ff :: a -> a
-- ff xs = xs
-- g :: [a] -> Int
-- g xs = length (ff xs)
-- h :: [Int] -> Int
-- h xs = sum (ff xs)

-- -- type Matrix a = [[a]]

-- -- flattenM :: Matrix a -> [a]
-- -- flattenM = concat
-- t :: Int
-- t = 1
-- s :: Int
-- s = 2
-- main = print $ g ([t] :: [Int]) + h [s]


--
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- by Don Stewart, Einar Karttunen, Branimir Maksimovic and Bertram Felgenhauer
--

import System.Environment

data F = F !Integer !Integer !Integer !Integer

main :: IO ()
main = loop 10 0 . flip take (str (F 1 0 0 1) ns) . read . head =<< getArgs

ns = [ F k (4*k+2) 0 (2*k+1) | k <- [1..] ]

loop :: Int -> Integer -> [Integer] -> IO ()
loop n s []     = putStrLn $ replicate n ' ' ++ "\t:" ++ show s
loop 0 s xs     = putStrLn ("\t:"++show s) >> loop 10 s xs
loop n s (x:xs) = putStr (show x)          >> loop (n-1) (s+1) xs

flr  x           (F q r s t) = (q*x + r) `div` (s*x + t)
comp1 (F q r s t) (F u v w x) = F (q*u+r*w) (q*v+r*x) (t*w) (t*x)
comp2 (F q r s t) (F u v w x) = F (q*u) (q*v+r*x) (s*u) (s*v+t*x)

str z (x:xs) | y == flr 4 z = y : str (comp1 (F 10 (-10*y) 0 1) z) (x:xs)
             | otherwise    =     str (comp2 z x) xs     where y = flr 3 z

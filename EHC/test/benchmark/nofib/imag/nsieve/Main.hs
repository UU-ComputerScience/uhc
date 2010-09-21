--
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- Nsieve over a Bool array
--

import Data.Array.IO
import Data.Array.Base
import System.Environment
import Text.Printf

main = do
    n <- getArgs >>= readIO . head :: IO Int
    mapM_ (sieve . (10000 *) . (2 ^)) [n, n-1, n-2]

sieve n = do
    a <- newArray (2,n) True :: IO (IOUArray Int Bool) -- an array of Bool
    r <- go a n 2 0
    printf "Primes up to %8d %8d\n" (n::Int) (r::Int) :: IO ()

go a m n c | a `seq` m `seq` n `seq` c `seq` False = undefined
go a m n c
    | n == m    = return c
    | otherwise = do
            e <- unsafeRead a n
            if e
                then let loop j | j `seq` False = undefined
                            | j <= m    = unsafeWrite a j False >> loop (j+n)
                            | otherwise = go a m (n+1) (c+1)
                     in loop (n+n)
                else go a m (n+1) c

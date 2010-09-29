{-# OPTIONS_GHC -fexcess-precision #-}
--
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Translation of the Clean by Don Stewart
--

import System.Environment
import Numeric
import Control.Monad

main = do
    n <- fmap (read . head) getArgs
    mapM_ draw $ go (n::Double) (1::Int) 1 0 0 0 0 0 0 0 0 0

draw (s,t) = putStrLn $ (showFFloat (Just 9) (s::Double) []) ++ "\t" ++ t

go n i alt a1 a2 a3 a4 a5 a6 a7 a8 a9

    | n `seq`i `seq`alt `seq`a1 `seq`a2 `seq`a3 `seq`a4 `seq`a5 `seq`a6
                `seq`a7 `seq`a8 `seq`a9 `seq` False = undefined

    | k <= n    = go n (i+1) (-alt)
                     (a1 + (2/3) ** (k-1))
                     (a2 + sqrt dk)
                     (a3 + 1 / (k * (k + 1)))
                     (a4 + 1 / (k3 * sk * sk))
                     (a5 + 1 / (k3 * ck * ck))
                     (a6 + dk)
                     (a7 + dk * dk)
                     (a8 + alt * dk)
                     (a9 + alt / (2 * k - 1))

    | otherwise = [(a1, "(2/3)^k"),     (a2, "k^-0.5"),        (a3, "1/k(k+1)")
                  ,(a4, "Flint Hills"), (a5, "Cookson Hills"), (a6, "Harmonic")
                  ,(a7, "Riemann Zeta"),(a8, "Alternating Harmonic"), (a9, "Gregory")]

    where k  = fromIntegral i
          k3 = k2*k; k2 = k*k; dk = 1/k; sk = sin k; ck = cos k

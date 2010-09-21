{-# OPTIONS_GHC -fexcess-precision #-}

import System.Environment

main = do
    n <- fmap (read.head) getArgs

    let go :: Double -> Double -> Int -> IO ()
        go x y i
            | x `seq` y `seq` i `seq` False = undefined -- no bang patterns
            | i == n    = print . take 5 . show $ x+y
            | otherwise = go (x*y/3) (x*9) (i+1)

    go (1/3) 3 1


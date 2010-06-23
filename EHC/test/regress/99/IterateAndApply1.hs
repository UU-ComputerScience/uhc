{- ----------------------------------------------------------------------------------------
   what    : from fromEnum impl,
             giving exponential behavior because 'x' was evaluated twice without sharing
             its result.
             Caused by == yielding an eval of an apply ('x', being head of recursive iterate call)
             withing an apply (primEqInt)
   expected: ok, within limited time
---------------------------------------------------------------------------------------- -}

module Main where

boundedSucc2 :: Int -> Int
boundedSucc2 x
  | x == 3333333 = error "succ: applied to maxBound"
  | otherwise     = x+1

main = putStrLn (concatMap show $ take 50 $ iterate boundedSucc2 (1::Int))



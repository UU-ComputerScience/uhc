{- ----------------------------------------------------------------------------------------
   what    : Testing CPUTime
   expected: ok
   platform: run time depends on platform   
---------------------------------------------------------------------------------------- -}

module Main where

import System.CPUTime

main :: IO ()

main = do
  print cpuTimePrecision
  time <- getCPUTime
  print time
  print $ f ([1..1000] :: [Int])
  time' <- getCPUTime
  print time'
  print (time' >= time)
  
f :: [Int] -> Int
f xs = length xs + head xs

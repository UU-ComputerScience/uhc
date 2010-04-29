{- ----------------------------------------------------------------------------------------
   what    : library System.Random
   expected: ok
   platform: random generators
---------------------------------------------------------------------------------------- -}

module Main where

import System.Random

main :: IO ()
main = do
  rin <- (randomIO :: IO Integer)
  print rin
  let sg  = mkStdGen 1
      chs = randoms sg :: [Char]
      (g1,g2) = split sg
      n       = next g1
      range   = genRange g2
  print (take 10 chs)
  print g1
  print n
  print range

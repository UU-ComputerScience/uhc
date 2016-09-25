{- ----------------------------------------------------------------------------------------
   what    : Floating functions, on Float
   expected: print with as much precision as possible (used to be 5 after comma)
---------------------------------------------------------------------------------------- -}

{-

With GHC (8.0.1): 1.4142135623730951
with UHC: 1.414214

Is there any reason for the difference?

Blocking agda/agda#1856.

-}

module Main where

main = do
  print $ sqrt (2.0 :: Float)
  print $ sqrt (2.0 :: Double)
  print $ sqrt (20000000.0 :: Float)
  print $ sqrt (20000000.0 :: Double)
  print $ sqrt (0.00000002 :: Float)
  print $ sqrt (0.00000002 :: Double)
  print $ sqrt (20000000000000000000.0 :: Float)
  print $ sqrt (20000000000000000000.0 :: Double)
  print $ sqrt (0.00000000000000000002 :: Float)
  print $ sqrt (0.00000000000000000002 :: Double)
  print $      (2.0 :: Float)
  print $      (2.0 :: Double)
  print $      (20000000.0 :: Float)
  print $      (20000000.0 :: Double)
  print $      (0.00000002 :: Float)
  print $      (0.00000002 :: Double)
  print $      (20000000000000000000.0 :: Float)
  print $      (20000000000000000000.0 :: Double)
  print $      (0.00000000000000000002 :: Float)
  print $      (0.00000000000000000002 :: Double)

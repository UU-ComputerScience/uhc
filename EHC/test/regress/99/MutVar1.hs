{- ----------------------------------------------------------------------------------------
   what    : MutVar testing
   expected: ok
---------------------------------------------------------------------------------------- -}

module MutVar1 where

import UHC.MVar

main = do
  m1 <- newMVar "m1"
  m2 <- newMVar "m2"
  print (sameMVar m1 m1)
  print (sameMVar m1 m2)
  mbt1 <- tryTakeMVar m1
  print mbt1
  mbt1 <- tryTakeMVar m1
  print mbt1
  mbp1 <- tryPutMVar m1 "m1"
  print mbp1
  mbp1 <- tryPutMVar m1 "m1"
  print mbp1


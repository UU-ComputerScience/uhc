{- ----------------------------------------------------------------------------------------
   what    : library System.Random
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Random

intRange :: (Int, Int)
intRange = (-100,100)

boolRange :: (Bool, Bool)
boolRange = (False,True)

floatRange :: (Float,Float)
floatRange  = (-10,10)

doubleRange :: (Double, Double)
doubleRange = (1,20)

diceRange :: (Int, Int)
diceRange = (1,6)

main :: IO ()
main = do
  r' <- rollDice
  r  <- rollDice 
  print (inRange diceRange r && inRange diceRange r')

  g <- newStdGen
  setStdGen g
  g' <- getStdGen
  print (show g == show g')

--  print (show g == show (read (show g)))
  
  rd  <- randomRIO doubleRange
  print (inRange doubleRange rd)
  
  let sg  = mkStdGen 1
      sg' = read "123" :: StdGen
      ri  = fst $ randomR intRange sg
      rb  = fst $ randomR boolRange sg'
      rfs = randomRs floatRange g
  print (inRange intRange ri)
  print (inRange boolRange rb)
  print (all (inRange floatRange) (take 10 rfs))
  

rollDice :: IO Int
rollDice = getStdRandom (randomR diceRange)

inRange :: Ord a => (a,a) -> a -> Bool
inRange (lo,hi) x = lo <= x && x <= hi

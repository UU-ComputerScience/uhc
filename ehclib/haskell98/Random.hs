module Random (
   RandomGen(next, split, genRange),
   StdGen, mkStdGen,
   Random( random,   randomR, randoms,  randomRs, randomIO, randomRIO ),
   getStdRandom, getStdGen, setStdGen, newStdGen
  ) where

import System.Random

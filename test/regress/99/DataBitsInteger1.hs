{- ----------------------------------------------------------------------------------------
   what    : library Data.Bits, for Integer
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataBitsInteger1 where

import Data.Bits

main :: IO ()
main
  = do -- putStrLn (show (bitSize (2 :: Integer)))
       putStrLn (show (2 .&. 1 :: Integer))
       putStrLn (show (2 .|. 1 :: Integer))
       putStrLn (show ((2 :: Integer) `shiftL` (1 :: Int)))
       putStrLn (show ((2 :: Integer) `shiftR` (1 :: Int)))
       putStrLn (show ((-2 :: Integer) `shiftL` (1 :: Int)))
       putStrLn (show ((-2 :: Integer) `shiftR` (1 :: Int)))
       putStrLn (show (bit 1 :: Integer))
       putStrLn (show (complement 2 :: Integer))
       putStrLn (show ((2 :: Integer) `testBit` (1 :: Int)))
       putStrLn (show ((2 :: Integer) `testBit` (2 :: Int)))
       putStrLn (show ((2 :: Integer) `complementBit` (1 :: Int)))
       putStrLn (show ((2 :: Integer) `complementBit` (2 :: Int)))
       putStrLn (show ((2 :: Integer) `rotateL` (2 :: Int)))
       putStrLn (show ((2 :: Integer) `rotateR` (2 :: Int)))

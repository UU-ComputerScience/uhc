{- ----------------------------------------------------------------------------------------
   what    : library Data.Bits, for Int
   expected: ok
   platform: word size dependent
---------------------------------------------------------------------------------------- -}

module DataBitsInt1 where

import Data.Bits

main :: IO ()
main
  = do putStrLn (show (bitSize (2 :: Int)))
       putStrLn (show (2 .&. 1 :: Int))
       putStrLn (show (2 .|. 1 :: Int))
       putStrLn (show ((2 :: Int) `shiftL` (1 :: Int)))
       putStrLn (show ((2 :: Int) `shiftR` (1 :: Int)))
       putStrLn (show ((-2 :: Int) `shiftL` (1 :: Int)))
       putStrLn (show ((-2 :: Int) `shiftR` (1 :: Int)))
       putStrLn (show (bit 1 :: Int))
       putStrLn (show (complement 2 :: Int))
       putStrLn (show ((2 :: Int) `testBit` (1 :: Int)))
       putStrLn (show ((2 :: Int) `testBit` (2 :: Int)))
       putStrLn (show ((2 :: Int) `complementBit` (1 :: Int)))
       putStrLn (show ((2 :: Int) `complementBit` (2 :: Int)))
       putStrLn (show ((2 :: Int) `rotateL` (2 :: Int)))
       putStrLn (show ((2 :: Int) `rotateR` (2 :: Int)))

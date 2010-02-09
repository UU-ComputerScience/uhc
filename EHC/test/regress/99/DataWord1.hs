{- ----------------------------------------------------------------------------------------
   what    : base classes for Data.Word: Word64, Word32, Word16, Word8, Word
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataWord1 where

import Data.Word

main :: IO ()
main
  = do putStrLn "\nWord constants"
       putStrLn (show (minBound :: Word))
       putStrLn (show (maxBound :: Word))
       putStrLn (show (7777777 :: Word))
       putStrLn (show (-7777777 :: Word))
       putStrLn "\nWord constants: overflow"
       putStrLn (show (777777777777777 :: Word))
       putStrLn (show (-777777777777777 :: Word))
       putStrLn (show (777777777777777777777777777777 :: Word))
       putStrLn (show (-777777777777777777777777777777 :: Word))
       putStrLn "\nWord arithmetic"
       putStrLn (show (1 + 1 :: Word))
       putStrLn (show (7777777 + 7777777 :: Word))
       putStrLn (show (7777777 - 7777777 :: Word))
       putStrLn (show (7777777 * 2 :: Word))
       putStrLn (show ((7777777 * 2) `quot` 2 :: Word))
       let (q,r) = (7777777 :: Word) `quotRem` (2 :: Word)
       putStrLn ("7777777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "Word64 constants"
       putStrLn (show (minBound :: Word64))
       putStrLn (show (maxBound :: Word64))
       putStrLn (show (7777777 :: Word64))
       putStrLn (show (-7777777 :: Word64))
       putStrLn (show (777777777777777 :: Word64))
       putStrLn (show (-777777777777777 :: Word64))
       putStrLn "\nWord64 constants: overflow"
       putStrLn (show (777777777777777777777777777777 :: Word64))
       putStrLn (show (-777777777777777777777777777777 :: Word64))
       putStrLn "\nWord64 arithmetic"
       putStrLn (show (1 + 1 :: Word64))
       putStrLn (show (777777777777777 + 777777777777777 :: Word64))
       putStrLn (show (777777777777777 - 777777777777777 :: Word64))
       putStrLn (show (777777777777777 * 2 :: Word64))
       putStrLn (show ((777777777777777 * 2) `quot` 2 :: Word64))
       let (q,r) = (777777777777777 :: Word64) `quotRem` (2 :: Word64)
       putStrLn ("777777777777777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nWord32 constants"
       putStrLn (show (minBound :: Word32))
       putStrLn (show (maxBound :: Word32))
       putStrLn (show (7777777 :: Word32))
       putStrLn (show (-7777777 :: Word32))
       putStrLn "\nWord32 constants: overflow"
       putStrLn (show (777777777777777 :: Word32))
       putStrLn (show (-777777777777777 :: Word32))
       putStrLn (show (777777777777777777777777777777 :: Word32))
       putStrLn (show (-777777777777777777777777777777 :: Word32))
       putStrLn "\nWord32 arithmetic"
       putStrLn (show (1 + 1 :: Word32))
       putStrLn (show (7777777 + 7777777 :: Word32))
       putStrLn (show (7777777 - 7777777 :: Word32))
       putStrLn (show (7777777 * 2 :: Word32))
       putStrLn (show ((7777777 * 2) `quot` 2 :: Word32))
       let (q,r) = (7777777 :: Word32) `quotRem` (2 :: Word32)
       putStrLn ("7777777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nWord16 constants"
       putStrLn (show (minBound :: Word16))
       putStrLn (show (maxBound :: Word16))
       putStrLn (show (7777 :: Word16))
       putStrLn (show (-7777 :: Word16))
       putStrLn "\nWord16 constants: overflow"
       putStrLn (show (777777777777777 :: Word16))
       putStrLn (show (-777777777777777 :: Word16))
       putStrLn "\nWord16 arithmetic"
       putStrLn (show (1 + 1 :: Word16))
       putStrLn (show (7777 + 7777 :: Word16))
       putStrLn (show (7777 - 7777 :: Word16))
       putStrLn (show (7777 * 2 :: Word16))
       putStrLn (show ((7777 * 2) `quot` 2 :: Word16))
       let (q,r) = (7777 :: Word16) `quotRem` (2 :: Word16)
       putStrLn ("7777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nWord8 constants"
       putStrLn (show (minBound :: Word8))
       putStrLn (show (maxBound :: Word8))
       putStrLn (show (77 :: Word8))
       putStrLn (show (-77 :: Word8))
       putStrLn "\nWord8 constants: overflow"
       putStrLn (show (7777777 :: Word8))
       putStrLn (show (-7777777 :: Word8))
       putStrLn "\nWord8 arithmetic"
       putStrLn (show (1 + 1 :: Word8))
       putStrLn (show (77 + 77 :: Word8))
       putStrLn (show (77 - 77 :: Word8))
       putStrLn (show (77 * 2 :: Word8))
       putStrLn (show ((77 * 2) `quot` 2 :: Word8))
       let (q,r) = (77 :: Word8) `quotRem` (2 :: Word8)
       putStrLn ("77 `quotRem` 2: " ++ show q ++ "," ++ show r)


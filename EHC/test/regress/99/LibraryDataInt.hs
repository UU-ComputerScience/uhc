{- ----------------------------------------------------------------------------------------
   what    : base classes for Data.Int: Int64, Int32, Int16, Int8
   expected: ok
---------------------------------------------------------------------------------------- -}

module LibraryDataInt where

import Data.Int

main :: IO ()
main
  = do putStrLn "Int64 constants"
       putStrLn (show (minBound :: Int64))
       putStrLn (show (maxBound :: Int64))
       putStrLn (show (7777777 :: Int64))
       putStrLn (show (-7777777 :: Int64))
       putStrLn (show (777777777777777 :: Int64))
       putStrLn (show (-777777777777777 :: Int64))
       putStrLn "\nInt64 constants: overflow"
       putStrLn (show (777777777777777777777777777777 :: Int64))
       putStrLn (show (-777777777777777777777777777777 :: Int64))
       putStrLn "\nInt64 arithmetic"
       putStrLn (show (1 + 1 :: Int64))
       putStrLn (show (777777777777777 + 777777777777777 :: Int64))
       putStrLn (show (777777777777777 - 777777777777777 :: Int64))
       putStrLn (show (777777777777777 * 2 :: Int64))
       putStrLn (show ((777777777777777 * 2) `quot` 2 :: Int64))
       let (q,r) = (777777777777777 :: Int64) `quotRem` (2 :: Int64)
       putStrLn ("777777777777777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nInt32 constants"
       putStrLn (show (minBound :: Int32))
       putStrLn (show (maxBound :: Int32))
       putStrLn (show (7777777 :: Int32))
       putStrLn (show (-7777777 :: Int32))
       putStrLn "\nInt32 constants: overflow"
       putStrLn (show (777777777777777 :: Int32))
       putStrLn (show (-777777777777777 :: Int32))
       putStrLn (show (777777777777777777777777777777 :: Int32))
       putStrLn (show (-777777777777777777777777777777 :: Int32))
       putStrLn "\nInt32 arithmetic"
       putStrLn (show (1 + 1 :: Int32))
       putStrLn (show (7777777 + 7777777 :: Int32))
       putStrLn (show (7777777 - 7777777 :: Int32))
       putStrLn (show (7777777 * 2 :: Int32))
       putStrLn (show ((7777777 * 2) `quot` 2 :: Int32))
       let (q,r) = (7777777 :: Int32) `quotRem` (2 :: Int32)
       putStrLn ("7777777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nInt16 constants"
       putStrLn (show (minBound :: Int16))
       putStrLn (show (maxBound :: Int16))
       putStrLn (show (7777 :: Int16))
       putStrLn (show (-7777 :: Int16))
       putStrLn "\nInt16 constants: overflow"
       putStrLn (show (777777777777777 :: Int16))
       putStrLn (show (-777777777777777 :: Int16))
       putStrLn "\nInt16 arithmetic"
       putStrLn (show (1 + 1 :: Int16))
       putStrLn (show (7777 + 7777 :: Int16))
       putStrLn (show (7777 - 7777 :: Int16))
       putStrLn (show (7777 * 2 :: Int16))
       putStrLn (show ((7777 * 2) `quot` 2 :: Int16))
       let (q,r) = (7777 :: Int16) `quotRem` (2 :: Int16)
       putStrLn ("7777 `quotRem` 2: " ++ show q ++ "," ++ show r)

       putStrLn "\nInt8 constants"
       putStrLn (show (minBound :: Int8))
       putStrLn (show (maxBound :: Int8))
       putStrLn (show (77 :: Int8))
       putStrLn (show (-77 :: Int8))
       putStrLn "\nInt8 constants: overflow"
       putStrLn (show (7777777 :: Int8))
       putStrLn (show (-7777777 :: Int8))
       putStrLn "\nInt8 arithmetic"
       putStrLn (show (1 + 1 :: Int8))
       putStrLn (show (77 + 77 :: Int8))
       putStrLn (show (77 - 77 :: Int8))
       putStrLn (show (77 * 2 :: Int8))
       putStrLn (show ((77 * 2) `quot` 2 :: Int8))
       let (q,r) = (77 :: Int8) `quotRem` (2 :: Int8)
       putStrLn ("77 `quotRem` 2: " ++ show q ++ "," ++ show r)


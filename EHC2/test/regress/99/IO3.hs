{- ----------------------------------------------------------------------------------------
   what    : IO
   expected: error, attempt to open non existent file for reading
---------------------------------------------------------------------------------------- -}

module Main where

main :: IO ()
main
  = do h1 <- openFile "filesForIOTesting/nonexistent" ReadMode
       hClose h1

{- ----------------------------------------------------------------------------------------
   what    : yield prog name
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System

main :: IO ()
main
  = do a <- getProgName
       putStrLn a

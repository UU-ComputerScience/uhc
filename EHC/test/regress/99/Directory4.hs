{- ----------------------------------------------------------------------------------------
   what    : Non existing files
   expected: ok
   constraints: exclude-if-jscript
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory

file :: String
file = "DoesNotExists"


main :: IO ()
main = do
  doesFileExist file  >>= print
  

{- ----------------------------------------------------------------------------------------
   what    : parsing of empty record
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

data Inh_SStrings = Inh_SStrings {}
  deriving Show

main = putStrLn $ "Ok: " ++ show Inh_SStrings

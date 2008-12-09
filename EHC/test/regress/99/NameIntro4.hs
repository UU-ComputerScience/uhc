{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- combi of value introducing
vvv = 'c'
data VVV = VVV { vvv :: Int }

-- combi of value introducing involving function bindings (where >1 are allowed)
fff x = '4'
fff x = '5'

fff = '5'
data FFF = FFF { fff :: Int }

main :: IO ()
main = putStr "Dummy"

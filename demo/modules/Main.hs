module Main where

import Lib

main = succ number

instance Succ N where
  succ n = S n

-- main = id number

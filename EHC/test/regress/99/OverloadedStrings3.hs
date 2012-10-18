{-# LANGUAGE OverloadedStrings #-}

{- ----------------------------------------------------------------------------------------
   what    : overloading of strings, pattern match
   expected: ok
---------------------------------------------------------------------------------------- -}

module OverloadedStrings3 where

import Data.String

data S = S String deriving (Eq,Show)
newtype S2 = S2 String deriving (Eq,Show)

instance IsString S where
  fromString = S

instance IsString S2 where
  fromString = S2

x_S :: S
x_S = "S"

x_S2 :: S2
x_S2 = "S2"

x_is_S x = case x of
  "S" -> True
  _   -> False

main = do
  print (x_is_S x_S)
  print (x_is_S x_S2)

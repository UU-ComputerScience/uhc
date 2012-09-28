{-# LANGUAGE OverloadedStrings, NoGenericDeriving #-}

{- ----------------------------------------------------------------------------------------
   what    : overloading of strings
   expected: ok, use of IsString, proper defaulting, explicit typing
---------------------------------------------------------------------------------------- -}

module OverloadedStrings2 where

import Data.String

x = "aap"

data S = S String deriving Show

instance IsString S where
  fromString = S

main = do
  print x
  print "noot"	-- ambig, should default
  print ("mies" :: S)
  print ("klaas" :: String)


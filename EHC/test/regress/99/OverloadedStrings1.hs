{-# LANGUAGE OverloadedStrings, NoGenericDeriving #-}

{- ----------------------------------------------------------------------------------------
   what    : overloading of strings
   expected: ok, use of IsString, proper defaulting
---------------------------------------------------------------------------------------- -}

module OverloadedStrings1 where

x = "aap"

main = do
  print x
  print "noot"	-- ambig, should default

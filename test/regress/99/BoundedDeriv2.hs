{- ----------------------------------------------------------------------------------------
   what    : Bounded class, derived (via generics)
   expected: ok
   platform: word size dependent
---------------------------------------------------------------------------------------- -}

module BoundedDeriv2 where

import Data.Int
import Data.Word

main :: IO ()
main
  = do print (minBound :: (Int,Char))
       print (maxBound :: (Int,Char))
       print (minBound :: (Bool,Int8,Int16,Int32,Int64))
       print (maxBound :: (Bool,Int8,Int16,Int32,Int64))

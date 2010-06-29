{- ----------------------------------------------------------------------------------------
   what    : some small tests for Control.Applicative
   expected: ok
---------------------------------------------------------------------------------------- -}

module ControlApplicative1 where

import Control.Applicative

newtype ZipList a = ZipList [a]
  deriving Show
 
instance Functor ZipList where
   fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
   (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
   pure x                        = ZipList (repeat x)


main = do
  putStrLn (show $ [(+(1::Int)),(*(3::Int))] <*> [5::Int, 6])
  putStrLn (show $ ZipList [(+(1::Int)),(*(3::Int))] <*> ZipList [5::Int, 6])

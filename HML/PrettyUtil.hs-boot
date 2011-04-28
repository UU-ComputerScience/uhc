module PrettyUtil where

class Pretty a where
   pp :: a -> ShowS

type Name = String

-- | Generate a fresh variable
fresh :: Int -> (Name, Int)
module Main where

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int

data Maybe =
    Nothing
  | Just Int

double :: Int -> Int
double x = x + x

doubleJust :: Maybe -> Int
doubleJust (Just x) = double x
doubleJust Nothing  = 42

pick :: Maybe -> Maybe -> Maybe
pick Nothing m = m
pick m Nothing = m
pick m _       = m

split :: Maybe -> Maybe
split m = pick m m 

main = doubleJust (split (Just 3))


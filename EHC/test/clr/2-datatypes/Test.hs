module Main where

data MyData =
    One   Int
  | Two   Int Int
  | Three Int Int Int
  | Four  Int Int Int Int

first :: MyData -> Int
first (One i) = i
first (Two i _) = i
first (Three i _ _) = i
first (Four i _ _ _) = i

main = first (Two 7 3)


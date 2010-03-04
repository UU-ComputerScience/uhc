module Foldl where

data Lis a = Con a (Lis a) | Ni
data I = Suc I | Z
  deriving Show

plus :: I -> I -> I
plus i j = case j of
  Z -> i
  Suc k -> plus (Suc i) k

one = Suc Z
two = Suc one  
  
foldy :: (a -> b -> a) -> a -> Lis b -> a
foldy f a xs = case xs of
                Ni -> a
                (Con x xs) -> foldy f (f a x) xs
         
x1 = foldy plus Z (Con one (Con two Ni))

x2 = case (Con one (Con two Ni)) of
                Ni -> Z
                Con x xs -> foldy plus (plus Z x) xs
                
x3 = foldy plus (plus Z one) (Con two Ni)

main = putStrLn (show x1 ++ show x2 ++ show x3)
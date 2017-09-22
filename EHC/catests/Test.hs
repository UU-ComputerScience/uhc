{-# LANGUAGE BangPatterns #-}
module Test where

t1 :: Int
t1 = let y = 3 in 5

tmap :: [Bool]
tmap = map not (True:[])

tFail = map not $ []

tfailbase = let u6 = map not in ($) u6 []

u6 = map not
tFail2 = ($) u6 []

tfailOpt = let !u6 = map not in let !u1 = [] in 
  let !u2 = u6 in ($) u6 []

tmapbase = \xs -> let u6 = True : xs in
  let u5 = map not in u5 $ u6

tmapopt = let u6 = True : [False] in
  let u5 = map not in 
    let !u3 = u6 in 
      let !u4 = u5 
      in u4 $ u3

x = if True || False then False else True

f c y z = if c y y then not x && True else not z || False

co :: a -> b -> a
co x y = x

fa :: a -> b
fa x = undefined

-- works
class E a where
  fce :: a
  -- gc :: a -> a -> a
  

funca :: E a => a
funca = fce


class D a where
  fcd :: a -> a
  -- gc :: a -> a -> a
  

func :: D a => a -> a
func x = f x
  where f = fcd

data C a = C
  { fc :: a -> a -> a
  , gc :: a -> a -> a
  }

funcd :: C a -> a -> a -> a -> a
funcd d x y z = f' (f' x y) z
  where f' = fc d


data B = T | F

bNot T = F
bNot F = T

bAnd T T = T
bAnd _ _ = F

fac :: Int -> Int 
fac n = if n <= 1 then 1 else n * fac (n - 1)

t2 :: a -> Int
t2 = let x = 1 + 2 in \y -> x + 5

t3 :: a -> Int
t3 = \y -> let x = 1 + 2 in x + 5

t4 :: Int
t4 = let x = 1 + 2 in let y = (\z -> z) x in y + y

le :: [a] -> Int
le [] = 0
le (x:xs) = 1 + length xs

addLength :: [Int] -> [Int]
addLength xs = map (+ (length xs)) xs

t5 :: Bool -> (a -> a) -> a -> a
t5 b g x = if b then x else g (g x)

fo :: (a -> b -> a) -> a -> [b] -> a
fo = \f z0 xs0 -> let
  lgo = \z ys -> case ys of
    [] -> z
    (x:xs) -> let z' = f z x in lgo z' xs
  in lgo z0 xs0

t6 :: Int
t6 = fo (+) 0 [1 .. 10000000]
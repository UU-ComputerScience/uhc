module Main where

choose :: a -> a -> a
choose a b = a

f1 :: ((forall a . a -> a) -> (forall a . a -> a)) -> Int
f1 g = let v1 = g pf 3
           v2 = g pf 'x'
           -- v3 = g (\x -> (x::Int)) 'x'
       in  3
f2 :: (forall a . (a -> a) -> a -> a) -> Int
f2 g = let v1 = g pf 3
           v2 = g pf 'x'
           v3 = g (\x -> (x::Int)) 3
       in  3

v1 = (f1 (choose id),f2 (choose id))
ch :: forall a . (a -> a) -> a -> a
ch = choose id
chid1 :: forall a . (a -> a) -> forall b . b -> b
chid1 = choose id
chid2 :: (forall a . a -> a) -> forall b . b -> b
chid2 = choose id
{-
-}
v2 = (f1 ch,f2 ch)
v3 = (f1 id,f2 id)
{-
v4 = (f1 gtry)
gtry :: (forall a . a -> a) -> (forall a . a -> a)
gtry x = x x
-}

pf :: a -> a
pf x = let v1 = pf 3
           v2 = pf 'x'
       in  x

vx = let ch = choose xx
         -- ch :: forall a . (a -> a) -> a -> a
         xx x = let yy = (f1 ch,f2 ch)
                in  x
     in  'x'

main = return ()

{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity & higher rank types
   expected: all ok
   note    : these appear on http://www.cs.uu.nl/wiki/Ehc/HigherRankComparison as well
---------------------------------------------------------------------------------------- -}

module Main where

apply :: forall a . forall b . (a -> b) -> a -> b
apply  f x = f x
revapp :: forall a . a -> forall b . (a -> b) -> b
revapp x f = f x
iapp :: (Int -> Int) -> Int -> Int
iapp   f x = f x
choose :: a -> a -> a
choose x y = x
single :: forall a . a -> [a]
single x = [x]
ii :: Int -> Int
ii = id
cons :: forall a . a -> [a] -> [a]
cons h t = h : t
revcons :: forall a . [a] -> a -> [a]
revcons t h = h : t
ids :: [forall a. a->a]
ids = []

poly (f :: forall a . a -> a) =  (f (1::Int), f True)

a1 = apply  poly
a2 = apply ~poly

ids1 = single  id
ids2 = single ~id
ids3 = single  id :: [forall a . a->a]
ids4 = single (id :: forall a . a->a)
ids5 = (single :: (forall a. a-> a) -> [forall a. a->a]) id

idss1a = single  ids1
idss1b = single ~ids1
idss2a = single  ids2
idss2b = single ~ids2

ht1  = head ids : ids
ht2  = (head ids :: forall a . a -> a) : ids
ht3  = head ~ids : ids

ht4 = head ids
ht5 = tail ids
ht6 = id   ids

ht7 = head ~ids
ht8 = tail ~ids
ht9 = id   ~ids

ch1 = choose [] ids2
ch2 = choose ids2 []
ch3 = let f = choose [] in f ids2

chid1 :: (forall a . a -> a) -> forall b . b -> b
chid1 = choose id
chid2 :: forall a . (a -> a) -> a -> a
chid2 = choose id

chp1 = choose  poly
chp2 = choose ~poly
chi1 = choose  id
chi2 = choose ~id

ra1 = revapp id poly
ra2 = let f = revapp id in f poly
ra3 = let f = revapp id in f iapp

cons1  = cons (\x->x)  ids
cons2  = cons (\x->x) ~ids						-- should be: [forall a. a->a], but is forall a.[a->a]
cons2' = cons (\x->x :: forall a . a -> a) ~ids	-- the extra annotation fixes this

cons3  = revcons  ids (\x->x)
cons4  = revcons ~ids (\x->x)

main = return ()

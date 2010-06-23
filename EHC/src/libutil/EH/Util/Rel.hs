module EH.Util.Rel
  ( Rel
  , empty
  , toList, fromList
  , singleton
  , dom, rng
  , restrictDom, restrictRng
  , mapDom, mapRng
  , partitionDom, partitionRng
  , intersection, difference, union, unions
  , apply
  , toDomMap, toRngMap
  , mapDomRng
  )
  where

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------
-- Relation
-------------------------------------------------------------------------

type Rel a b = Set.Set (a,b)

toList :: Rel a b -> [(a,b)]
toList = Set.toList

fromList :: (Ord a, Ord b) => [(a,b)] -> Rel a b
fromList = Set.fromList

singleton :: (Ord a, Ord b) => a -> b -> Rel a b
singleton a b = fromList [(a,b)]

empty :: Rel a b
empty = Set.empty

dom :: (Ord a, Ord b) => Rel a b -> Set.Set a
dom = Set.map fst

rng :: (Ord a, Ord b) => Rel a b -> Set.Set b
rng = Set.map snd

restrictDom :: (Ord a, Ord b) => (a -> Bool) -> Rel a b -> Rel a b
restrictDom p = Set.filter (p . fst)

restrictRng :: (Ord a, Ord b) => (b -> Bool) -> Rel a b -> Rel a b
restrictRng p = Set.filter (p . snd)

mapDom :: (Ord a, Ord b, Ord x) => (a -> x) -> Rel a b -> Rel x b
mapDom f = Set.map (\(a,b) -> (f a,b))

mapRng :: (Ord a, Ord b, Ord x) => (b -> x) -> Rel a b -> Rel a x
mapRng f = Set.map (\(a,b) -> (a,f b))

partitionDom :: (Ord a, Ord b) => (a -> Bool) -> Rel a b -> (Rel a b,Rel a b)
partitionDom f = Set.partition (f . fst)

partitionRng :: (Ord a, Ord b) => (b -> Bool) -> Rel a b -> (Rel a b,Rel a b)
partitionRng f = Set.partition (f . snd)

intersection :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
intersection = Set.intersection

difference :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
difference = Set.difference

union :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
union = Set.union

unions :: (Ord a, Ord b) => [Rel a b] -> Rel a b
unions = Set.unions

apply :: (Ord a, Ord b) => Rel a b -> a -> [b]
apply r a = Set.toList $ rng $ restrictDom (==a) $ r

toDomMap :: Ord a => Rel a b -> Map.Map a [b]
toDomMap r = Map.unionsWith (++) [ Map.singleton a [b] | (a,b) <- toList r ]

toRngMap :: Ord b => Rel a b -> Map.Map b [a]
toRngMap r = Map.unionsWith (++) [ Map.singleton b [a] | (a,b) <- toList r ]

mapDomRng :: (Ord a, Ord b, Ord a', Ord b') => ((a,b) -> (a',b')) -> Rel a b -> Rel a' b'
mapDomRng = Set.map


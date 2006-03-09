-------------------------------------------------------------------------
-- Gamma
-------------------------------------------------------------------------

module Gam
  ( Gam, emptyGam, gamIsEmpty, gamSingleton
  , gamTryLookups, gamTryLookupsWithDefault
  , gamLookup, gamFindWithDefault
  , gamUnions, gamUnionsShadow, gamUnion, gamUnionShadow, gamUnionWith
  , gamDelete
  , gamInsert, gamInsertShadow
  , gamDifference, gamIntersection
  , gamToMap
  , gamAssocs, gamAssocsShadow, gamFromAssocs, gamFromAssocsWith
  , gamElems, gamElemsShadow
  , gamKeys
  , gamMap, gamMapKeys, gamMapWithKey
  , gamFilterWithKey, gamFilter
  , gamPartition
  , gamMapAccumWithKey
  , gamFold, gamFoldWithKey
  , gamCheckDups
  , ppGam, ppGam'
  , dblGamLookup
  )
  where

import Data.Maybe
import qualified Data.Map as Map
import PPUtils
import UU.Pretty
import Err
import Common

-------------------------------------------------------------------------
-- Gam v1
-------------------------------------------------------------------------

{-
type Gam k v = Map.Map k v

gamSingleton :: k -> v -> Gam k v
gamSingleton = Map.singleton

gamLookup :: Ord k => k -> Gam k v -> Maybe v
gamLookup = Map.lookup

gamAssocsShadow :: Gam k v -> [(k,v)]
gamAssocsShadow = Map.assocs

gamAssocs :: Gam k v -> [(k,v)]
gamAssocs = gamAssocsShadow

gamAssocs' :: Gam k v -> [[(k,v)]]
gamAssocs' g = [ [kv] | kv <- Map.assocs g ]

gamElemsShadow :: Gam k v -> [v]
gamElemsShadow = Map.elems

gamElems :: Gam k v -> [v]
gamElems = gamElemsShadow

gamFromAssocs :: Ord k => [(k,v)] -> Gam k v
gamFromAssocs = Map.fromList

gamFromAssocsWith :: Ord k => (v -> v -> v) -> [(k,v)] -> Gam k v
gamFromAssocsWith = Map.fromListWith

gamToMap :: Gam k v -> Map.Map k v
gamToMap = id

gamUnionWith :: Ord k => (v -> v -> v) -> Gam k v -> Gam k v -> Gam k v
gamUnionWith = Map.unionWith

gamUnion :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnion = Map.union

gamUnionShadow :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnionShadow = Map.union

gamUnions :: Ord k => [Gam k v] -> Gam k v
gamUnions = gamUnionsShadow

gamUnionsShadow :: Ord k => [Gam k v] -> Gam k v
gamUnionsShadow = Map.unions

gamInsert :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsert = gamInsertShadow

gamInsertShadow :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsertShadow = Map.insert

gamFilterWithKey :: Ord k => (k -> v -> Bool) -> Gam k v -> Gam k v
gamFilterWithKey = Map.filterWithKey

gamFilter :: Ord k => (v -> Bool) -> Gam k v -> Gam k v
gamFilter = Map.filter

gamPartition :: Ord k => (v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition = Map.partition

gamMap :: (v -> w) -> Gam k v -> Gam k w
gamMap = Map.map

gamMapWithKey :: (k -> v -> w) -> Gam k v -> Gam k w
gamMapWithKey = Map.mapWithKey

gamMapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Gam k b -> (a, Gam k c)
gamMapAccumWithKey = Map.mapAccumWithKey

gamFoldWithKey :: (k -> v -> a -> a) -> a -> Gam k v -> a
gamFoldWithKey = Map.foldWithKey

gamFold :: (v -> a -> a) -> a -> Gam k v -> a
gamFold = Map.fold

gamKeysWithDup :: Gam k v -> [k]
gamKeysWithDup g = []
-}

-------------------------------------------------------------------------
-- Gam v2
-------------------------------------------------------------------------

{-
-}
type Gam k v = Map.Map k [v]

liftWith :: (v -> v -> v) -> ([v] -> [v] -> [v])
liftWith f = \(v1:v1s) (v2:v2s) -> [f v1 v2] ++ v1s ++ v2s

liftAnyWithKey :: (k -> v -> w) -> (k -> [v] -> w)
liftAnyWithKey f = \k (v:_) -> f k v

liftAny :: (v -> w) -> ([v] -> w)
liftAny f = \(v:_) -> f v

liftMap :: (v -> w) -> ([v] -> [w])
liftMap = map

liftMapKey :: (k -> v -> w) -> (k -> [v] -> [w])
liftMapKey f k = map (f k)

liftMapAccumKey :: (a -> k -> b -> (a, c)) -> (a -> k -> [b] -> (a, [c]))
liftMapAccumKey f a k bs
  = l a bs
  where l a []     = (a,[])
        l a (b:bs) = (a3,c:cs)
                   where (a2,c ) = f a k b
                         (a3,cs) = l a2 bs

liftFoldKey :: (k -> v -> a -> a) -> (k -> [v] -> a -> a)
liftFoldKey f k vs a
  = l a vs
  where l a []     = a
        l a (v:vs) = l (f k v a) vs

liftFold :: (v -> a -> a) -> ([v] -> a -> a)
liftFold f vs a
  = l a vs
  where l a []     = a
        l a (v:vs) = l (f v a) vs

liftAssocs :: [(k,v)] -> [(k,[v])]
liftAssocs = map (\(k,v) -> (k,[v]))

gamSingleton :: k -> v -> Gam k v
gamSingleton k v = Map.singleton k [v]

gamLookup :: Ord k => k -> Gam k v -> Maybe v
gamLookup k = fmap head . Map.lookup k

gamAssocsShadow :: Gam k v -> [(k,v)]
gamAssocsShadow = Map.assocs . Map.map head

gamAssocs' :: Gam k v -> [[(k,v)]]
gamAssocs' g = [ zip (repeat k) vs | (k,vs) <- Map.assocs g ]

gamAssocs :: Gam k v -> [(k,v)]
gamAssocs = concat . gamAssocs'

gamElemsShadow :: Gam k v -> [v]
gamElemsShadow = map head . Map.elems

gamElems :: Gam k v -> [v]
gamElems = concat . Map.elems

gamFromAssocs :: Ord k => [(k,v)] -> Gam k v
gamFromAssocs = Map.fromListWith (++) . liftAssocs

gamFromAssocsWith :: Ord k => (v -> v -> v) -> [(k,v)] -> Gam k v
gamFromAssocsWith f = Map.fromListWith (liftWith f) . liftAssocs

gamToMap :: Gam k v -> Map.Map k v
gamToMap = Map.map head

gamUnionWith :: Ord k => (v -> v -> v) -> Gam k v -> Gam k v -> Gam k v
gamUnionWith f = Map.unionWith (liftWith f)

gamUnion :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnion = Map.unionWith (++)

gamUnionShadow :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnionShadow = Map.unionWith const

gamUnions :: Ord k => [Gam k v] -> Gam k v
gamUnions = Map.unionsWith (++)

gamUnionsShadow :: Ord k => [Gam k v] -> Gam k v
gamUnionsShadow = Map.unionsWith const

gamInsert :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsert k v = Map.insertWith (++) k [v]

gamInsertShadow :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsertShadow k v = Map.insertWith const k [v]

gamFilterWithKey :: Ord k => (k -> v -> Bool) -> Gam k v -> Gam k v
gamFilterWithKey f = Map.filterWithKey (liftAnyWithKey f)

gamFilter :: Ord k => (v -> Bool) -> Gam k v -> Gam k v
gamFilter f = Map.filter (liftAny f)

gamPartition :: Ord k => (v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition f = Map.partition (liftAny f)

gamMap :: (v -> w) -> Gam k v -> Gam k w
gamMap f = Map.map (liftMap f)

gamMapWithKey :: (k -> v -> w) -> Gam k v -> Gam k w
gamMapWithKey f = Map.mapWithKey (liftMapKey f)

gamMapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Gam k b -> (a, Gam k c)
gamMapAccumWithKey f = Map.mapAccumWithKey (liftMapAccumKey f)

gamFoldWithKey :: (k -> v -> a -> a) -> a -> Gam k v -> a
gamFoldWithKey f = Map.foldWithKey (liftFoldKey f)

gamFold :: (v -> a -> a) -> a -> Gam k v -> a
gamFold f = Map.fold (liftFold f)

gamKeysWithDup :: Gam k v -> [k]
gamKeysWithDup g = [ k | (k,(_:_:_)) <- Map.toList g ]

-------------------------------------------------------------------------
-- Gam v1 & v2 share
-------------------------------------------------------------------------

emptyGam :: Gam k v
emptyGam = Map.empty

gamIsEmpty :: Gam k v -> Bool
gamIsEmpty = Map.null

gamFindWithDefault :: Ord k => v -> k -> Gam k v -> v
gamFindWithDefault v k = maybe v id . gamLookup k

gamKeys :: Gam k v -> [k]
gamKeys = Map.keys

gamDelete :: Ord k => k -> Gam k v -> Gam k v
gamDelete = Map.delete

gamDifference :: Ord k => Gam k v -> Gam k w -> Gam k v
gamDifference = Map.difference

gamIntersection :: Ord k => Gam k v -> Gam k w -> Gam k v
gamIntersection = Map.intersection

gamMapKeys :: Ord j => (k -> j) -> Gam k v -> Gam j v
gamMapKeys = Map.mapKeys

-------------------------------------------------------------------------
-- Duplicate key detection
-------------------------------------------------------------------------

gamCheckDups :: PP k => SPos -> String -> String -> Gam k v -> [Err]
gamCheckDups p cx knd g
  = if null d then [] else [Err_Dups p cx knd (map pp d)]
  where d = gamKeysWithDup g

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

{-
ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam = ppListSepV "[" "]" "," . map (\(k,v) -> pp k >#< ":->" >#< pp v) . gamAssocs
-}
ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam
  = ppl . map (ppl . map (\(k,v) -> pp k >#< ":->" >#< pp v)) . gamAssocs'
  where ppl = ppListSepV "[" "]" ","

ppGam' :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam' = vlist . map (\(k,v) -> pp k >#< ":->" >#< pp v) . gamAssocs

instance (PP k,PP v) => PP (Gam k v) where
  pp g = ppGam' g

-------------------------------------------------------------------------
-- Double lookup
-------------------------------------------------------------------------

dblGamLookup :: Ord k => (i1 -> Gam k i2) -> k -> k -> Gam k i1 -> Maybe (i1,i2)
dblGamLookup gOf sn vn g
  = case gamLookup sn g of
      Just si
        -> fmap ((,) si) . gamLookup vn . gOf $ si
      _ -> Nothing

-------------------------------------------------------------------------
-- General purpose lookup with a default key
-------------------------------------------------------------------------

gamTryLookups :: Ord k => v -> (e -> v) -> [k] -> Gam k e -> v
gamTryLookups dflt extr keys g
  = case keys of
      (k:ks) -> case gamLookup k g of
                  Just i  -> extr i
                  Nothing -> gamTryLookups dflt extr ks g
      _      -> dflt

gamTryLookupsWithDefault :: Ord k => k -> v -> (e -> v) -> [k] -> Gam k e -> v
gamTryLookupsWithDefault dfltKey dflt extr keys g
  = gamTryLookups dflt extr (keys ++ [dfltKey]) g


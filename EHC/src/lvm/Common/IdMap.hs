{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: IdMap.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Common.IdMap( module Lvm.Common.Id
            , IdMap

            -- essential: used by "Asm" and "Lvm"
            , emptyMap, singleMap, elemMap, mapMap
            , insertMap, extendMap, insertMapWith
            , lookupMap, findMap
            , filterMap
            , listFromMap
            , mapMapWithId, unionMap, unionMapWith
            , updateMap

            -- exotic: used by core compiler
            , foldMap, deleteMap
            , filterMapWithId
            , mapFromList
            , unionMaps, diffMap, unionlMap
            , foldMapWithId
            , isEmptyMap, sizeMap

            ) where

import qualified Lvm.Common.IntMap as IntMap
import Lvm.Common.Id( Id, intFromId, idFromInt
         , NameSupply, splitNameSupply
         )

----------------------------------------------------------------
-- IdMap
----------------------------------------------------------------
newtype IdMap a = IdMap (IntMap.IntMap a)

emptyMap :: IdMap a
emptyMap
  = IdMap (IntMap.empty)

singleMap :: Id -> a -> IdMap a
singleMap id x
  = insertMap id x emptyMap

isEmptyMap :: IdMap a -> Bool
isEmptyMap (IdMap map)
  = IntMap.null map


elemMap :: Id -> IdMap a -> Bool
elemMap id (IdMap map)
  = IntMap.member map (intFromId id)

mapMap :: (a -> b) -> IdMap a -> IdMap b
mapMap f (IdMap map)
  = IdMap (IntMap.map f map)

mapMapWithId :: (Id -> a -> b) -> IdMap a -> IdMap b
mapMapWithId f (IdMap map)
  = IdMap (IntMap.mapWithKey (\i x -> f (idFromInt i) x) map)


insertMap :: Id -> a -> IdMap a -> IdMap a
insertMap id x (IdMap map)
  = IdMap (IntMap.insertWith fail (intFromId id) x map)
  where
    fail _ _ = error ("IdMap.insertMap: duplicate id " ++ show id)

insertMapWith :: Id -> a -> (a -> a) -> IdMap a -> IdMap a
insertMapWith id x f (IdMap map)
  = IdMap (IntMap.insertWith (\i x -> f x) (intFromId id) x map)

updateMap :: Id -> a -> IdMap a -> IdMap a
updateMap id x (IdMap map)
  = IdMap (IntMap.insertWith const (intFromId id) x map)

deleteMap :: Id -> IdMap a -> IdMap a
deleteMap id (IdMap map)
  = IdMap (IntMap.delete (intFromId id) map)

extendMap :: Id -> a -> IdMap a -> IdMap a
extendMap id x (IdMap map)
  = IdMap (IntMap.insertWith const (intFromId id) x map)

lookupMap :: Id -> IdMap a -> Maybe a
lookupMap id (IdMap map)
  = IntMap.lookupM map (intFromId id)

filterMap :: (a -> Bool) -> IdMap a -> IdMap a
filterMap p (IdMap map)
  = IdMap (IntMap.filter p map)

filterMapWithId :: (Id -> a -> Bool) -> IdMap a -> IdMap a
filterMapWithId p (IdMap map)
  = IdMap (IntMap.filterWithKey (\i x -> p (idFromInt i) x) map)

findMap :: Id -> IdMap a -> a
findMap id map
  = case lookupMap id map of
      Nothing -> error ("IdMap.findMap: unknown identifier " ++ show id)
      Just x  -> x

listFromMap :: IdMap a -> [(Id,a)]
listFromMap (IdMap idmap)
  = map (\(i,x) -> (idFromInt i,x)) (IntMap.toList idmap)

mapFromList :: [(Id,a)] -> IdMap a
mapFromList xs
  = IdMap (IntMap.fromList (map (\(id,x) -> (intFromId id,x)) xs))

diffMap :: IdMap a -> IdMap a -> IdMap a
diffMap (IdMap map1) (IdMap map2)
  = IdMap (IntMap.difference map1 map2)

unionMap :: IdMap a -> IdMap a -> IdMap a
unionMap (IdMap map1) (IdMap map2)
  = IdMap (IntMap.unionWith err map1 map2)
  where
    err a b   = error "IdMap.unionMap: duplicate identifiers"

unionMapWith :: (a->a->a) -> IdMap a -> IdMap a -> IdMap a
unionMapWith f (IdMap map1) (IdMap map2)
  = IdMap (IntMap.unionWith (\x y -> f x y) map1 map2)


unionlMap :: IdMap a -> IdMap a -> IdMap a
unionlMap (IdMap map1) (IdMap map2)
  = IdMap (IntMap.unionl map1 map2)

unionMaps maps
  = foldr unionMap emptyMap maps

foldMapWithId :: (Id -> a -> b -> b) -> b -> IdMap a -> b
foldMapWithId f z (IdMap map)
  = IntMap.foldWithKey (\i x z -> f (idFromInt i) x z) z map

foldMap :: (a -> b -> b) -> b -> IdMap a -> b
foldMap f z (IdMap map)
  = IntMap.fold (\x z -> f x z) z map

sizeMap :: IdMap a -> Int
sizeMap (IdMap map)
  = IntMap.size map

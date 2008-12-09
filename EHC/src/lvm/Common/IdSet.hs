{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: IdSet.hs 222 2004-02-14 16:33:04Z uust $

-- this module is exotic, only used by the core compiler
-- but it works with any IdMap
module Lvm.Common.IdSet( module Lvm.Common.Id
            , IdSet
            , emptySet, singleSet
            , elemSet, filterSet, foldSet
            , insertSet, deleteSet
            , unionSet, unionSets, diffSet
            , listFromSet, setFromList
            , sizeSet, isEmptySet

            , mapFromSet
            , setFromMap,
            ) where

import Lvm.Common.Id   ( Id )
import Lvm.Common.IdMap

----------------------------------------------------------------
-- IdSet
----------------------------------------------------------------
type IdSet  = IdMap ()

emptySet :: IdSet
emptySet
  = emptyMap

singleSet :: Id -> IdSet
singleSet id
  = insertMap id () emptyMap

elemSet :: Id -> IdSet -> Bool
elemSet id set
  = elemMap id set

filterSet :: (Id -> Bool) -> IdSet -> IdSet
filterSet pred set
  = filterMapWithId (\id x -> pred id) set

foldSet :: (Id -> a ->  a) -> a -> IdSet -> a
foldSet f x set
  = foldMapWithId (\id x -> f id) x set


insertSet :: Id -> IdSet -> IdSet
insertSet id set
  = insertMap id () set

deleteSet :: Id -> IdSet -> IdSet
deleteSet id set
  = deleteMap id set

unionSet :: IdSet -> IdSet -> IdSet
unionSet set1 set2
  = unionMap set1 set2

unionSets :: [IdSet] -> IdSet
unionSets sets
  = foldr unionSet emptySet sets

diffSet :: IdSet -> IdSet -> IdSet
diffSet set1 set2
  = diffMap set1 set2

listFromSet :: IdSet -> [Id]
listFromSet set
  = map fst (listFromMap set)

setFromList :: [Id] -> IdSet
setFromList xs
  = mapFromList (map (\id -> (id,())) xs)

sizeSet :: IdSet -> Int
sizeSet set
  = sizeMap set

isEmptySet :: IdSet -> Bool
isEmptySet set
  = isEmptyMap set

indicesMap :: IdMap a -> IdSet
indicesMap map
  = mapMap (const ()) map

setFromMap :: IdMap a -> IdSet
setFromMap map
  = indicesMap map

mapFromSet :: (Id -> a) -> IdSet -> IdMap a
mapFromSet f set
  = mapMapWithId (\id () -> f id) set

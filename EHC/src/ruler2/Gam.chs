-------------------------------------------------------------------------
-- Gamma
-------------------------------------------------------------------------

%%[1 hs module(Gam)
%%]

%%[1 hs export ( Gam, emptyGam, gamIsEmpty, gamSingleton, gamMember)
%%]
  
%%[1 hs export (gamTryLookups, gamTryLookupsWithDefault)
%%]

%%[1 hs export (gamLookup, gamLookupMaybe, gamLookupJust, gamFindWithDefault)
%%]

%%[1 hs export (gamUnions, gamUnionsShadow, gamUnion, gamUnionShadow, gamUnionWith)
%%]

%%[1 hs export (gamDelete)
%%]

%%[1 hs export (gamInsert, gamInsertShadow)
%%]

%%[1 hs export (gamDifference, gamIntersection)
%%]

%%[1 hs export (gamToMap)
%%]

%%[1 hs export (gamAssocs, gamAssocsShadow, gamFromAssocs, gamFromAssocsWith)
%%]

%%[1 hs export (gamElems, gamElemsShadow)
%%]

%%[1 hs export (gamKeys)
%%]

%%[1 hs export (gamMap, gamMapKeys, gamMapWithKey)
%%]

%%[1 hs export (gamFilterWithKey, gamFilter)
%%]

%%[1 hs export (gamPartition)
%%]

%%[1 hs export (gamMapAccumWithKey)
%%]

%%[1 hs export (gamFold, gamFoldWithKey)
%%]

%%[1 hs export (gamCheckDups)
%%]

%%[1 hs export (ppGam, ppGam')
%%]

%%[1 hs export (dblGamLookup, tripleGamLookup)
%%]

%%[1 hs export (GamMerge(..))
%%]

%%[1 hs import (Data.Maybe)
%%]

%%[1 hs import (qualified Data.Map as Map)
%%]

%%[1 hs import (EH.Util.Pretty)
%%]

%%[1 hs import (EH.Util.Utils)
%%]

%%[1 hs import (Err)
%%]

%%[1 hs import (Common)
%%]


%%[1

-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

type Gam k v = Map.Map k [v]

liftList :: (v -> v -> v) -> ([v] -> [v] -> [v])
liftList f = \(v1:v1s) (v2:v2s) -> [f v1 v2] ++ v1s ++ v2s

liftList2 :: (v -> v -> v) -> ([v] -> [v] -> [v])
liftList2 f = \v1s v2s -> [foldr1 f (v1s ++ v2s)]

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

gamLookupMaybe :: Ord k => w -> (v -> w) -> k -> Gam k v -> w
gamLookupMaybe n j k = maybe n j . gamLookup k

gamLookupJust :: Ord k => k -> Gam k v -> v
gamLookupJust = gamLookupMaybe (panic "gamLookupJust") id

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
gamFromAssocsWith f = Map.fromListWith (liftList f) . liftAssocs

gamToMap :: Gam k v -> Map.Map k v
gamToMap = Map.map head

gamUnionWith :: Ord k => (v -> v -> v) -> Gam k v -> Gam k v -> Gam k v
gamUnionWith f = Map.unionWith (liftList f)

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

gamMember :: Ord k => k -> Gam k v -> Bool
gamMember = Map.member

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
-- Merging
-------------------------------------------------------------------------

class GamMerge g where
  gamMerge :: g -> g -> g

instance (Ord k,GamMerge v) => GamMerge (Gam k v) where
  gamMerge = Map.unionWith (liftList2 gamMerge)

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
-- Double/... lookup
-------------------------------------------------------------------------

contGamLookup :: Ord k => (i1 -> Gam k i2) -> (i2 -> r) -> k -> i1 -> Maybe r
contGamLookup g2Of mk n2 = fmap mk . gamLookup n2 . g2Of

dblGamLookup :: Ord k => (i1 -> Gam k i2) -> k -> k -> Gam k i1 -> Maybe (i1,i2)
dblGamLookup g2Of n1 n2 g
  = case gamLookup n1 g of
      Just i1
        -> contGamLookup g2Of ((,) i1) n2 i1
      _ -> Nothing

tripleGamLookup :: Ord k => (i1 -> Gam k i2) -> (i2 -> Gam k i3) -> k -> k -> k -> Gam k i1 -> Maybe (i1,i2,i3)
tripleGamLookup g2Of g3Of n1 n2 n3 g
  = case dblGamLookup g2Of n1 n2 g of
      Just (i1,i2)
        -> contGamLookup g3Of (\i3 -> (i1,i2,i3)) n3 i2
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

%%]

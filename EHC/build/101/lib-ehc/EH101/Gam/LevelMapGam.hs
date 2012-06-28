module EH101.Gam.LevelMapGam
( LGam, emptyLGam
, lgamFilterMapEltAccumWithKey, lgamMapEltWithKey, lgamMapThr, lgamMap
, lgamSingleton
, lgamUnion
, lgamPartitionEltWithKey, lgamPartitionWithKey
, lgamUnzip
, lgamPop, lgamTop
, lgamPushNew, lgamPushGam
, lgamLookupDup
, lgamToAssocDupL, lgamFromAssocDupL
, lgamNoDups )
where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import EH101.Base.Common

{-# LINE 36 "src/ehc/Gam/LevelMapGam.chs" #-}
data LGamElt v
  = LGamElt
      { lgeLev		:: !Int
      , lgeVal		:: !v
      }

data LGam k v
  = LGam
      { lgLev		:: !Int
      , lgMap		:: Map.Map k [LGamElt v]		-- strictness has negative mem usage effect. Why??
      }

emptyLGam :: LGam k v
emptyLGam = LGam 0 Map.empty

instance Show (LGam k v) where
  show _ = "LGam"

{-# LINE 56 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamFilterMapEltAccumWithKey
  :: (Ord k')
       => (k -> LGamElt v -> Bool)
          -> (k -> LGamElt v -> acc -> (k',LGamElt v',acc))
          -> (k -> LGamElt v -> acc -> acc)
          -> acc -> LGam k v -> (LGam k' v',acc)
lgamFilterMapEltAccumWithKey p fyes fno a g
  = (g {lgMap = m'},a')
  where (m',a') = Map.foldrWithKey
                    (\k es ma@(m,a)
                      -> foldr (\e (m,a)
                                 -> if p k e
                                    then let (k',e',a') = fyes k e a
                                         in  (Map.insertWith (++) k' [e'] m,a')
                                    else (m,fno k e a)
                               ) ma es
                    ) (Map.empty,a) (lgMap g)

lgamMapEltWithKey :: (Ord k,Ord k') => (k -> LGamElt v -> (k',LGamElt v')) -> LGam k v -> LGam k' v'
lgamMapEltWithKey f g
  = g'
  where (g',_) = lgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e a -> let (k',e') = f k e in (k',e',a)) undefined () g

lgamMapThr :: (Ord k,Ord k') => ((k,v) -> t -> ((k',v'),t)) -> t -> LGam k v -> (LGam k' v',t)
lgamMapThr f thr g = lgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e thr -> let ((k',v'),thr') = f (k,lgeVal e) thr in (k',e {lgeVal = v'},thr')) undefined thr g

lgamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> LGam k v -> LGam k' v'
lgamMap f g = lgamMapEltWithKey (\k e -> let (k',v') = f (k,lgeVal e) in (k',e {lgeVal = v'})) g

{-# LINE 87 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamSingleton :: k -> v -> LGam k v
lgamSingleton k v = LGam 0 (Map.singleton k [LGamElt 0 v])

{-# LINE 92 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamUnion :: Ord k => LGam k v -> LGam k v -> LGam k v
lgamUnion g1@(LGam {lgMap = m1}) g2@(LGam {lgLev = l2, lgMap = m2})
  = g2 {lgMap = Map.unionWith (++) m1' m2}
  -- where m1' = Map.map (\(e:_) -> [e {lgeLev = l2}]) $ Map.filter (\es -> not (null es)) m1
  where m1' = Map.map (map (\e -> e {lgeLev = l2})) $ Map.filter (\es -> not (null es)) m1

{-# LINE 100 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamPartitionEltWithKey :: Ord k => (k -> LGamElt v -> Bool) -> LGam k v -> (LGam k v,LGam k v)
lgamPartitionEltWithKey p g
  = (g1, LGam (lgLev g1) m2)
  where (g1,m2) = lgamFilterMapEltAccumWithKey p (\k e a -> (k,e,a)) (\k e a -> Map.insertWith (++) k [e] a) Map.empty g

lgamPartitionWithKey :: Ord k => (k -> v -> Bool) -> LGam k v -> (LGam k v,LGam k v)
lgamPartitionWithKey p = lgamPartitionEltWithKey (\k e -> p k (lgeVal e))

{-# LINE 110 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamUnzip :: Ord k => LGam k (v1,v2) -> (LGam k v1,LGam k v2)
lgamUnzip g
  = (g1, g1 {lgMap = m2})
  where (g1,m2) = lgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e@(LGamElt {lgeVal = (v1,v2)}) m -> (k,e {lgeVal = v1},Map.insertWith (++) k [e {lgeVal = v2}] m)) undefined Map.empty g

{-# LINE 117 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamPop :: Ord k => LGam k v -> (LGam k v,LGam k v)
lgamPop g@(LGam {lgMap = m, lgLev = l})
  = (LGam 0 $ mk ts, LGam (max 0 (l-1)) $ mk rs)
  where (ts,rs) = unzip [ ((k,t),(k,r)) | (k,es) <- Map.toList m, let (t,r) = span (\e -> l <= lgeLev e) es ]
        mk = Map.filter (not . null) . Map.fromList
{-
lgamPop :: Ord k => LGam k v -> (LGam k v,LGam k v)
lgamPop g
  = (lgamMapEltWithKey (\k e -> (k,e {lgeLev = 0})) (g1 {lgLev = 0}),g2 {lgLev = max 0 (lgLev g - 1)})
  where (g1,g2) = lgamPartitionEltWithKey (\_ e -> lgeLev e == lgLev g) g
-}

-- gamTop = fst . gamPop
lgamTop :: Ord k => LGam k v -> LGam k v
lgamTop g@(LGam {lgMap = m, lgLev = l})
  = LGam 0 $ Map.fromList [ (k,t) | (k,es) <- Map.toList m, let t = takeWhile (\e -> l <= lgeLev e) es, not (null t) ]

{-# LINE 136 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamPushNew :: LGam k v -> LGam k v
lgamPushNew g = g {lgLev = lgLev g + 1}

lgamPushGam :: Ord k => LGam k v -> LGam k v -> LGam k v
lgamPushGam g1 g2 = g1 `lgamUnion` lgamPushNew g2

{-# LINE 144 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamLookupDup :: Ord k => k -> LGam k v -> Maybe [v]
lgamLookupDup k = fmap (map lgeVal) . Map.lookup k . lgMap

{-# LINE 149 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamToAssocDupL :: LGam k v -> AssocL k [v]
lgamToAssocDupL = Map.toList . Map.map (map lgeVal) . lgMap

lgamFromAssocDupL :: Ord k => AssocL k [v] -> LGam k v
lgamFromAssocDupL l
  = LGam 0 m
  where m = Map.map (map (LGamElt 0)) $ Map.fromList l

{-# LINE 159 "src/ehc/Gam/LevelMapGam.chs" #-}
lgamNoDups :: LGam k v -> LGam k v
lgamNoDups g
  = g {lgMap = m}
  where m = Map.map (\(e:_) -> [e]) $ lgMap g


%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment, Map based, providing levels for scoping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment/Gamma where the lexical level is used to provide nesting behavior.
Both a LGam and its entries know at which level they are.

Insertion is efficient, lookup also, because a single Map is used.
However, popping is expensive, it does not scale.
See Gam/ScopeMapGam for the next generation of this impl.
%%]

%%[9 module {%{EH}Gam.LevelMapGam}
%%]

%%[9 import(qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe,Data.List)
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Level Gam, a Gam with entries having a level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(LGam,emptyLGam)
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
%%]

%%[9 export(lgamFilterMapEltAccumWithKey,lgamMapEltWithKey,lgamMapThr,lgamMap)
lgamFilterMapEltAccumWithKey
  :: (Ord k')
       => (k -> LGamElt v -> Bool)
          -> (k -> LGamElt v -> acc -> (k',LGamElt v',acc))
          -> (k -> LGamElt v -> acc -> acc)
          -> acc -> LGam k v -> (LGam k' v',acc)
lgamFilterMapEltAccumWithKey p fyes fno a g
  = (g {lgMap = m'},a')
  where (m',a') = Map.foldWithKey
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
%%]

%%[9 export(lgamSingleton)
lgamSingleton :: k -> v -> LGam k v
lgamSingleton k v = LGam 0 (Map.singleton k [LGamElt 0 v])
%%]

%%[9 export(lgamUnion)
lgamUnion :: Ord k => LGam k v -> LGam k v -> LGam k v
lgamUnion g1@(LGam {lgMap = m1}) g2@(LGam {lgLev = l2, lgMap = m2})
  = g2 {lgMap = Map.unionWith (++) m1' m2}
  -- where m1' = Map.map (\(e:_) -> [e {lgeLev = l2}]) $ Map.filter (\es -> not (null es)) m1
  where m1' = Map.map (map (\e -> e {lgeLev = l2})) $ Map.filter (\es -> not (null es)) m1
%%]

%%[9 export(lgamPartitionEltWithKey,lgamPartitionWithKey)
lgamPartitionEltWithKey :: Ord k => (k -> LGamElt v -> Bool) -> LGam k v -> (LGam k v,LGam k v)
lgamPartitionEltWithKey p g
  = (g1, LGam (lgLev g1) m2)
  where (g1,m2) = lgamFilterMapEltAccumWithKey p (\k e a -> (k,e,a)) (\k e a -> Map.insertWith (++) k [e] a) Map.empty g

lgamPartitionWithKey :: Ord k => (k -> v -> Bool) -> LGam k v -> (LGam k v,LGam k v)
lgamPartitionWithKey p = lgamPartitionEltWithKey (\k e -> p k (lgeVal e))
%%]

%%[9 export(lgamUnzip)
lgamUnzip :: Ord k => LGam k (v1,v2) -> (LGam k v1,LGam k v2)
lgamUnzip g
  = (g1, g1 {lgMap = m2})
  where (g1,m2) = lgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e@(LGamElt {lgeVal = (v1,v2)}) m -> (k,e {lgeVal = v1},Map.insertWith (++) k [e {lgeVal = v2}] m)) undefined Map.empty g
%%]

%%[9 export(lgamPop,lgamTop)
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
%%]

%%[9 export(lgamPushNew,lgamPushGam)
lgamPushNew :: LGam k v -> LGam k v
lgamPushNew g = g {lgLev = lgLev g + 1}

lgamPushGam :: Ord k => LGam k v -> LGam k v -> LGam k v
lgamPushGam g1 g2 = g1 `lgamUnion` lgamPushNew g2
%%]

%%[9 export(lgamLookupDup)
lgamLookupDup :: Ord k => k -> LGam k v -> Maybe [v]
lgamLookupDup k = fmap (map lgeVal) . Map.lookup k . lgMap
%%]

%%[9 export(lgamToAssocDupL,lgamFromAssocDupL)
lgamToAssocDupL :: LGam k v -> AssocL k [v]
lgamToAssocDupL = Map.toList . Map.map (map lgeVal) . lgMap

lgamFromAssocDupL :: Ord k => AssocL k [v] -> LGam k v
lgamFromAssocDupL l
  = LGam 0 m
  where m = Map.map (map (LGamElt 0)) $ Map.fromList l
%%]

%%[9 export(lgamNoDups)
lgamNoDups :: LGam k v -> LGam k v
lgamNoDups g
  = g {lgMap = m}
  where m = Map.map (\(e:_) -> [e]) $ lgMap g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999
instance ForceEval v => ForceEval (LGamElt v) where
  forceEval x@(LGamElt l v) | forceEval v `seq` True = x
%%[[102
  fevCount (LGamElt l v) = cm1 "LGamElt" `cmUnion` fevCount l `cmUnion` fevCount v
%%]]

instance (ForceEval k, ForceEval v) => ForceEval (LGam k v) where
  forceEval x@(LGam l m) | forceEval m `seq` True = x
%%[[102
  fevCount (LGam l m) = cm1 "LGam" `cmUnion` fevCount l `cmUnion` fevCount m
%%]]
%%]


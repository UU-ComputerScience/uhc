%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment, Map based, providing levels for scoping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment/Gamma where the lexical level + scoping is used to provide nesting behavior.
Both a SGam and its entries know at which scope they are.

Insertion is efficient, lookup also, because a single Map is used.
%%]

%%[9 module {%{EH}Gam.ScopeMapGam}
%%]

%%[9 import(qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe,Data.List)
%%]
%%[9 import(EH.Util.Utils)
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999 import(EH.Util.Debug)
%%]

%%[9
%%]
tr _ _ x = x

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scope Gam, a Gam with entries having a level in a scope, and the Gam a scope
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(SGam,emptySGam)
type Scp = [Int]									-- a stack of scope idents defines what's in scope

data SGamElt v
  = SGamElt
      { sgeScpId	:: !Int							-- scope ident
      , sgeVal		:: v
      }

type SMap k v = Map.Map k [SGamElt v]	

data SGam k v
  = SGam
      { sgScpId		:: !Int							-- current scope, increment with each change in scope
      , sgScp		:: !Scp							-- 
      -- , sgDelayed	:: (SGam k v -> SGam k v)		-- delayed postprocessing, in particular to cleanup the map
      , sgMap		:: SMap k v	
      }

mkSGam :: SMap k v -> SGam k v
mkSGam = SGam 0 [0] -- id

emptySGam :: SGam k v
emptySGam = mkSGam Map.empty

instance Show (SGam k v) where
  show _ = "SGam"
%%]

%%[9
-- scope ident in scope?
inScp :: Scp -> Int -> Bool
inScp = flip elem

-- sgam elt in scope?
sgameltInScp :: Scp -> SGamElt v -> Bool
sgameltInScp scp = inScp scp . sgeScpId
%%]

%%[9
-- filter out the out of scopes
sgameltFilterInScp :: Scp -> [SGamElt v] -> [SGamElt v]
sgameltFilterInScp scp = filter (sgameltInScp scp)

-- map the in scopes
sgameltMapInScp :: Scp -> (v -> v) -> [SGamElt v] -> [SGamElt v]
sgameltMapInScp scp f = map (\e -> if sgameltInScp scp e then e {sgeVal = f (sgeVal e)} else e)

-- extract the in scopes
sgameltGetFilterInScp :: Scp -> (v -> v') -> [SGamElt v] -> [v']
sgameltGetFilterInScp scp f es = [ f (sgeVal e) | e <- es, sgameltInScp scp e ]
%%]

%%[9
%%]
sgamDelayFilterScp :: SGam k v -> SGam k v
sgamDelayFilterScp g = g {sgDelayed = sgamFilterInScp}

sgamDoDelayed :: SGam k v -> SGam k v
sgamDoDelayed g = sgDelayed g $ g {sgDelayed = id}

%%[9
mapFilterInScp :: Ord k => Scp -> (SGamElt v -> SGamElt v) -> SMap k v -> SMap k v
mapFilterInScp scp f m
  = Map.mapMaybe (\es -> maybeNull Nothing (Just . map f) $ sgameltFilterInScp scp es) m

sgamFilterInScp :: Ord k => SGam k v -> SGam k v
sgamFilterInScp g@(SGam {sgScp = scp, sgMap = m})
  = g {sgMap = mapFilterInScp scp id m}
%%]

%%[9 export(sgamFilterMapEltAccumWithKey,sgamMapEltWithKey,sgamMapThr,sgamMap)
sgamFilterMapEltAccumWithKey
  :: (Ord k')
       => (k -> SGamElt v -> Bool)
          -> (k -> SGamElt v -> acc -> (k',SGamElt v',acc))
          -> (k -> SGamElt v -> acc -> acc)
          -> acc -> SGam k v -> (SGam k' v',acc)
sgamFilterMapEltAccumWithKey p fyes fno a g
  = (g {sgMap = m'},a')
  where (m',a') = Map.foldWithKey
                    (\k es ma@(m,a)
                      -> foldr (\e (m,a)
                                 -> if p k e
                                    then let (k',e',a') = fyes k e a
                                         in  (Map.insertWith (++) k' [e'] m,a')
                                    else (m,fno k e a)
                               ) ma
                         $ sgameltFilterInScp (sgScp g) es
                    ) (Map.empty,a) (sgMap g)

sgamMapEltWithKey :: (Ord k,Ord k') => (k -> SGamElt v -> (k',SGamElt v')) -> SGam k v -> SGam k' v'
sgamMapEltWithKey f g
  = g'
  where (g',_) = sgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e a -> let (k',e') = f k e in (k',e',a)) undefined () g

sgamMapThr :: (Ord k,Ord k') => ((k,v) -> t -> ((k',v'),t)) -> t -> SGam k v -> (SGam k' v',t)
sgamMapThr f thr g = sgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e thr -> let ((k',v'),thr') = f (k,sgeVal e) thr in (k',e {sgeVal = v'},thr')) undefined thr g

sgamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> SGam k v -> SGam k' v'
sgamMap f g = sgamMapEltWithKey (\k e -> let (k',v') = f (k,sgeVal e) in (k',e {sgeVal = v'})) g
%%]

%%[9 export(sgamSingleton)
sgamSingleton :: k -> v -> SGam k v
sgamSingleton k v = mkSGam (Map.singleton k [SGamElt 0 v])
%%]

%%[9 export(sgamUnion)
sgamUnion :: Ord k => SGam k v -> SGam k v -> SGam k v
sgamUnion g1@(SGam {sgScp = scp1, sgMap = m1}) g2@(SGam {sgScp = scp2@(hscp2:_), sgMap = m2})
  = g2 {sgMap = Map.unionWith (++) m1' m2}
  where m1' = mapFilterInScp scp1 (\e -> e {sgeScpId = hscp2}) m1
%%]

%%[9 export(sgamPartitionEltWithKey,sgamPartitionWithKey)
sgamPartitionEltWithKey :: Ord k => (k -> SGamElt v -> Bool) -> SGam k v -> (SGam k v,SGam k v)
sgamPartitionEltWithKey p g
  = (g1, SGam (sgScpId g1) (sgScp g1) m2)
  where (g1,m2) = sgamFilterMapEltAccumWithKey p (\k e a -> (k,e,a)) (\k e a -> Map.insertWith (++) k [e] a) Map.empty g

sgamPartitionWithKey :: Ord k => (k -> v -> Bool) -> SGam k v -> (SGam k v,SGam k v)
sgamPartitionWithKey p = sgamPartitionEltWithKey (\k e -> p k (sgeVal e))
%%]

%%[9 export(sgamUnzip)
sgamUnzip :: Ord k => SGam k (v1,v2) -> (SGam k v1,SGam k v2)
sgamUnzip g
  = (g1, g1 {sgMap = m2})
  where (g1,m2) = sgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e@(SGamElt {sgeVal = (v1,v2)}) m -> (k,e {sgeVal = v1},Map.insertWith (++) k [e {sgeVal = v2}] m)) undefined Map.empty g
%%]

%%[9 export(sgamPop,sgamTop)
sgamPop :: Ord k => SGam k v -> (SGam k v, SGam k v)
sgamPop g@(SGam {sgMap = m, sgScpId = scpId, sgScp = scp@(hscp:tscp)})
  = (sgamFilterInScp $ SGam scpId [hscp] m, sgamFilterInScp $ SGam scpId tscp m)

sgamTop :: Ord k => SGam k v -> SGam k v
sgamTop g
  = fst $ sgamPop g
%%]

%%[9 export(sgamPushNew,sgamPushGam)
sgamPushNew :: SGam k v -> SGam k v
sgamPushNew g
 = g {sgScpId = si, sgScp = si : sgScp g}
 where si = sgScpId g + 1

sgamPushGam :: Ord k => SGam k v -> SGam k v -> SGam k v
sgamPushGam g1 g2 = g1 `sgamUnion` sgamPushNew g2
%%]

%%[9 export(sgamLookupDup)
sgamLookupDup :: Ord k => k -> SGam k v -> Maybe [v]
sgamLookupDup k g@(SGam {sgMap = m, sgScpId = scpId, sgScp = scp})
  = case Map.lookup k m of
      Just es | not (null vs)
        -> Just vs
        where vs = sgameltGetFilterInScp scp id es
      _ -> Nothing
%%]

%%[9 export(sgamToAssocDupL,sgamFromAssocDupL)
sgamToAssocDupL :: SGam k v -> AssocL k [v]
sgamToAssocDupL g = Map.toList $ Map.map (map sgeVal) $ sgMap g

sgamFromAssocDupL :: Ord k => AssocL k [v] -> SGam k v
sgamFromAssocDupL l
  = mkSGam m
  where m = Map.map (map (SGamElt 0)) $ Map.fromList l
%%]

%%[9 export(sgamNoDups)
sgamNoDups :: SGam k v -> SGam k v
sgamNoDups g
  = g {sgMap = m}
  where m = Map.map (\(e:_) -> [e]) $ sgMap g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval v => ForceEval (SGamElt v) where
  forceEval x@(SGamElt l v) | forceEval v `seq` True = x
%%[[102
  fevCount (SGamElt l v) = cm1 "SGamElt" `cmUnion` fevCount l `cmUnion` fevCount v
%%]]

instance (ForceEval k, ForceEval v) => ForceEval (SGam k v) where
  forceEval x@(SGam l s m) | forceEval s `seq` forceEval m `seq` True = x
%%[[102
  fevCount (SGam l s m) = cm1 "SGam" `cmUnion` fevCount l `cmUnion` fevCount s `cmUnion` fevCount m
%%]]
%%]


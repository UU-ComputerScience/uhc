module EH101.Gam.ScopeMapGam
( SGam, emptySGam
, sgamFilterMapEltAccumWithKey, sgamMapEltWithKey, sgamMapThr, sgamMap
, sgamSingleton
, sgamUnion
, sgamPartitionEltWithKey, sgamPartitionWithKey
, sgamUnzip
, sgamPop, sgamTop
, sgamPushNew, sgamPushGam
, sgamLookupDup
, sgamToAssocDupL, sgamFromAssocDupL
, sgamNoDups )
where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import EH.Util.Utils
import EH101.Base.Common
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Base.Serialize
import Control.Monad
import EH101.Base.Binary



{-# LINE 59 "src/ehc/Gam/ScopeMapGam.chs" #-}
type Scp = [Int]									-- a stack of scope idents defines what's in scope

data SGamElt v
  = SGamElt
      { sgeScpId	:: !Int							-- scope ident
      , sgeVal		:: v							-- the value
      }
  deriving (Typeable, Data)

type SMap k v = Map.Map k [SGamElt v]

data SGam k v
  = SGam
      { sgScpId		:: !Int							-- current scope, increment with each change in scope
      , sgScp		:: !Scp							-- scope stack
      , sgMap		:: SMap k v						-- map holding the values
      }
  deriving (Typeable, Data)

mkSGam :: SMap k v -> SGam k v
mkSGam = SGam 0 [0]

emptySGam :: SGam k v
emptySGam = mkSGam Map.empty

instance Show (SGam k v) where
  show _ = "SGam"


{-# LINE 94 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- scope ident in scope?
inScp :: Scp -> Int -> Bool
inScp = flip elem

-- sgam elt in scope?
sgameltInScp :: Scp -> SGamElt v -> Bool
sgameltInScp scp = inScp scp . sgeScpId

{-# LINE 104 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- filter out the out of scopes
sgameltFilterInScp :: Scp -> [SGamElt v] -> [SGamElt v]
sgameltFilterInScp scp = filter (sgameltInScp scp)

-- map the in scopes
sgameltMapInScp :: Scp -> (v -> v) -> [SGamElt v] -> [SGamElt v]
sgameltMapInScp scp f = map (\e -> if sgameltInScp scp e then e {sgeVal = f (sgeVal e)} else e)

-- extract the in scopes
sgameltGetFilterInScp :: Scp -> (v -> v') -> [SGamElt v] -> [v']
sgameltGetFilterInScp scp f es = [ f (sgeVal e) | e <- es, sgameltInScp scp e ]

{-# LINE 118 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- filter out the out of scopes, applying a mapping function on the fly
mapFilterInScp' :: Ord k => Scp -> ([SGamElt v] -> [SGamElt v]) -> SMap k v -> SMap k v
mapFilterInScp' scp f m
  = Map.mapMaybe (\es -> maybeNull Nothing (Just . f) $ sgameltFilterInScp scp es) m

mapFilterInScp :: Ord k => Scp -> (SGamElt v -> SGamElt v) -> SMap k v -> SMap k v
mapFilterInScp scp f m
  = mapFilterInScp' scp (map f) m

sgamFilterInScp :: Ord k => SGam k v -> SGam k v
sgamFilterInScp g@(SGam {sgScp = scp, sgMap = m})
  = g {sgMap = mapFilterInScp scp id m}

{-# LINE 133 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- do it all: map, filter, fold
sgamFilterMapEltAccumWithKey
  :: (Ord k')
       => (k -> SGamElt v -> Bool)
          -> (k -> SGamElt v -> acc -> (k',SGamElt v',acc))
          -> (k -> SGamElt v -> acc -> acc)
          -> acc -> SGam k v -> (SGam k' v',acc)
sgamFilterMapEltAccumWithKey p fyes fno a g
  = (g {sgMap = m'},a')
  where (m',a') = Map.foldrWithKey
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

{-# LINE 166 "src/ehc/Gam/ScopeMapGam.chs" #-}
sgamSingleton :: k -> v -> SGam k v
sgamSingleton k v = mkSGam (Map.singleton k [SGamElt 0 v])

{-# LINE 171 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- combine gam, g1 is added to g2 with scope of g2
sgamUnion :: Ord k => SGam k v -> SGam k v -> SGam k v
sgamUnion g1@(SGam {sgScp = scp1, sgMap = m1}) g2@(SGam {sgScp = scp2@(hscp2:_), sgMap = m2})
  = g2 {sgMap = Map.unionWith (++) m1' m2}
  where m1' = mapFilterInScp scp1 (\e -> e {sgeScpId = hscp2}) m1

{-# LINE 179 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- equivalent of partition
sgamPartitionEltWithKey :: Ord k => (k -> SGamElt v -> Bool) -> SGam k v -> (SGam k v,SGam k v)
sgamPartitionEltWithKey p g
  = (g1, SGam (sgScpId g1) (sgScp g1) m2)
  where (g1,m2) = sgamFilterMapEltAccumWithKey p (\k e a -> (k,e,a)) (\k e a -> Map.insertWith (++) k [e] a) Map.empty g

sgamPartitionWithKey :: Ord k => (k -> v -> Bool) -> SGam k v -> (SGam k v,SGam k v)
sgamPartitionWithKey p = sgamPartitionEltWithKey (\k e -> p k (sgeVal e))

{-# LINE 190 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- equivalent of unzip
sgamUnzip :: Ord k => SGam k (v1,v2) -> (SGam k v1,SGam k v2)
sgamUnzip g
  = (g1, g1 {sgMap = m2})
  where (g1,m2) = sgamFilterMapEltAccumWithKey (\_ _ -> True) (\k e@(SGamElt {sgeVal = (v1,v2)}) m -> (k,e {sgeVal = v1},Map.insertWith (++) k [e {sgeVal = v2}] m)) undefined Map.empty g

{-# LINE 198 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- split gam in top and the rest, both with the same scope
sgamPop :: Ord k => SGam k v -> (SGam k v, SGam k v)
sgamPop g@(SGam {sgMap = m, sgScpId = scpId, sgScp = scp@(hscp:tscp)})
  = (SGam scpId [hscp] m, SGam scpId tscp m)
  -- = (sgamFilterInScp $ SGam scpId [hscp] m, sgamFilterInScp $ SGam scpId tscp m)

-- top gam, with same scope as g
sgamTop :: Ord k => SGam k v -> SGam k v
sgamTop g
  = fst $ sgamPop g

{-# LINE 211 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- enter a new scope
sgamPushNew :: SGam k v -> SGam k v
sgamPushNew g
 = g {sgScpId = si, sgScp = si : sgScp g}
 where si = sgScpId g + 1

-- enter a new scope, add g1 in that scope to g2
sgamPushGam :: Ord k => SGam k v -> SGam k v -> SGam k v
sgamPushGam g1 g2 = g1 `sgamUnion` sgamPushNew g2

{-# LINE 223 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- lookup, return at least one found value, otherwise Nothing
sgamLookupDup :: Ord k => k -> SGam k v -> Maybe [v]
sgamLookupDup k g@(SGam {sgMap = m, sgScpId = scpId, sgScp = scp})
  = case Map.lookup k m of
      Just es | not (null vs)
        -> Just vs
        where vs = {- map sgeVal es -- -} sgameltGetFilterInScp scp id es
      _ -> Nothing

{-# LINE 234 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- convert to association list, with all duplicates, scope is lost
sgamToAssocDupL :: Ord k => SGam k v -> AssocL k [v]
sgamToAssocDupL g@(SGam {sgScp = scp, sgMap = m})
  = Map.toList $ Map.map (map sgeVal) $ sgMap $ sgamFilterInScp g

-- convert from association list, assume default scope
sgamFromAssocDupL :: Ord k => AssocL k [v] -> SGam k v
sgamFromAssocDupL l
  = mkSGam m
  where m = Map.map (map (SGamElt 0)) $ Map.fromList l

{-# LINE 247 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- get rid of duplicate entries, by taking the first of them all
sgamNoDups :: Ord k => SGam k v -> SGam k v
sgamNoDups g@(SGam {sgScp = scp, sgMap = m})
  = g {sgMap = mapFilterInScp' scp (\(e:_) -> [e]) m}

{-# LINE 258 "src/ehc/Gam/ScopeMapGam.chs" #-}
instance (Serialize v) => Serialize (SGamElt v) where
  sput (SGamElt a b) = sput a >> sput b
  sget = liftM2 SGamElt sget sget

{-# LINE 264 "src/ehc/Gam/ScopeMapGam.chs" #-}
instance (Ord k, Serialize k, Serialize v) => Serialize (SGam k v) where
  sput (SGam a b c) = sput a >> sput b >> sput c
  sget = liftM3 SGam sget sget sget

{-# LINE 270 "src/ehc/Gam/ScopeMapGam.chs" #-}
-- instance (Binary v) => Serialize (SGamElt v)
-- instance (Ord k, Binary k, Binary v) => Serialize (SGam k v)


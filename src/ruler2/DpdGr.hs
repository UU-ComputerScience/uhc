-------------------------------------------------------------------------
-- Graph for version/view dpd
-------------------------------------------------------------------------

module DpdGr
  ( DpdGr
  , vgTopSort
  , vgVertices
  , vgReachableFrom, vgReachableTo
  , vgDpdsOn
  , vgIsFirst
  , vgCheckSCCMutuals
  , mkScDpdGr, mkVwDpdGr
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Graph
import UU.Pretty
import PPUtils
import Nm
import Err

-------------------------------------------------------------------------
-- DpdGr
-------------------------------------------------------------------------

data DpdGr n
  = DpdGr
      { vgGr    :: Graph
      , vgGrT   :: Graph
      , vgEdges	:: [(n, n, [n])]
      , vgV2N   :: Vertex -> (n, [n])
      , vgK2V   :: n -> Maybe Vertex
      }

emptyVwDpdGr :: DpdGr Nm
emptyVwDpdGr = mkVwDpdGr [] []

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

instance Show (DpdGr n) where
  show _ = "DpdGr"

instance (Ord n,PP n) => PP (DpdGr n) where
  pp g = "DpdGr" >#< ("topsort:" >#< ppCommas (vgTopSort g) >-< "scc:" >#< ppCommaList (vgSCC g) >-< "orig:" >#< (ppCommaList $ map (\(n,_,ns) -> n >|< ":" >|< ppCommaList ns) $ vgEdges $ g))

instance Show (SCC n) where
  show _ = "SCC"

instance PP n => PP (SCC n) where
  pp (AcyclicSCC n ) = "ASCC" >#< n
  pp (CyclicSCC  ns) = "CSCC" >#< ppCommaList ns

-------------------------------------------------------------------------
-- Building from dpds
-------------------------------------------------------------------------

mkDpdGrFromEdges' :: Ord n => [Map.Map n [n]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
mkDpdGrFromEdges' ns
  = (graphFromEdges es,es)
  where cmbChain = Map.unionWith (++)
        mkEdges = map (\(n,ns) -> (n,n,ns)) . Map.toList
        es = mkEdges . foldr cmbChain Map.empty $ ns

mkDpdGrFromEdges :: Ord n => [[(n,[n])]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
mkDpdGrFromEdges
  = mkDpdGrFromEdges' . map Map.fromList

mkDpdGrFromOrder :: Ord n => [[n]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
mkDpdGrFromOrder
  = mkDpdGrFromEdges' . map mkChain
  where mkChain = Map.fromList . fst . foldl (\(c,prev) n -> ((n,prev) : c,[n])) ([],[])

mkDpdGr :: Ord n => ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])]) -> DpdGr n
mkDpdGr ((g,n2,v2),es)
  = DpdGr g (transposeG g) es (\v -> let (n,_,ns) = n2 v in (n,ns)) v2

mkVwDpdGr :: [Nm] -> [[Nm]] -> DpdGr Nm
mkVwDpdGr missing = mkDpdGr . mkDpdGrFromOrder . ([[n] | n <- missing] ++)

mkScDpdGr :: [Nm] -> [(Nm,Nm)] -> DpdGr Nm
mkScDpdGr missing = mkDpdGr . mkDpdGrFromEdges . map (\(n1,n2) -> [(n1,[n2])]) . ([(n,n) | n <- missing] ++)

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

vgVsToNs :: DpdGr n -> [Vertex] -> [n]
vgVsToNs g = map (\v -> fst (vgV2N g v))

-------------------------------------------------------------------------
-- Derived info
-------------------------------------------------------------------------

vgTopSort :: DpdGr n -> [n]
vgTopSort g = vgVsToNs g . topSort . vgGr $ g

vgVertices :: Ord n => DpdGr n -> Set.Set n
vgVertices g = Set.fromList . vgVsToNs g . vertices . vgGr $ g

vgReachable :: Ord n => (DpdGr n -> Graph) -> DpdGr n -> n -> Set.Set n
vgReachable gOf g n
  = case vgK2V g n of
      Just n' -> Set.fromList . vgVsToNs g $ reachable (gOf g) n'
      Nothing -> Set.empty

vgReachableFrom :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableFrom = vgReachable vgGr 

vgReachableTo :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableTo = vgReachable vgGrT 

vgDpdsOn :: DpdGr n -> n -> [n]
vgDpdsOn g n = maybe [] (snd . vgV2N g) (vgK2V g n)

vgIsFirst :: Ord n => DpdGr n -> n -> Set.Set n -> Bool
vgIsFirst g n ns
  = Set.null s
  where s = Set.delete n ns `Set.difference` vgReachableTo g n

-------------------------------------------------------------------------
-- SCC
-------------------------------------------------------------------------

vgSCC :: Ord n => DpdGr n -> [SCC n]
vgSCC g = stronglyConnComp . vgEdges $ g

vgSCCMutuals :: Ord n => DpdGr n -> [[n]]
vgSCCMutuals g = [ ns | (CyclicSCC ns@(_:_:_)) <- vgSCC g ]

vgCheckSCCMutuals :: (Ord n,PP n) => ([PP_Doc] -> Err) -> DpdGr n -> [Err]
vgCheckSCCMutuals mk g
  = if null ns then [] else [mk $ map pp $ concat $ ns]
  where ns = vgSCCMutuals g


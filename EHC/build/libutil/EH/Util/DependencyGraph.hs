-------------------------------------------------------------------------
-- Graph for version/view dpd
-------------------------------------------------------------------------

module EH.Util.DependencyGraph
  ( DpdGr
  , dgTopSort
  , dgVertices
  , dgReachableFrom, dgReachableTo
  , dgDpdsOn
  , dgIsFirst
  , dgCheckSCCMutuals
  , dgSCCToList
  , mkDpdGrFromEdges
  , mkDpdGrFromEdgesMp, mkDpdGrFromEdgesMpPadMissing
  , mkDpdGrFromAssocWithMissing
  , mkDpdGrFromOrderWithMissing
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Graph
import EH.Util.Pretty
-- import EH.Util.Nm
-- import Err

-------------------------------------------------------------------------
-- DpdGr
-------------------------------------------------------------------------

data DpdGr n
  = DpdGr
      { dgGr    :: Graph
      , dgGrT   :: Graph
      , dgEdges	:: [(n, n, [n])]
      , dgV2N   :: Vertex -> (n, [n])
      , dgK2V   :: n -> Maybe Vertex
      }

emptyDpdGr :: Ord n => DpdGr n
emptyDpdGr = mkDpdGrFromOrderWithMissing [] []

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

instance Show (DpdGr n) where
  show _ = "DpdGr"

instance (Ord n,PP n) => PP (DpdGr n) where
  pp g = "DpdGr" >#< ("topsort:" >#< ppCommas (dgTopSort g) >-< "scc   :" >#< ppBracketsCommas (dgSCC g) >-< "edges  :" >#< (ppBracketsCommas $ map (\(n,_,ns) -> n >|< ":" >|< ppBracketsCommas ns) $ dgEdges $ g))

instance Show (SCC n) where
  show _ = "SCC"

instance PP n => PP (SCC n) where
  pp (AcyclicSCC n ) = "ASCC" >#< n
  pp (CyclicSCC  ns) = "CSCC" >#< ppBracketsCommas ns

-------------------------------------------------------------------------
-- Building from dpds
-------------------------------------------------------------------------

dpdGrFromEdgesMp :: Ord n => [Map.Map n [n]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
dpdGrFromEdgesMp ns
  = (graphFromEdges es,es)
  where cmbChain = Map.unionWith (++)
        mkEdges = map (\(n,ns) -> (n,n,ns)) . Map.toList
        es = mkEdges . foldr cmbChain Map.empty $ ns

dpdGrFromEdges :: Ord n => [[(n,[n])]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
dpdGrFromEdges
  = dpdGrFromEdgesMp . map Map.fromList

dpdGrFromOrder :: Ord n => [[n]] -> ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])])
dpdGrFromOrder
  = dpdGrFromEdgesMp . map mkChain
  where mkChain = Map.fromList . fst . foldl (\(c,prev) n -> ((n,prev) : c,[n])) ([],[])

mkDpdGr :: Ord n => ((Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex),[(n, n, [n])]) -> DpdGr n
mkDpdGr ((g,n2,v2),es)
  = DpdGr g (transposeG g) es (\v -> let (n,_,ns) = n2 v in (n,ns)) v2

mkDpdGrFromEdgesMp :: Ord n => Map.Map n [n] -> DpdGr n
mkDpdGrFromEdgesMp
  = mkDpdGr . dpdGrFromEdgesMp . (:[])

mkDpdGrFromEdges :: Ord n => [(n,[n])] -> DpdGr n
mkDpdGrFromEdges
  = mkDpdGr . dpdGrFromEdges . (:[])

mkDpdGrFromEdgesMpWithMissing :: Ord n => [n] -> Map.Map n [n] -> DpdGr n
mkDpdGrFromEdgesMpWithMissing missing
  = mkDpdGrFromEdgesMp
    . (Map.fromList [(n,[n]) | n <- missing] `Map.union`)

mkDpdGrFromEdgesMpPadMissing :: Ord n => Map.Map n [n] -> DpdGr n
mkDpdGrFromEdgesMpPadMissing m
  = mkDpdGrFromEdgesMpWithMissing [ n | ns <- Map.elems m, n <- ns, not (Map.member n m) ] m

mkDpdGrFromOrderWithMissing :: Ord n => [n] -> [[n]] -> DpdGr n
mkDpdGrFromOrderWithMissing missing
  = mkDpdGr . dpdGrFromOrder
    . ([[n] | n <- missing] ++)

mkDpdGrFromAssocWithMissing :: Ord n => [n] -> [(n,n)] -> DpdGr n
mkDpdGrFromAssocWithMissing missing
  = mkDpdGr . dpdGrFromEdges
    . map (\(n1,n2) -> [(n1,[n2])])
    . ([(n,n) | n <- missing] ++)

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

dgVsToNs :: DpdGr n -> [Vertex] -> [n]
dgVsToNs g = map (\v -> fst (dgV2N g v))

-------------------------------------------------------------------------
-- Derived info
-------------------------------------------------------------------------

dgTopSort :: DpdGr n -> [n]
dgTopSort g = dgVsToNs g . topSort . dgGr $ g

dgVertices :: Ord n => DpdGr n -> Set.Set n
dgVertices g = Set.fromList . dgVsToNs g . vertices . dgGr $ g

dgReachable :: Ord n => (DpdGr n -> Graph) -> DpdGr n -> n -> Set.Set n
dgReachable gOf g n
  = case dgK2V g n of
      Just n' -> Set.fromList . dgVsToNs g $ reachable (gOf g) n'
      Nothing -> Set.empty

dgReachableFrom :: Ord n => DpdGr n -> n -> Set.Set n
dgReachableFrom = dgReachable dgGr 

dgReachableTo :: Ord n => DpdGr n -> n -> Set.Set n
dgReachableTo = dgReachable dgGrT 

dgDpdsOn :: DpdGr n -> n -> [n]
dgDpdsOn g n = maybe [] (snd . dgV2N g) (dgK2V g n)

dgIsFirst :: Ord n => DpdGr n -> n -> Set.Set n -> Bool
dgIsFirst g n ns
  = Set.null s
  where s = Set.delete n ns `Set.difference` dgReachableTo g n

-------------------------------------------------------------------------
-- SCC
-------------------------------------------------------------------------

dgSCC :: Ord n => DpdGr n -> [SCC n]
dgSCC g = stronglyConnComp . dgEdges $ g

dgSCCToList :: Ord n => DpdGr n -> [[n]]
dgSCCToList = map (flattenSCC) . dgSCC

dgSCCMutuals :: Ord n => DpdGr n -> [[n]]
dgSCCMutuals g = [ ns | (CyclicSCC ns@(_:_:_)) <- dgSCC g ]

dgCheckSCCMutuals :: (Ord n,PP n) => ([PP_Doc] -> err) -> DpdGr n -> [err]
dgCheckSCCMutuals mk g
  = if null ns then [] else [mk $ map pp $ concat $ ns]
  where ns = dgSCCMutuals g


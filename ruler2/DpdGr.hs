-------------------------------------------------------------------------
-- Graph for version/view dpd
-------------------------------------------------------------------------

module DpdGr
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Graph
import Nm

data DpdGr n
  = DpdGr
      { vgDpd   :: [[n]]
      , vgGr    :: Graph
      , vgGrT   :: Graph
      , vgV2N   :: Vertex -> (n, [n])
      , vgK2V   :: n -> Maybe Vertex
      }

emptyVwDpdGr :: DpdGr Nm
emptyVwDpdGr = mkVwDpdGr [] 

vgVsToNs :: DpdGr n -> [Vertex] -> [n]
vgVsToNs g = map (\v -> fst (vgV2N g v))

mkDpdGr :: Ord n => [[n]] -> (Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex)
mkDpdGr
  = graphFromEdges . mkEdges . foldr cmbChain Map.empty . map mkChain
  where mkChain = Map.fromList . fst . foldl (\(c,prev) n -> ((n,prev) : c,[n])) ([],[])
        cmbChain = Map.unionWith (++)
        mkEdges = map (\(n,ns) -> (n,n,ns)) . Map.toList

mkVwDpdGr :: [[Nm]] -> DpdGr Nm
mkVwDpdGr nLL
  = DpdGr nLL g (transposeG g) (\v -> let (n,_,ns) = n2 v in (n,ns)) v2
  where (g,n2,v2) = mkDpdGr nLL

vgTopSort :: DpdGr n -> [n]
vgTopSort g
  = vgVsToNs g . topSort . vgGr $ g

vgVertices :: Ord n => DpdGr n -> Set.Set n
vgVertices g
  = Set.fromList . vgVsToNs g . vertices . vgGr $ g

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
vgDpdsOn g n
  = maybe [] (snd . vgV2N g) (vgK2V g n)

vgIsFirst :: Ord n => DpdGr n -> n -> Set.Set n -> Bool
vgIsFirst g n ns
  = Set.null s
  where s = Set.delete n ns `Set.difference` vgReachableTo g n


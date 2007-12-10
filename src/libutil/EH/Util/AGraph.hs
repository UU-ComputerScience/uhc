-------------------------------------------------------------------------
-- Interface to inductive graph library, by Gerrit vd Geest
-------------------------------------------------------------------------

module EH.Util.AGraph 
  ( AGraph(agraphGraph)
  , insertEdge
  , insertEdges
  , deleteEdge
  , successors
  , predecessors
  , emptyAGraph
  )
  where

import Data.Graph.Inductive.Graph     (empty, insNodes, gelem, lab, lpre, lsuc, delEdge)
import Data.Graph.Inductive.NodeMap   (NodeMap, new, mkNodes, mkNode_, insMapEdge)
import Data.Graph.Inductive.Tree      (Gr)
import Data.Graph.Inductive.Graphviz  (graphviz')

import Data.Maybe (fromJust)
import Data.List(nub)

data AGraph a b = AGr { agraphNodeMap :: NodeMap a, agraphGraph :: Gr a b}

instance (Show a, Show b) => Show (AGraph a b) where
  show (AGr _ gr) = graphviz' gr

insertEdges :: Ord a => [(a, a, b)] -> AGraph a b -> AGraph a b
insertEdges = flip (foldr insertEdge)

insertEdge :: Ord a => (a, a, b) -> AGraph a b -> AGraph a b
insertEdge e@(p, q, _) gr = let (AGr nm' gr') = insMapNodes (p:[q]) gr
                            in  AGr nm' (insMapEdge nm' e gr')

deleteEdge :: Ord a => (a, a) -> AGraph a b -> AGraph a b
deleteEdge (p, q) (AGr nm gr) = AGr nm (delEdge (getId p, getId q) gr)
  where getId nd = fst $ mkNode_ nm nd

insMapNodes :: Ord a => [a] -> AGraph a b -> AGraph a b
insMapNodes as (AGr m g) =
    let (ns, m') = mkNodes m (nub as)
        ns'      = filter (\(i, _) -> not $ gelem i g) ns
    in AGr m' (insNodes ns' g)

successors, predecessors :: Ord a => AGraph a b -> a -> [(b, a)]
successors   = neighbours lsuc
predecessors = neighbours lpre

emptyAGraph :: Ord a => AGraph a b
emptyAGraph = AGr new empty

neighbours dir (AGr nm gr) node
  | nd `gelem` gr  = map (\(n, info) -> (info, fromJust $ lab gr n)) (dir gr nd)
  | otherwise      = []
  where  nd = fst $ mkNode_ nm node

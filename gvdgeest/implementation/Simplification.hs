{-# OPTIONS -fglasgow-exts #-}
module Simplification
     ( Graph
     , Node (..)
     , emptyAGraph
     , addAssumption
     , addReduction
     , alternatives
     )
where

import qualified Data.Set as Set

import Constraints
import AGraph
import Heuristics (Red (..), Alts (..))

data Node p  =  Pred p
             |  And [p]
             deriving (Eq, Ord)

instance Show p => Show (Node p) where
  show (Pred p)    = show p
  show (And [])    = "True"
  show (And _ )    = "And"

true  ::  Node p
true  =   And []

type Graph p info = AGraph (Node p) info
    
------------------------------------------------------------------
-- Constructing a graph from Reduction constraints
------------------------------------------------------------------

addAssumption :: Ord p => Constraint p info -> [info] -> Graph p info -> Graph p info
addAssumption (Assume  p)  is  = insertEdges (zip3 (repeat (Pred p)) (repeat true) is) 
addAssumption _            _   = id

addReduction :: Ord p => Constraint p info -> Graph p info -> Graph p info
addReduction (Reduction p i [q])  =  insertEdge (Pred p, Pred q  , i)
addReduction (Reduction p i ps)   =  let  andNd  = And ps
                                          edges  = map (\q -> (andNd, Pred q, i)) ps
                                     in   insertEdges ((Pred p, andNd, i) : edges)
addReduction _                    =  id

------------------------------------------------------------------
-- Generating alternatives from a reduction graph
------------------------------------------------------------------
alternatives :: Ord p => Graph p info -> p -> Alts p info
alternatives gr = recOr
  where  recOr   p       = Alts  p  (map recAnd  (successors gr (Pred p))) 
         recAnd  (i, n)  = Red   i  (map recOr   (preds n))
         preds  n  =   case n of
                       Pred  q   -> [q]
                       And   qs  -> qs

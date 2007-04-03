module GraphSolver ( constructGraph
                   , constructGraphT
                   , RuleEnv
                   , Rule) where

import Data.Map (member)
import Data.Graph.Inductive.Graph   (DynGraph, empty, insNode, insEdge, gelem)
import Data.Graph.Inductive.NodeMap (NodeMap, new, mkNode, mkEdge)
import Data.Graph.Inductive.Tree    (Gr)


-----------------------------------------------------------------------------
-- GvdG: Code for building a graph from a set of rules
-- Type variable a is the type of the constraint
-- Type variable b is the type of the edge-annotation 
-----------------------------------------------------------------------------

type RuleEnv  a b = (a -> [Rule a b])
type RGraph g a b = (g a b, NodeMap a)
type Rule     a b = (a, a, b)

constructGraphT :: Ord a => RuleEnv a b -> [a] -> RGraph Gr a b
constructGraphT = constructGraph

constructGraph :: (Ord a, DynGraph g) => RuleEnv a b -> [a] -> RGraph g a b
constructGraph = addNodes (empty, new)

addNodes :: (DynGraph g, Ord a) => RGraph g a b -> RuleEnv a b -> [a] -> RGraph g a b
addNodes gr env = foldr (addNode env) gr

addNode :: (DynGraph g, Ord a) => RuleEnv a b -> a       -> RGraph g a b -> RGraph g a b
addNode env c rgr@(gr, nm) 
  | fst node `gelem` gr = rgr
  | otherwise = foldr (addEdge env) (gr', nm') (env c)
  where (node, nm') = mkNode nm c 
        gr'         = insNode node gr

addEdge :: (DynGraph g, Ord a) => RuleEnv a b -> Rule a b -> RGraph g a b -> RGraph g a b
addEdge env rule@(x, y, _) rgr
  = (insEdge edge gr, nm)
  where (gr, nm)    = addNodes rgr env (x:y:[])
        (Just edge) = mkEdge nm rule

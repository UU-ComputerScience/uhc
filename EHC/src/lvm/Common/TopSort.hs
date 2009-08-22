{------------------------------------------------------------------------
 The Core Assembler.

 Modified and adapted by Daan Leijen

 Original version by Arjan van IJzendoorn

 7 december 2000:
 changed module name (originally Ldfs)
 changed export list
 changed Vertex to Int
 included is some additional code for printing tree structures ...
------------------------------------------------------------------------}

----------------------------------------------------------------
-- A version of the graph algorithms described in:
--
-- ``Lazy Depth-First Search and Linear Graph Algorithms in Haskell''
--   by David King and John Launchbury
--
-- Original version as "Ldfs.hs" in the Hugs98 distribution
--  (by David King and John Launchbury ??)
----------------------------------------------------------------

--  $Id: TopSort.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Common.TopSort( topSort ) where

{-
        ( Graph, Table, Vertex, Tree(Node), Forest
        , topSort
        , scc
        , buildG
        ) where
-}

import Array
import List
import Lvm.Common.Special( ST,STArray, runST, newSTArray, readSTArray, writeSTArray)

type Vertex  = Int



-- Representing graphs:

type Table a = Array Vertex a
type Graph   = Table [Vertex]

vertices :: Graph -> [Vertex]
vertices  = indices

type Edge = (Vertex, Vertex)

edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (v, f v (t!v)) | v <- indices t ]

type Bounds = (Vertex, Vertex)

outdegree :: Graph -> Table Int
outdegree  = mapT numEdges
             where numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG  = accumArray (flip (:)) []

{-
graph = buildG ('a','j')
         (reverse
          [ ('a', 'b'),  ('a', 'f'),  ('b', 'c'),
            ('b', 'e'),  ('c', 'a'),  ('c', 'd'),
            ('e', 'd'),  ('g', 'h'),  ('g', 'j'),
            ('h', 'f'),  ('h', 'i'),  ('h', 'j') ]
         )
-}

transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

indegree :: Graph -> Table Int
indegree  = outdegree . transposeG


-- Depth-first search

-- Specification and implementation of depth-first search:

data Tree a   = Node a (Forest a) deriving Show
type Forest a = [Tree a]

dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

type Set s    = STArray s Vertex Bool

mkEmpty      :: Bounds -> ST s (Set s)
mkEmpty bnds  = newSTArray bnds False

contains     :: Set s -> Vertex -> ST s Bool
contains m v  = readSTArray m v

include      :: Set s -> Vertex -> ST s ()
include m v   = writeSTArray m v True

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds >>= \m ->
                       chop m ts)

chop         :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m []     = return []
chop m (Node v ts : us)
              = contains m v >>= \visited ->
                if visited then
                  chop m us
                else
                  include m v >>= \_  ->
                  chop m ts   >>= \as ->
                  chop m us   >>= \bs ->
                  return (Node v as : bs)

-- Depth-first search algorithms

-- Algorithm 1: depth first search numbering

preorder            :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF           :: Forest a -> [a]
preorderF ts         = concat (map preorder ts)

preOrd :: Graph -> [Vertex]
preOrd  = preorderF . dff

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF

-- Algorithm 2: topological sorting

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF   :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd      :: Graph -> [Vertex]
postOrd       = postorderF . dff

topSortG     :: Graph -> [Vertex]
topSortG     = reverse . postOrd

-- Algorithm 3: connected components

components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

scc          :: Graph -> Forest Vertex
scc g         = dfs (transposeG g) (reverse (postOrd g))

scc'         :: Graph -> Forest Vertex
scc' g        = dfs g (reverse (postOrd (transposeG g)))

-- Algorithm 5: Classifying edges

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v rs) = [ (v, w) | Node w us <- ts ] ++
                          concat (map flat ts)

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree!v

-- Algorithm 6: Finding reachable vertices

reachable    :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

path         :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

-- Algorithm 7: Biconnected components

bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (u,du,lu) xs <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,dv,lv) ts)
      = [ Node (v:vs) us | (l,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws us) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]

{-
figure4 = buildG ('a','i') (vs ++ reverse [ (v, w) | (w, v) <- vs ])
          where vs = [ ('b', 'a'), ('e', 'a'), ('c', 'b'),
                       ('d', 'c'), ('b', 'd'), ('f', 'e'),
                       ('h', 'e'), ('g', 'f'), ('e', 'g'),
                       ('i', 'h'), ('a', 'i'), ('h', 'a') ]

figure5 = showForest (map (label figure4 dnum) f)
          where f    = dff figure4
                dnum = preArr (bounds figure4) f

figure7 = showForest (bcc figure4)
-}

----------------------------------------------------------------
-- topological sort:
-- takes a maximal vertex and a list depencies,
-- where (x,y) means x depends on y.
--
-- Graph> topsort 3 [(0,1),(1,2),(1,1),(2,1),(1,3)]
-- [[3],[1,2],[0]]
----------------------------------------------------------------
topSort :: Vertex -> [Edge] -> [[Vertex]]
topSort max edges
  = let graph = buildG (0, max) edges
        forest = scc graph
    in reverse (flattenForest forest)

flattenForest :: Forest a -> [[a]]
flattenForest trees
  = map flattenTree trees

flattenTree :: Tree a -> [a]
flattenTree (Node x trees)  = x : concatMap flattenTree trees

----------------------------------------------------------------
-- Utility functions for drawing trees and forests:

showTree :: Show a => Tree a -> String
showTree  = drawTree . mapTree show

showForest :: Show a => Forest a -> String
showForest  = unlines . map showTree

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

drawTree        :: Tree String -> String
drawTree         = unlines . draw

draw (Node x ts) = grp this (space (length this)) (stLoop ts)
 where this          = s1 ++ x ++ " "

       space n       = take n (repeat ' ')

       stLoop []     = [""]
       stLoop [t]    = grp s2 "  " (draw t)
       stLoop (t:ts) = grp s3 s4 (draw t) ++ [s4] ++ rsLoop ts

       rsLoop [t]    = grp s5 "  " (draw t)
       rsLoop (t:ts) = grp s6 s4 (draw t) ++ [s4] ++ rsLoop ts

       grp fst rst   = zipWith (++) (fst:repeat rst)

       [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]

------------------------------------------------------------------------------

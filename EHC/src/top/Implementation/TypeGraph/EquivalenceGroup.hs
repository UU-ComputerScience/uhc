-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- An equivalence group is a graph-like structure containing type variables and 
-- type constants that should all be equivalent. The edges explain why they should
-- be equal.
--
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.EquivalenceGroup 
   ( EquivalenceGroup 
   , emptyGroup, insertVertex, insertEdge, insertClique, combineGroups
   , vertices, constants, edges, equalPaths
   , removeEdge, removeClique, splitGroup
   , typeOfGroup, consistent, checkGroup
   ) where
   
import Top.Implementation.TypeGraph.Path
import Top.Implementation.TypeGraph.Basics
import Top.Types
import Data.List
import qualified Data.Set as S

-----------------------------------------------------------------------
-- * Representation of an equivalence group

data EquivalenceGroup info = 
   EQGroup { vertices :: [(VertexId, VertexInfo)]  -- ^ vertices in this equivalence group
           , edges    :: [(EdgeId, info)]          -- ^ (initial) edges in this equivalence group
           , cliques  :: [Clique]                  -- ^ (implied) cliques in this equivalence group
           }

-- first sort the items of an equivalence group
instance Show (EquivalenceGroup info) where
   show eqgroup = 
      unlines $ 
              [ "[Vertices]:"
              , "   " ++ concatMap show (sort (vertices eqgroup))
              , "[Edges]:"
              , "   " ++ concatMap show (sort [ a | (a, _) <- edges eqgroup ])
              , "[Cliques  ]:"
              ] ++ 
              (map (("   "++). show) (sort  (cliques eqgroup)))

-----------------------------------------------------------------------
-- * Constructing an equivalence group

emptyGroup :: EquivalenceGroup info
emptyGroup = 
   EQGroup { vertices = [], edges = [], cliques = [] }
	   
insertVertex :: VertexId -> VertexInfo -> EquivalenceGroup info -> EquivalenceGroup info
insertVertex i info eqgroup = 
   eqgroup { vertices = (i, info) : vertices eqgroup }  

insertEdge :: EdgeId -> info -> EquivalenceGroup info -> EquivalenceGroup info
insertEdge edge info eqgroup = 
   eqgroup { edges = (edge, info) : edges eqgroup }  
   
insertClique :: Clique -> EquivalenceGroup info -> EquivalenceGroup info 
insertClique clique eqgroup =
   eqgroup { cliques = newCliques }
      
 where
   newCliques = mergeCliques (clique : cs2) : cs1
   (cs1, cs2) = partition (isDisjointClique clique) (cliques eqgroup)
	    
   {- msg = unlines [ "------------------insert clique -------------------------"
                 , show eqgroup
                 , "---- new cliques ----"
                 , show newCliques
                 ] -}

combineGroups :: EquivalenceGroup info -> EquivalenceGroup info -> EquivalenceGroup info
combineGroups eqgroup1 eqgroup2 = 
   EQGroup { vertices = vertices eqgroup1 ++ vertices eqgroup2
           , edges    = edges    eqgroup1 ++ edges    eqgroup2
           , cliques  = cliques  eqgroup1 `combineCliqueList` cliques  eqgroup2
           }
	   
----------------------------------------------------------------------
-- * Removing parts from an equivalence group

removeEdge :: EdgeId -> EquivalenceGroup info -> EquivalenceGroup info
removeEdge edge eqgroup =
   let p (e, _) = edge /= e
   in eqgroup { edges = filter p (edges eqgroup) }

removeClique :: Clique -> EquivalenceGroup info -> EquivalenceGroup info
removeClique clique eqgroup =
   eqgroup { cliques = filter (not . (`isSubsetClique` clique)) (cliques eqgroup) }
             
splitGroup :: EquivalenceGroup info -> [EquivalenceGroup info]
splitGroup eqgroup = 
   let (vs, es, cs) = (vertices eqgroup, edges eqgroup, cliques eqgroup)
       eqcs = map (\(a, b) -> insertVertex a b emptyGroup) vs

       addClique clique groups = 
          let is         = childrenInClique clique
              (gs1, gs2) = partition (any ((`elem` is) . fst) . vertices) groups    
          in insertClique clique (foldr combineGroups emptyGroup gs1) : gs2

       addEdge (edge@(EdgeId v1 v2 _), info) groups =
          let is         = [v1, v2] 
              (gs1, gs2) = partition (any ((`elem` is) . fst) . vertices) groups
          in insertEdge edge info (foldr combineGroups emptyGroup gs1) : gs2

   in foldr addEdge (foldr addClique eqcs cs) es

----------------------------------------------------------------------
-- * Interrogating an equivalence group

constants :: EquivalenceGroup info -> [String]
constants eqgroup = 
   nub [ s | (_, (VCon s, _)) <- vertices eqgroup ]
   
consistent :: EquivalenceGroup info -> Bool
consistent eqgroup = 
   case constants eqgroup of
      []  -> True
      [_] -> null [ () | (_, (VApp _ _, _)) <- vertices eqgroup ]
      _   -> False
       
equalPaths  :: S.Set VertexId -> VertexId -> [VertexId] -> EquivalenceGroup info -> TypeGraphPath info
equalPaths without start targets eqgroup =
   reduceNumberOfPaths $
      tailSharingBy (\(e1, _) (e2, _) -> e1 `compare` e2) $
      rec start (edgeList, cliqueList)
 where   
      -- msg        = "Path from "++show start++" to "++show targets++" without "++show (S.elems without)
      edgeList   = let p (EdgeId v1 v2 _, _) = 
                          not (v1 `S.member` without) && not (v2 `S.member` without)
                   in filter p (edges eqgroup)
      cliqueList = let f = filter (not . (`S.member` without) . child) . triplesInClique
                   in map f (cliques eqgroup)
      targetSet  = S.fromList targets
      
      -- Allow a second visit of a clique in a path?
      secondCliqueVisit = False
      
      rec :: VertexId -> ([(EdgeId, info)], [[ParentChild]]) -> TypeGraphPath info
      rec v1 (es, cs)
        | v1 `S.member` targetSet  = Empty
        | otherwise =
             let (edges1,es' ) = partition (\(EdgeId a _ _, _) -> v1 == a) es
                 (edges2,es'') = partition (\(EdgeId _ a _, _) -> v1 == a) es'
                 (neighbourCliques, otherCliques) = 
                    partition ((v1 `elem`) . map child) cs 
                 rest@(_, restCliques)
                    | secondCliqueVisit = (es'', removeFromClique v1 neighbourCliques ++ otherCliques)
                    | otherwise         = (es'', otherCliques)
             in 
                altList $ 
                map (\(EdgeId _ neighbour edgeNr, info) -> 
                      Step (EdgeId v1 neighbour edgeNr, Initial info) 
                      :+: rec neighbour rest) edges1
             ++ map (\(EdgeId neighbour _ edgeNr, info) -> 
                      Step (EdgeId v1 neighbour edgeNr, Initial info) 
                      :+: rec neighbour rest) edges2
             ++ concatMap (\list ->
                           let (sources, others) = partition ((v1==) . child) list
                               sourceParents     = map parent sources
                               neighbours        = nub (map child others)
                               f neighbour       = altList 
                                  [ beginPath :+: restPath
                                  | pc <- others
                                  , child pc == neighbour
                                  , let beginPath = altList1 (map g sourceParents)
                                        restPath   
                                           | secondCliqueVisit = rec neighbour (es'', map (filter (/= pc)) restCliques)
                                           | otherwise         = rec neighbour rest
                                        g sp = Step ( EdgeId v1 neighbour impliedEdgeNr
                                                    , Implied (childSide pc) sp (parent pc)
                                                    )
                                  ]
                           in if null sources 
                                then []
                                else map f neighbours) neighbourCliques
				
      removeFromClique :: VertexId -> [[ParentChild]] -> [[ParentChild]]
      removeFromClique vid =
         let p pcs = length pcs > 1
             f pcs = filter ((/=vid) . child) pcs
         in filter p . map f

typeOfGroup :: OrderedTypeSynonyms -> EquivalenceGroup info -> Maybe Tp
typeOfGroup synonyms eqgroup

   | length allConstants > 1                           =  Nothing
   | not (null allConstants) && not (null allApplies)  =  Nothing
   
   | not (null allOriginals)  =  Just (theBestType synonyms allOriginals)
   | not (null allConstants)  =  Just (TCon (head allConstants))
   | not (null allApplies)    =  Just $  let (VertexId  l, VertexId r) = head allApplies
                                         in (TApp (TVar l) (TVar r)) 
   | otherwise                =  Just (TVar (head allVariables))
   
  where
    allVariables  =       [ i       |  (VertexId i, _)     <- vertices eqgroup  ]
    allConstants  =  nub  [ s       |  (_, (VCon s, _))    <- vertices eqgroup  ]
    allApplies    =       [ (l, r)  |  (_, (VApp l r, _))  <- vertices eqgroup  ]       
    allOriginals  =       [ tp      |  (_, (_, Just tp))   <- vertices eqgroup  ]

-- If I cannot select a best type at this point, an arbitary type is returned.
-- This is because I cannot see "inside" the types 
-- Todo: improve
theBestType :: OrderedTypeSynonyms -> Tps -> Tp 
theBestType synonyms tps = 
   let f t1 t2 = case equalUnderTypeSynonyms synonyms t1 t2 of
                    Just best -> best
                    Nothing -> t1
   in foldr1 f tps
   
-- Check for some invariants: identity if everything is okay, otherwise an internal error
checkGroup :: EquivalenceGroup info -> EquivalenceGroup info
checkGroup = test c2 . test c1 where 
   test p eqGroup = if p eqGroup then error "Check failed for equivalence group" else eqGroup 
   c1 eqGroup = hasDouble (concatMap triplesInClique $ cliques eqGroup) 
   c2 eqGroup = any ((<2) . length . triplesInClique) (cliques eqGroup)
   hasDouble [] = False
   hasDouble (x:xs) = x `elem` xs || hasDouble xs
{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.Class where

import Top.Types
import Top.Implementation.TypeGraph.Basics
import qualified Data.Set as S
import Data.List (nub)
import Utils (internalError)

class TypeGraph graph info | graph -> info where          
   
   -- construct a type graph
   addTermGraph :: OrderedTypeSynonyms -> Int -> Tp -> graph -> (Int, VertexId, graph)
   addVertex    :: VertexId -> VertexInfo -> graph -> graph
   addEdge      :: EdgeId -> info -> graph -> graph
   addNewEdge   :: (VertexId, VertexId) -> info -> graph -> graph

   -- deconstruct a type graph
   deleteEdge :: EdgeId -> graph -> graph

   -- inspect an equivalence group in a type graph
   verticesInGroupOf       :: VertexId -> graph -> [(VertexId, VertexInfo)]
   childrenInGroupOf       :: VertexId -> graph -> ([ParentChild], [ParentChild])
   constantsInGroupOf      :: VertexId -> graph -> [String]
   representativeInGroupOf :: VertexId -> graph -> VertexId
   edgesFrom               :: VertexId -> graph -> [(EdgeId, info)]

   -- query a path in an equivalence group
   allPaths            :: VertexId -> VertexId -> graph -> TypeGraphPath info
   allPathsList        :: VertexId -> [VertexId] -> graph -> TypeGraphPath info
   allPathsListWithout :: S.Set VertexId -> VertexId -> [VertexId] -> graph -> TypeGraphPath info 
     
   -- substitution and term graph
   substituteVariable :: OrderedTypeSynonyms -> Int -> graph -> Tp
   substituteType     :: OrderedTypeSynonyms -> Tp  -> graph -> Tp
   substituteTypeSafe :: OrderedTypeSynonyms -> Tp  -> graph -> Maybe Tp
   makeSubstitution   :: OrderedTypeSynonyms -> graph -> [(VertexId, Tp)]
   typeFromTermGraph  :: VertexId -> graph -> Tp
   
   -- Extra administration
   markAsPossibleError     :: VertexId -> graph -> graph
   getMarkedPossibleErrors :: graph -> [VertexId]
   unmarkPossibleErrors    :: graph -> graph

   -------------------------------------------
   -- default definitions   
   
   allPaths i1 i2 = 
      allPathsList i1 [i2]

   allPathsList =
      allPathsListWithout S.empty
      
   childrenInGroupOf i graph =
      unzip [ ( ParentChild { parent=p, child = t1, childSide=LeftChild  }
              , ParentChild { parent=p, child = t2, childSide=RightChild } 
              ) 
            | (p, (VApp t1 t2, _)) <- verticesInGroupOf i graph 
            ]
          
   constantsInGroupOf i graph =
      nub [ s | (_, (VCon s, _)) <- verticesInGroupOf i graph ]
   
   representativeInGroupOf i graph =
      case verticesInGroupOf i graph of 
         (vid, _):_ -> vid
         _ -> internalError "Top.TypeGraph.TypeGraphState" "representativeInGroupOf" "unexpected empty equivalence group"
            
   substituteVariable syns =
      substituteType syns . TVar
      
   substituteType syns tp graph =
      case substituteTypeSafe syns tp graph of
         Just stp -> stp
         Nothing  -> internalError "Top.TypeGraph.TypeGraphState" "substituteType" "inconsistent state"
         
   -- Extra administration
   markAsPossibleError _     = id
   getMarkedPossibleErrors _ = []
   unmarkPossibleErrors      = id
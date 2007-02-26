{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.Standard where

import Top.Implementation.TypeGraph.Basics
import Top.Implementation.TypeGraph.EquivalenceGroup
import Top.Implementation.TypeGraph.Class
import Top.Implementation.General
import Top.Types
import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (isJust)
import Utils (internalError)

data StandardTypeGraph info = STG
   { referenceMap            :: M.Map VertexId Int{- group number -}
   , equivalenceGroupMap     :: M.Map Int (EquivalenceGroup info)
   , equivalenceGroupCounter :: Int
   , possibleErrors          :: [VertexId]
   , constraintNumber        :: Int
   }

instance Show info => Empty (StandardTypeGraph info) where
   empty = STG
      { referenceMap            = M.empty
      , equivalenceGroupMap     = M.empty
      , equivalenceGroupCounter = 0
      , possibleErrors          = []
      , constraintNumber        = 0
      }

instance Show (StandardTypeGraph info) where
   show stg = 
      "(Type graph consists of " ++ show (M.size (equivalenceGroupMap stg)) ++ " equivalence groups)"
  
instance TypeGraph (StandardTypeGraph info) info where

   addTermGraph synonyms = rec 
    where 
      rec unique tp stg = 
         let (newtp, original) = 
                case expandToplevelTC synonyms tp of
                   Nothing -> (tp, Nothing) 
                   Just x  -> (x, Just tp)
         in case newtp of
               TVar i ->
                  let vid = VertexId i 
                  in (unique, vid, if vertexExists vid stg then stg else addVertex vid (VVar, original) stg)
               TCon s -> 
                  let vid = VertexId unique
                  in (unique+1, vid, addVertex vid (VCon s, original) stg)
               TApp t1 t2 -> 
                  let (u1, v1, g1) = rec unique t1 stg
                      (u2, v2, g2) = rec u1     t2 g1 
                      vid = VertexId u2
                  in (u2+1, vid, addVertex vid (VApp v1 v2, original) g2)
   
   addVertex v info =
      createGroup (insertVertex v info emptyGroup)
   
   addEdge edge@(EdgeId v1 v2 _) info =
      propagateEquality v1 . updateGroupOf v1 (insertEdge edge info) . combineClasses [v1, v2] 

   addNewEdge (v1, v2) info stg =
      let cnr = makeEdgeNr (constraintNumber stg)
      in addEdge (EdgeId v1 v2 cnr) info (stg { constraintNumber = constraintNumber stg + 1})   
   
   deleteEdge edge@(EdgeId v1 _ _) =
      propagateRemoval v1 . updateGroupOf v1 (removeEdge edge)
   
   verticesInGroupOf i = 
      vertices . getGroupOf i
      
   substituteTypeSafe synonyms =
      let rec history (TVar i) stg
            |  i `elem` history  = Nothing
            |  otherwise         =
                  case maybeGetGroupOf (VertexId i) stg of
                     Nothing ->
                        Just (TVar i)
                     Just eqnr -> 
                        do newtp <- typeOfGroup synonyms (getGroupOf (VertexId i) stg)
                           case newtp of 
                              TVar j -> Just (TVar j)
                              _      -> rec (i:history) newtp stg
          
          rec _ tp@(TCon _) _ = Just tp
          
          rec history (TApp l r) stg =
             do l' <- rec history l stg
                r' <- rec history r stg
                Just (TApp l' r')
       in rec []
    
   edgesFrom i =
      let p (EdgeId v1 v2 _, _) = v1 == i || v2 == i
      in filter p . edges . getGroupOf i
   
   allPathsListWithout without v1 vs = 
      equalPaths without v1 vs . getGroupOf v1

   makeSubstitution syns stg = 
      let f eqgroup =
             case typeOfGroup syns eqgroup of 
                Just tp -> [ (vid, tp) | (vid@(VertexId i), _) <- vertices eqgroup, notId i tp ]
                Nothing -> internalError "Top.TypeGraph.Implementation" "makeSubstitution" "inconsistent equivalence group"
          notId i (TVar j) = i /= j
          notId _ _        = True
      in concatMap f (getAllGroups stg)
   
   typeFromTermGraph vid stg =
      case [ tp | (x, (tp, _)) <- verticesInGroupOf vid stg, vid == x ] of
         [VCon s]   -> TCon s
         [VApp a b] -> TApp (typeFromTermGraph a stg) (typeFromTermGraph b stg)
         _          -> vertexIdToTp vid
   
   markAsPossibleError     = 
      addPossibleInconsistentGroup
   
   getMarkedPossibleErrors = 
      getPossibleInconsistentGroups
   
   unmarkPossibleErrors = 
      setPossibleInconsistentGroups []

-- Helper functions
combineClasses :: [VertexId] -> StandardTypeGraph info -> StandardTypeGraph info
combineClasses is stg =
      case nub (map (flip representativeInGroupOf stg) is) of
         list@(i:_:_) ->
            let eqgroups = map (flip getGroupOf stg) list
                newGroup = foldr combineGroups emptyGroup eqgroups
            in addPossibleInconsistentGroup i . createGroup newGroup . foldr removeGroup stg $ eqgroups
         _ -> stg

propagateEquality :: VertexId -> StandardTypeGraph info -> StandardTypeGraph info
propagateEquality vid stg = 
   let (listLeft, listRight) = childrenInGroupOf vid stg
       left  = map (flip representativeInGroupOf stg . child) listLeft
       right = map (flip representativeInGroupOf stg . child) listRight
   in (if length (nub right) > 1
         then propagateEquality (head right)
         else id)
    . (if length (nub left) > 1
         then propagateEquality (head left) 
         else id)
    . (if length listLeft > 1   
         then addClique (makeClique listRight) . addClique (makeClique listLeft) 
         else id)
    $ stg   

addClique :: Clique -> StandardTypeGraph info -> StandardTypeGraph info
addClique clique =
   updateGroupOf (cliqueRepresentative clique) (insertClique clique) . combineClasses (childrenInClique clique)

propagateRemoval :: VertexId -> StandardTypeGraph info -> StandardTypeGraph info
propagateRemoval i stg = 
   let (is, new) = splitClass i stg   
       ts = map (flip childrenInGroupOf new) is

       (leftList, rightList) = unzip ts
       cliqueLeft  = makeClique (concat leftList)
       cliqueRight = makeClique (concat rightList)
       newCliques  = [ makeClique list | list <- leftList ++ rightList, length list > 1 ] 
       children    = [ child pc | pc:_ <- leftList ++ rightList ]  
   in 
      if length (filter (not . null) leftList) > 1 
        then flip (foldr propagateRemoval) children
           . flip (foldr addClique) newCliques
           . deleteClique cliqueRight
           . deleteClique cliqueLeft
           $ new
        else new
    
splitClass ::  VertexId -> StandardTypeGraph info -> ([VertexId], StandardTypeGraph info)
splitClass vid stg = 
   let eqgroup   = getGroupOf vid stg  
       newGroups = splitGroup eqgroup
       results   = [ vid | (vid, _):_ <- map vertices newGroups ]
       newGraph  
          | length newGroups > 1 = foldr createGroup (removeGroup eqgroup stg) newGroups
          | otherwise = stg
   in (results, newGraph)
      
deleteClique :: Clique -> StandardTypeGraph info -> StandardTypeGraph info
deleteClique clique = 
   updateGroupOf (cliqueRepresentative clique) (removeClique clique)
      
-----------------------------------------------------------------

createGroup :: EquivalenceGroup info -> StandardTypeGraph info -> StandardTypeGraph info
createGroup eqgroup stg =
   let newGroupNumber = equivalenceGroupCounter stg
       list = [(i, newGroupNumber) | (i, _) <- vertices eqgroup ]
   in if null list 
        then internalError "Top.TypeGraph.TypeGraphMonad" "createNewGroup" "cannot create an empty equivalence group"
        else stg { referenceMap            = M.union (referenceMap stg) (M.fromList list)
                 , equivalenceGroupMap     = M.insert newGroupNumber eqgroup (equivalenceGroupMap stg)
                 , equivalenceGroupCounter = newGroupNumber + 1
                 }

removeGroup :: EquivalenceGroup info -> StandardTypeGraph info -> StandardTypeGraph info               
removeGroup eqgroup stg =
   let vertexIds   = map fst (vertices eqgroup)
       oldGroupNr  = maybe [] (:[]) (M.lookup (head vertexIds) (referenceMap stg))
   in stg { referenceMap        = foldr M.delete (referenceMap stg) vertexIds
          , equivalenceGroupMap = foldr M.delete (equivalenceGroupMap stg) oldGroupNr
          }
          
updateGroupOf :: VertexId -> (EquivalenceGroup info -> EquivalenceGroup info) -> StandardTypeGraph info -> StandardTypeGraph info
updateGroupOf vid f stg =
   let eqgrp = getGroupOf vid stg
       err  = internalError "Top.TypeGraph.TypeGraphMonad" "updateEquivalenceGroupOf" ("error in lookup map: "++show vid)
       eqnr = M.findWithDefault err vid (referenceMap stg)
   in stg { equivalenceGroupMap = M.insert eqnr (f eqgrp) (equivalenceGroupMap stg) }

maybeGetGroupOf :: VertexId -> StandardTypeGraph info -> Maybe (EquivalenceGroup info)
maybeGetGroupOf vid stg = 
   do eqnr <- M.lookup vid (referenceMap stg)
      let err = internalError "Top.TypeGraph.TypeGraphMonad" "equivalenceGroupOf" "error in lookup map"
      return (M.findWithDefault err eqnr (equivalenceGroupMap stg))

getGroupOf :: VertexId -> StandardTypeGraph info -> EquivalenceGroup info                
getGroupOf vid =
   let err = internalError "Top.TypeGraph.Standard" "getGroupOf" "the function getGroupOf does no longer create an empty group if the vertexId doesn't exist"
   in maybe err id . maybeGetGroupOf vid

getAllGroups :: StandardTypeGraph info -> [EquivalenceGroup info]
getAllGroups = M.elems . equivalenceGroupMap

vertexExists :: VertexId -> StandardTypeGraph info -> Bool
vertexExists vid = isJust . M.lookup vid . referenceMap

-----------------------------------------------------------------------------------

getPossibleInconsistentGroups :: StandardTypeGraph info -> [VertexId]
getPossibleInconsistentGroups = possibleErrors

setPossibleInconsistentGroups :: [VertexId] -> StandardTypeGraph info -> StandardTypeGraph info
setPossibleInconsistentGroups vids stg = stg { possibleErrors = vids }
      
addPossibleInconsistentGroup :: VertexId -> StandardTypeGraph info -> StandardTypeGraph info
addPossibleInconsistentGroup vid stg = stg { possibleErrors = vid : possibleErrors stg }

--------------------------------------------------------------------------------
{-
setHeuristics :: [Heuristic info] -> StandardTypeGraph info -> StandardTypeGraph info
setHeuristics = setPathHeuristics . const 

setPathHeuristics :: (Path (EdgeId, info) -> [Heuristic info]) -> StandardTypeGraph info -> StandardTypeGraph info
setPathHeuristics f stg = stg {typegraphHeuristics = f}
   
getPathHeuristics :: StandardTypeGraph info -> Path (EdgeId, info) -> [Heuristic info]
getPathHeuristics = typegraphHeuristics -}
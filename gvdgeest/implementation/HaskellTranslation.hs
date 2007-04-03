module HaskellTranslation
( ClassDecl
, InstanceDecl
, genChrs)
where

import CHRSolver
import AGraph
import Simplification
import Constraints

import Data.Maybe (mapMaybe)

-----------------------------------------------------------------------------
-- Functions to generate chrs for a list of class declarations
-- and to check the acyclicity of the class hiearchy.
-----------------------------------------------------------------------------
type ClassDecl    a info = ([a], a, [info])
type InstanceDecl a info = ([a], a, info)

genChrs :: (Matchable a s, Ord a, Ord info) => [ClassDecl a info] -> [InstanceDecl a info] -> [Rule a s info]
genChrs classes insts =
  let classChrs = genClassChrs classes
      instChrs  = genInstanceChrs insts
  in  classChrs ++ instChrs

genClassChrs :: (Matchable a s, Ord a, Ord info) => [ClassDecl a info] -> [Rule a s info]
genClassChrs clsDecls =
  let assumeChrs = mapMaybe genAssumeChrs clsDecls
      simplChrs  = concatMap (genClassSimplChrs' assumeChrs) clsDecls
  in assumeChrs ++ simplChrs

genAssumeChrs :: (Matchable a s, Ord info) => ClassDecl a info -> Maybe (Rule a s info)
genAssumeChrs ([]     , _   , _    ) = Nothing
genAssumeChrs (context, head, infos) =
  let super sClass info = [Assume sClass, Reduction sClass info [head]]
  in  Just ([Assume head] ==> concat (zipWith super context infos))

-- Versie met evidence
genClassSimplChrs' :: (Matchable a s, Ord a, Ord info) => [Rule a s info] -> ClassDecl a info -> [Rule a s info]
genClassSimplChrs' rules (context, head, infos) =
  let superClasses = chrSolveList rules (map Assume context)
      graph        = foldr addReduction emptyAGraph superClasses 

      mapTrans reds subClass = concatMap (transClosure reds subClass)

      transClosure reds par (info, pr@(Pred p)) =
        let reds'  = Reduction p info [par] : reds
            pres   = predecessors graph pr
         in ([Prove head, Prove p] ==> reds')
            : mapTrans reds' p pres

  in mapTrans [] head (zip infos (map Pred context))

-----------------------------------------------------------------------------
-- Chrs for instance declarations
-----------------------------------------------------------------------------
genInstanceChrs :: (Matchable a s, Ord info) => [InstanceDecl a info] -> [Rule a s info]
genInstanceChrs =
  let genSimpl (context, head, info) = [Prove head] ==> Reduction head info context : map Prove context
  in  map genSimpl

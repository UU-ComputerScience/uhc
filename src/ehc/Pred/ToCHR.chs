%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[9 module {%{EH}Pred.ToCHR} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}Cnstr})
%%]

%%[9 import(Data.Maybe)
%%]

%%[9 import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Solve})
%%]

%%[9 import({%{EH}Pred.CHR},{%{EH}Pred.Heuristics})
%%]

%%[9 import(EH.Util.AGraph)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(PredStore)
type PredStore p g s info = CHRStore (Constraint p info) g s
%%]
type PredStore = CHRStore (Constraint PredOcc ()) Guard Cnstr

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Temporary stuff, placeholders for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type Info = ()
%%]

%%[9
type ClassDecl    a info = ([a], a, [info])
type InstanceDecl a info = ([a], a, info)
%%]
type ClassDecl    a info = ([a], a, [info])
type InstanceDecl a info = ([a], a, info)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR, initial (test) version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-----------------------------------------------------------------------------
-- Functions to generate chrs for a list of class declarations
-- and to check the acyclicity of the class hiearchy.
-----------------------------------------------------------------------------

%%[9
genChrs :: (CHRMatchable env a s, Ord a, Ord info) => [ClassDecl a info] -> [InstanceDecl a info] -> PredStore a g s info
genChrs classes insts =
  let classChrs = genClassChrs classes
      instChrs  = genInstanceChrs insts
  in  classChrs `chrStoreUnion` instChrs

genClassChrs :: (CHRMatchable env a s, Ord a, Ord info) => [ClassDecl a info] -> PredStore a g s info
genClassChrs clsDecls =
  let assumeChrs = chrStoreUnions $ map genAssumeChrs clsDecls
      simplChrs  = chrStoreUnions $ map (genClassSimplChrs' assumeChrs) clsDecls
  in  assumeChrs `chrStoreUnion` simplChrs

genAssumeChrs :: (CHRMatchable env a s, Ord info) => ClassDecl a info -> PredStore a g s info
genAssumeChrs ([]     , _   , _    ) = emptyCHRStore
genAssumeChrs (context, head, infos) =
  let super sClass info = [Assume sClass, Reduction sClass info [head]]
  in  chrStoreSingletonElem $ [Assume head] ==> concat (zipWith super context infos)
%%]

-- Versie met evidence

%%[9
genClassSimplChrs' :: (CHRMatchable env a s, Ord a, Ord info) => PredStore a g s info -> ClassDecl a info -> PredStore a g s info
genClassSimplChrs' rules (context, head, infos) =
  let superClasses = chrSolve' rules (map Assume context)
      graph        = foldr addReduction emptyAGraph superClasses 

      mapTrans reds subClass = concatMap (transClosure reds subClass)

      transClosure reds par (info, pr@(Red_Pred p)) =
        let reds'  = Reduction p info [par] : reds
            pres   = predecessors graph pr
         in ([Prove head, Prove p] ==> reds')
            : mapTrans reds' p pres

  in chrStoreFromElems $ mapTrans [] head (zip infos (map Red_Pred context))
%%]

%%[9
-----------------------------------------------------------------------------
-- Chrs for instance declarations
-----------------------------------------------------------------------------
genInstanceChrs :: (CHRMatchable env a s, Ord info) => [InstanceDecl a info] -> PredStore a g s info
genInstanceChrs =
  let genSimpl (context, head, info) = chrStoreSingletonElem $ [Prove head] ==> Reduction head info context : map Prove context
  in  chrStoreUnions . map genSimpl
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
%%]
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

%%[9
data RedNode p
  =  Red_Pred p
  |  Red_And [p]
  deriving (Eq, Ord)

instance Show p => Show (RedNode p) where
  show (Red_Pred p)    = show p
  show (Red_And [])    = "True"
  show (Red_And _ )    = "And"

true  ::  RedNode p
true  =   Red_And []

type Graph p info = AGraph (RedNode p) info
    
------------------------------------------------------------------
-- Constructing a graph from Reduction constraints
------------------------------------------------------------------

addAssumption :: Ord p => Constraint p info -> [info] -> Graph p info -> Graph p info
addAssumption (Assume  p)  is  = insertEdges (zip3 (repeat (Red_Pred p)) (repeat true) is) 
addAssumption _            _   = id

addReduction :: Ord p => Constraint p info -> Graph p info -> Graph p info
addReduction (Reduction p i [q])  =  insertEdge (Red_Pred p, Red_Pred q  , i)
addReduction (Reduction p i ps)   =  let  andNd  = Red_And ps
                                          edges  = map (\q -> (andNd, Red_Pred q, i)) ps
                                     in   insertEdges ((Red_Pred p, andNd, i) : edges)
addReduction _                    =  id

------------------------------------------------------------------
-- Generating alternatives from a reduction graph
------------------------------------------------------------------
alternatives :: Ord p => Graph p info -> p -> Alts p info
alternatives gr = recOr
  where  recOr   p       = Alts  p  (map recAnd  (successors gr (Red_Pred p))) 
         recAnd  (i, n)  = Red   i  (map recOr   (preds n))
         preds  n  = case n of
                       Red_Pred  q   -> [q]
                       Red_And   qs  -> qs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9

%%]

sPreds1 = flip sPred s1

scopedTrans1 clsDecls insts =
  let  (assumeChrs, instChrs) = genScopedChrs clsDecls insts
       simplChrs  =  concatMap (genClassSimplChrs' False assumeChrs) clsDecls
       scoperule  =  [Prove (SPred p1 s1)] 
                     ==> [Reduction (SPred p1 s1) ByScope [(SPred p1 s2)],  Prove (SPred p1 s2)]
                     |> s2 `assignParentScope` s1 
  in   scoperule :  assumeChrs ++ instChrs ++ simplChrs 

scopedTrans2 clsDecls insts =
  let  (assumeChrs, instChrs) = genScopedChrs clsDecls insts
       simplChrs  = concatMap (genClassSimplChrs' True assumeChrs) clsDecls
       scopeProve = [Prove (SPred p1 s1), Prove (SPred p1 s2)] 
                    ==> [Prove (SPred p1 s3), Reduction (SPred p1 s1) ByScope [(SPred p1 s3)]]
                    |> assignCommonScope s1 s2 s3   
       scopeAssum = [Prove (SPred p1 s1), Assume (SPred p1 s2)] 
                    ==> [Reduction (SPred p1 s1) ByScopeA [(SPred p1 s3)]]
                    |> assignCommonScope s1 s2 s3                            
  in   scopeProve : scopeAssum : assumeChrs ++ instChrs ++ simplChrs 
                        
genClassSimplChrs' scopeRules rules (context, head, infos) =
  let superClasses = chrSolve' rules (map (Assume . sPreds1) context)
      graph        = foldr addReduction emptyAGraph superClasses 

      mapTrans reds subClass = concatMap (transClosure reds subClass)

      transClosure reds par (info, pr@(Pred p@(SPred super _))) =
        let superRule  = [Prove (sPred head s1), Prove p] ==> Prove (sPred head s1) : reds'
            
            scopeRule1 = [Prove (sPred head s1), Prove (SPred super s2)] 
                         ==> [Prove (sPred head s3), Reduction (sPred head s1) ByScope [(sPred head s3)]]
                         |> assignCommonScope s1 s2 s3
                         
            scopeRule2 = [Prove (sPred head s2), Prove (SPred super s1)] 
                         ==> [Prove (SPred super s3), Reduction (SPred super s1) ByScope [(SPred super s3)]]
                         |> assignCommonScope s1 s2 s3
                         
            reds'  = Reduction p info [par] : reds
                         
            rules = mapTrans reds' p (predecessors graph pr)             
                        
         in if scopeRules
            then superRule : scopeRule1 : scopeRule2 : rules 
            else superRule : rules

  in mapTrans [] (sPreds1 head ) (zip infos (map (Pred . sPreds1) context))

genScopedChrs clsDecls insts =
  let assumeChrs = mapMaybe genAssumeChrs clsDecls 
      instChrs   = genInstanceChrs insts
  in  (assumeChrs, instChrs)

genAssumeChrs ([]     ,  _  , _    ) = Nothing
genAssumeChrs (context, head, infos) =
  let super sClass info = [Assume (sPreds1 sClass), Reduction (sPreds1 sClass) info [sPreds1 head]]
  in  Just ([Assume (sPreds1 head)] ==> concat (zipWith super context infos))

genInstanceChrs insts = map genInstanceChr insts

genInstanceChr (context, hd, i, s) =
  let  constraint = sPreds1 hd
       body = map sPreds1 context
  in   [Prove constraint] ==>
       Reduction constraint i body : map Prove body
       |> s1 `visibleIn` s


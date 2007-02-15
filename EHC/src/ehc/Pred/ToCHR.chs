%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[9 module {%{EH}Pred.ToCHR} import({%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Cnstr})
%%]

%%[9 import(Data.Maybe)
%%]

%%[9 import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Solve})
%%]

%%[9 import({%{EH}Pred.CHR},{%{EH}Pred.Heuristics})
%%]

%%[9 import({%{EH}Ty.FitsIn})
%%]

%%[9 import(EH.Util.AGraph)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ScopedPredStore)
type PredStore p g s info = CHRStore p info g s
type ScopedPredStore = PredStore PredOcc Guard Cnstr RedInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intermediate structures for constructing CHR variants of class/instance decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRClassDecl,CHRScopedInstanceDecl)
type CHRClassDecl    		a info 		= ([a], a, [info])
type CHRInstanceDecl 		a info 		= ([a], a, info)
type CHRScopedInstanceDecl  a info sc 	= ([a], a, info, sc)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR, initial (test) version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-----------------------------------------------------------------------------
-- Functions to generate chrs for a list of class declarations
-- and to check the acyclicity of the class hiearchy.
-----------------------------------------------------------------------------

%%[9
%%]
genChrs :: (CHRMatchable env a s, Ord a, Ord info) => [CHRClassDecl a info] -> [CHRInstanceDecl a info] -> PredStore a g s info
genChrs classes insts =
  let classChrs = genClassChrs classes
      instChrs  = genInstanceChrs insts
  in  classChrs `chrStoreUnion` instChrs

genClassChrs :: (CHRMatchable env a s, Ord a, Ord info) => [CHRClassDecl a info] -> PredStore a g s info
genClassChrs clsDecls =
  let assumeChrs = chrStoreUnions $ map genAssumeChrs clsDecls
      simplChrs  = chrStoreUnions $ map (genClassSimplChrs' assumeChrs) clsDecls
  in  assumeChrs `chrStoreUnion` simplChrs

genAssumeChrs :: (CHRMatchable env a s, Ord info) => CHRClassDecl a info -> PredStore a g s info
genAssumeChrs ([]     , _   , _    ) = emptyCHRStore
genAssumeChrs (context, head, infos) =
  let super sClass info = [Assume sClass, Reduction sClass info [head]]
  in  chrStoreSingletonElem $ [Assume head] ==> concat (zipWith super context infos)

-- Versie met evidence

%%[9
%%]
genClassSimplChrs' :: (CHRMatchable env a s, Ord a, Ord info) => PredStore a g s info -> CHRClassDecl a info -> PredStore a g s info
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

%%[9
%%]
-----------------------------------------------------------------------------
-- Chrs for instance declarations
-----------------------------------------------------------------------------
genInstanceChrs :: (CHRMatchable env a s, Ord info) => [CHRInstanceDecl a info] -> PredStore a g s info
genInstanceChrs =
  let genSimpl (context, head, info) = chrStoreSingletonElem $ [Prove head] ==> Reduction head info context : map Prove context
  in  chrStoreUnions . map genSimpl

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
type MkRes1 =  (ScopedPredStore,([PredOcc],PredOcc))
type MkResN = (ScopedPredStore,[([PredOcc],PredOcc)])
%%]

Variables used in CHR's are implicitly universally quantified for each constraint,
they will match against a concrete constraint and then bound the info found therein.
Hence we can safely use non-unique variables.

%%[9
([sc1,sc2,sc3]
 ,[poi1,poi2,poi3]
 ,[pr1,pr2,pr3]
 )
  = ( map PredScope_Var [u1,u2,u3]
    , map PredOccId_Var [u4,u5,u6]
    , map Pred_Var [u7,u8,u9]
    )
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9] = mkNewLevUIDL 9 uidStart
%%]

%%[9 export(initScopedPredStore)
initScopedPredStore :: FIIn -> ScopedPredStore
initScopedPredStore env
  = chrStoreFromElems [scopeProve,scopeAssum]
  where  p1s1         = mkPredOcc pr1 poi1 sc1
         p1s2         = mkPredOcc pr1 poi1 sc2
         p1s3         = mkPredOcc pr1 poi1 sc3
         scopeProve   = [Prove p1s1, Prove p1s2] 
                          ==> [Prove p1s3, Reduction p1s1 (mkRedInfo RedHow_ByScope) [p1s3]]
                           |> [HasCommonScope sc1 sc2 sc3]
         scopeAssum   = [Prove p1s1, Assume p1s2] 
                          ==> [Reduction p1s1 (mkRedInfo RedHow_ByScopeA) [p1s3]]
                            |> [HasCommonScope sc1 sc2 sc3]
%%]

%%[9 export(mkScopedCHR2)
mkScopedCHR2 :: FIIn -> [CHRClassDecl Pred RedInfo] -> [CHRScopedInstanceDecl Pred RedInfo PredScope] -> ScopedPredStore
mkScopedCHR2 env clsDecls insts
  = chrStoreUnions $ [assumeStore,instStore] ++ simplStores
  where  (u1:ucls) = mkNewLevUIDL (length clsDecls + 1) $ fiUniq env
         ((assumeStore,assumePredOccs), (instStore,_)) = mkScopedChrs (env {fiUniq = u1}) clsDecls insts
         simplStores  = zipWith3 (\u (cx,h) (_,_,i) -> mkClassSimplChrs (env {fiUniq = u}) assumeStore (cx,h,i)) ucls assumePredOccs clsDecls

mkClassSimplChrs :: FIIn -> ScopedPredStore -> CHRClassDecl PredOcc RedInfo -> ScopedPredStore
mkClassSimplChrs env rules (context, head, infos)
  = chrStoreFromElems $ mapTrans [] head (zip infos (map Red_Pred context))
  where superClasses = chrSolve env rules (map Assume context)
        graph        = foldr addReduction emptyAGraph superClasses
        head1        = poUpdSc sc1 head
        head2        = poUpdSc sc2 head
        head3        = poUpdSc sc3 head
        byScInfo     = mkRedInfo RedHow_ByScope
    
        mapTrans reds subClass = concatMap (transClosure reds subClass)
    
        transClosure reds par (info, pr@(Red_Pred p))
          = superRule : scopeRule1 : scopeRule2 : rules
          where super1     = poUpdSc sc1 p
                super2     = poUpdSc sc2 p
                super3     = poUpdSc sc3 p
                superRule  = [Prove head1, Prove p] ==> Prove head1 : reds'
                scopeRule1 = [Prove head1, Prove super2] 
                               ==> [Prove head3, Reduction head1 byScInfo [head3]]
                                 |> [HasCommonScope sc1 sc2 sc3]
                scopeRule2 = [Prove head2, Prove super1] 
                               ==> [Prove super3, Reduction super1 byScInfo [super3]]
                                 |> [HasCommonScope sc1 sc2 sc3]
                reds'      = Reduction p info [par] : reds
                rules      = mapTrans reds' p (predecessors graph pr)             

mkScopedChrs :: FIIn -> [CHRClassDecl Pred RedInfo] -> [CHRScopedInstanceDecl Pred RedInfo PredScope] -> (MkResN,MkResN)
mkScopedChrs env clsDecls insts
  = ((chrStoreUnions assumeStores,assumePredOccs), instChrs)
  where [u1,u2] = mkNewLevUIDL 2 $ fiUniq env
        (assumeStores,assumePredOccs) = unzip $ mapMaybe (mkAssumeChrs (env {fiUniq = u1})) clsDecls 
        instChrs   = mkInstanceChrs (env {fiUniq = u2}) insts

mkAssumeChrs :: FIIn -> CHRClassDecl Pred RedInfo -> Maybe MkRes1
mkAssumeChrs env ([]     ,  _  , _    ) = Nothing
mkAssumeChrs env (context, head, infos) =
  let (up:uother) = mkNewLevUIDL (length context + 1) $ fiUniq env
      prThis = mkPredOccCHR up head sc1
      super prSuper info = [Assume prSuper, Reduction prSuper info [prThis]]
      prSuper = zipWith (\u c -> mkPredOccCHR u c sc1) uother context
  in  Just ( chrStoreSingletonElem $ [Assume prThis] ==> concat (zipWith super prSuper infos)
           , (prSuper,prThis)
           )

mkInstanceChrs :: FIIn -> [CHRScopedInstanceDecl Pred RedInfo PredScope] -> MkResN
mkInstanceChrs env insts
  = (chrStoreUnions instStores,instChrs)
  where us = mkNewLevUIDL (length insts) $ fiUniq env
        (instStores,instChrs) = unzip $ zipWith (\u i -> mkInstanceChr (env {fiUniq = u}) i) us insts

mkInstanceChr :: FIIn -> CHRScopedInstanceDecl Pred RedInfo PredScope -> MkRes1
mkInstanceChr env (context, hd, i, s)
  = ( chrStoreSingletonElem
      $ [Prove constraint]
          ==> Reduction constraint i body : map Prove body
            |> [sc1 `IsVisibleInScope` s]
    , (body,constraint)
    )
  where (up:uother) = mkNewLevUIDL (length context + 1) $ fiUniq env
        constraint = mkPredOcc hd poi1 sc1
        body = zipWith (\u p -> mkPredOccCHR u p sc1) uother context
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHREnv, additional info required for CHR solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
%%]
data CHREnv
  = CHREnv
  	  { fiUniq		:: UID
  	  , chrenvFIIn		:: FIIn
  	  }

emptyCHREnv :: CHREnv
emptyCHREnv = mkCHREnvFromFI emptyFI

mkCHREnv :: FIOpts -> UID -> FIEnv -> CHREnv
mkCHREnv o u e = mkCHREnvFromFI (emptyFI {fiFIOpts = o, fiUniq = u, fiEnv = e})

mkCHREnvFromFI :: FIIn -> CHREnv
mkCHREnvFromFI fi = CHREnv (fiUniq fi) fi


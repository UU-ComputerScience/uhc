%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}Annotations.ConstraintSolver}
%%]

%%[7_2 import({%{EH}Base.Common}, {%{EH}Ty}, {%{EH}Ty.AnnCommon}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), Data.Tree(Tree(..)), EH.Util.Pretty, {%{EH}Ty.Pretty})
%%]

%%[7_2 import({%{EH}Annotations.Constraints}, {%{EH}Ty.Expand}, {%{EH}Ty.AnnInferKind}, {%{EH}EH.ConstrInferTy}, {%{EH}Gam})
%%]

%%[7_2 export(mkSingleTySolver, TySolverIn(..), SolverSem(..), StageSem(..), SingleTySolver)
%%]

%%[7_2 export(mkAllExprSolver, ExprSolverIn(..), AllExprSolver, ExprContext(..))
%%]


Semantics of a solver. These semantics are the main parameter of the solver
framework.

%%[7_2

data SolverSem sem lattice context
  = SolverSem { initialConstr  :: AnnConstrSet sem context
              , initialSubst   :: Map (Annotation Ty) lattice
              , initialValue   :: lattice
              , excludeAnns    :: Set (Annotation Ty)
              , preStagedSem   :: [StageSem sem lattice]
              , finalStagedSem :: [StageSem sem lattice]
              , edgeEqFilter   :: FlowSem sem -> Bool
              }

data StageSem sem lattice
  = StageSem { matchSem :: FlowSem sem -> lattice -> lattice -> (lattice, lattice)
             , compSem  :: AnnComp lattice -> lattice -> (AnnComp lattice, lattice)
             }

%%]


Types for constraint solving. The solve function has a lot of parameters. These
data types take their place to reduce the burden of calling the solve functions.

%%[7_2

type SingleTySolver l = TyGam -> Map HsName Ty -> Map HsName BndgId -> Ty -> Map (Annotation Ty) l

data TySolverIn s
  = TySolverIn { siScope       :: Map BndgId [Ty]
               , siBndgs       :: Map BndgId (AnnConstrSet s ())
               , siExposedAnns :: Map BndgId (Annotations Ty)
               , siExpandTy    :: [Annotation Ty] -> [Ty] -> HsName -> Map HsName [Ty]
               , siUid         :: UID
               }

%%]


Construct a solver for constraints on the kinds of a type.

%%[7_2

mkSingleTySolver :: (Ord s, Show s, PP s, Eq l, Show l) => TySolverIn s -> SolverSem s l () -> SingleTySolver l
mkSingleTySolver si solveSem
  = solver
  where
    (myBndgId, infKindUid, infConstrUid, procConstrUid, procConstrsUid, myTouchUid) = mkNewLevUID5 (siUid si)
  
    solver tyGam tyConKindMap tyConBndgIdMap ty
      = let (kindMap, _)      = inferAnnKindMap tyGam Map.empty ty infKindUid
            (wrappedCs, _, resAnnKind, _, _)
                              = inferTyConstr kindMap tyConKindMap tyConBndgIdMap Map.empty Map.empty ty infConstrUid
            cs                = unwrapSet wrappedCs
            outerAnnTy        = tyOutermostAnn resAnnKind
            fullCs            = Set.singleton (myTouchUid #.. (touchAnn =>=! outerAnnTy) softFlowSem ..# ()) `Set.union` cs
            scope             = Map.singleton myBndgId [ty]
            exclude           = Set.singleton touchAnn `Set.union` excludeAnns solveSem
            bndgs             = Map.singleton myBndgId fullCs
            exposedAnns       = Map.singleton myBndgId (annotations ty) `Map.union` siExposedAnns si
            (annLatMap, g, _) = processConstraints (graphSolve noOptimize) (initialSubst solveSem) flatOps exposedAnns scope exclude bndgs graphs procConstrUid
         in annLatMap `Map.intersection` (Map.fromList [(a, undefined) | a <- annotations_ ty])
         -- Note: remove the intersection. Not required.

    flatOps = FlattenOps { flatExpFun     = \anns tys tcNm -> let mp = siExpandTy si anns tys tcNm
                                                               in \conNm -> Map.findWithDefault (error ("mkSingleTySolver:expand:no such constructor " ++ show conNm ++ " in " ++ show tcNm)) conNm mp
                         , inferVariance  = const kindInferVariance
                         , inferBelowness = \_ _ _ -> NotBelow
                         }
    
    optimize   = composeRewriters [compToEqualityRewriter, basicGraphRewriter (edgeEqFilter solveSem), rewriteSelfEdges (edgeEqFilter solveSem)]
    noOptimize = const (const id)
    
    graphSolve optimizer = stagedPartialSolve stages optimizer
    stages = [ improveSubst (initialValue solveSem) (matchSem stage) (compSem stage) | stage <- (preStagedSem solveSem ++ finalStagedSem solveSem) ]

    graphs
      = let scope      = siScope si
            exclude    = excludeAnns solveSem
            bndgs      = Map.insertWith Set.union uidNull (initialConstr solveSem) (siBndgs si)
            (_, gs, _) = processConstraints (graphSolve noOptimize) (initialSubst solveSem) flatOps (siExposedAnns si) scope exclude bndgs Map.empty procConstrsUid
         in gs


kindInferVariance :: [Ty] -> Annotation Ty -> CoContraVariance
kindInferVariance kinds
  = let mp = foldr (\k m -> kindVarianceMap k `Map.union` m) Map.empty kinds
     in \ann -> Map.findWithDefault (error ("kindInferVariance: Can't find ann " ++ show ann ++ " in " ++ show kinds)) ann mp

%%]


Create an all-in-one-go solver for expression constraints.

%%[7_2

data ExprContext
  = ExprContext

type AllExprSolver l
  = Map (Annotation Ty) l

data ExprSolverIn s
  = ExprSolverIn { esiScope           :: Map BndgId [Ty]
                 , esiBndgs           :: Map BndgId (AnnConstrSet s ExprContext)
                 , esiExpandTy        :: [Annotation Ty] -> [Ty] -> HsName -> Map HsName [Ty]
                 , esiVarianceSolver  :: Ty -> Map (Annotation Ty) CoContraVariance
                 , esiBelownessSolver :: Ty -> Map (Annotation Ty) Belowness
                 , esiUid             :: UID
                 }

mkAllExprSolver :: (Ord s, Show s, PP s, Eq l, Show l) => ExprSolverIn s -> SolverSem s l ExprContext -> AllExprSolver l
mkAllExprSolver si solveSem
  = {- error (show (pp (esiBndgs si)))
  $ -} subst'
  where
    procConstrsUid = esiUid si

    graphSolve = stagedPartialSolve stages optimizer
    optimizer  = const (const id) -- composeRewriters [componentReduction]
    stages     = [ improveSubst (initialValue solveSem) (matchSem stage) (compSem stage) | stage <- preStagedSem solveSem ]

    addAnns = Map.fromList [(bndgId, Set.empty) | bndgId <- Map.keys bndgs ]
    bndgs   = Map.insertWith Set.union uidNull (initialConstr solveSem) (esiBndgs si)
    scope   = esiScope si
    exclude = Set.singleton touchAnn `Set.union` excludeAnns solveSem

    flatOps = FlattenOps { flatExpFun     = \anns tys tcNm -> let mp = esiExpandTy si anns tys tcNm
                                                               in \conNm -> Map.findWithDefault (error ("allExprSolver:expand:no such constructor " ++ show conNm ++ " in " ++ show tcNm)) conNm mp
                         , inferVariance  = wrapSolver "variance" (esiVarianceSolver si)
                         , inferBelowness = wrapSolver "belowness" (esiBelownessSolver si)
                         }

    (subst, graphs, _) = processConstraints graphSolve (initialSubst solveSem) flatOps addAnns scope exclude bndgs Map.empty procConstrsUid
    subst' = foldr (\stage -> solveToBndgSubstitution (improveSubst (initialValue solveSem) (matchSem stage) (compSem stage)) graphs) subst (finalStagedSem solveSem)

wrapSolver :: Show l => String -> (Ty -> Map (Annotation Ty) l) -> ExprContext -> [Ty] -> Annotation Ty -> l
wrapSolver what f ctx tyL
  = let results = map f tyL
        result  = Map.unions results
     in \ann -> Map.findWithDefault (error ("mkAllExprSolver:apply:"++what++": No such solve result for annotation: " ++ show ann ++ " | have: " ++ show results ++ " | " ++ show tyL)) ann result

%%]


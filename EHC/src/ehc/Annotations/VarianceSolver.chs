%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver for variance constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}Annotations.VarianceSolver}
%%]

%%[7_2 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Ty}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), EH.Util.Pretty)
%%]

%%[7_2 import({%{EH}Annotations.Constraints}, {%{EH}Annotations.ConstraintSolver})
%%]

%%[7_2 export(mkVarianceSem, VarianceLattice, latticeToVariance)
%%]


Variance Lattice

%%[7_2

data VarianceLattice
  = VarBottom
  | VarCoVariant
  | VarContraVariant
  | VarTop
  deriving Eq

data VarianceSem = VarSemFlip deriving (Show, Eq, Ord)

instance PP VarianceSem where
  pp _ = text "~"

instance PP VarianceLattice where
  pp = text . show

instance Show VarianceLattice where
  show VarBottom        = "?"
  show VarCoVariant     = "+"
  show VarContraVariant = "-"
  show VarTop           = "o"

latticeToVariance :: VarianceLattice -> CoContraVariance
latticeToVariance b
  = case b of
      VarCoVariant     -> CoVariant
      VarContraVariant -> ContraVariant
      _                -> CoContraVariant

%%]


Transfer functions.

%%[7_2

match :: FlowSem VarianceSem -> VarianceLattice -> VarianceLattice -> (VarianceLattice, VarianceLattice)
match (UserFlow _) a b = (a, joinLat (inverse a) b)
match _            a b = (a, joinLat a b)

comp :: AnnComp VarianceLattice -> VarianceLattice -> (AnnComp VarianceLattice, VarianceLattice)
comp tree a
  = let as = collect tree
        a' = foldr joinLat a as
     in (distribute (repeat a') tree, a')

joinLat :: VarianceLattice -> VarianceLattice -> VarianceLattice
joinLat x VarBottom = x
joinLat VarBottom x = x
joinLat a b | a == b    = a
            | otherwise = VarTop

inverse :: VarianceLattice -> VarianceLattice
inverse VarCoVariant     = VarContraVariant
inverse VarContraVariant = VarCoVariant
inverse x                = x

%%]


Constructions of solver semantics.

%%[7_2

mkVarianceSem :: Map HsName Ty -> UID -> SolverSem VarianceSem VarianceLattice ()
mkVarianceSem tyconKindMap uid
  = let annArrow    = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:->")       hsnArrow    tyconKindMap
        annInt      = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Int")      hsnInt      tyconKindMap
        annChar     = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Char")     hsnChar     tyconKindMap
        annRec      = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Rec")      hsnRec      tyconKindMap
        annEmptyRec = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:RecEmpty") hsnRowEmpty tyconKindMap

        [aArrA, aArrB, aArrC] = annotationsStar annArrow
        [aInt]                = annotationsStar annInt
        [aChar]               = annotationsStar annChar
        [aRecEmpty]           = annotationsStar annEmptyRec
        [aRecRow, aRecStar]   = annotationsStar annRec

        (uid1,uid2,uid3) = mkNewLevUID2 uid
        cs    = Set.fromList [ uid1 #.. (aArrC =>=! aArrA) (UserFlow VarSemFlip) ..# ()
                             , uid2 #.. (aArrC =>=! aArrB) softFlowSem ..# ()
                             , uid3 #.. (aRecStar =>=! aRecRow) hardFlowSem ..# ()
                             ]
        subst = Map.fromList [(touchAnn, VarCoVariant)]
     in SolverSem { initialConstr  = cs
                  , initialSubst   = subst
                  , initialValue   = VarBottom
                  , excludeAnns    = Set.empty
                  , preStagedSem   = [ StageSem { matchSem = match
                                                , compSem  = comp
                                                }
                                     ]
                  , finalStagedSem = []
                  , edgeEqFilter   = not . isUserFlow
                  }

%%]

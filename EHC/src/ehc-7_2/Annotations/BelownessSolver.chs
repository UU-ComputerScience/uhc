%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver for belowness constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}Annotations.BelownessSolver}
%%]

%%[7_2 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Ty}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), EH.Util.Pretty)
%%]

%%[7_2 import({%{EH}Annotations.Constraints}, {%{EH}Annotations.ConstraintSolver})
%%]

%%[7_2 export(mkBelownessSem, BelownessLattice, latticeToBelowness)
%%]


Belowness Lattice.

%%[7_2

data BelownessLattice
  = BLBottom
  | BLBelow
  | BLNotBelow
  | BLTop
  deriving Eq

type BelownessSem = ()

instance PP BelownessLattice where
  pp = text . show

instance Show BelownessLattice where
  show BLBottom   = "?"
  show BLBelow    = "B"
  show BLNotBelow = "NB"
  show BLTop      = "T"

latticeToBelowness :: BelownessLattice -> Belowness
latticeToBelowness b
  = case b of
      BLBottom   -> Below
      BLBelow    -> Below
      BLNotBelow -> NotBelow
      _          -> UnknownBelow

%%]


Transfer functions.

%%[7_2

match :: FlowSem BelownessSem -> BelownessLattice -> BelownessLattice -> (BelownessLattice, BelownessLattice)
match _ a b = (a, joinLat a b)

comp :: AnnComp BelownessLattice -> BelownessLattice -> (AnnComp BelownessLattice, BelownessLattice)
comp tree a
  = let as = collect tree
        a' = foldr joinLat a as
     in (distribute (repeat a') tree, a')

joinLat :: BelownessLattice -> BelownessLattice -> BelownessLattice
joinLat x BLBottom = x
joinLat BLBottom x = x
joinLat a b | a == b    = a
            | otherwise = BLTop

%%]


Constructions of solver semantics.

%%[7_2

mkBelownessSem :: Map HsName Ty -> UID -> SolverSem BelownessSem BelownessLattice ()
mkBelownessSem tyconKindMap uid
  = let (uid1,uid2,uid3,uid4) = mkNewLevUID3 uid
        annBelow    = Annotation { annUID      = uid1
                                 , annInstFrom = Nothing
                                 , annOnRefTp  = Nothing
                                 }

        annArrow    = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:->")       hsnArrow    tyconKindMap
        annInt      = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Int")      hsnInt      tyconKindMap
        annChar     = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Char")     hsnChar     tyconKindMap
        annRec      = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:Rec")      hsnRec      tyconKindMap
        annEmptyRec = Map.findWithDefault (error "mkBelownessSem:tyconKindMap:undefined:RecEmpty") hsnRowEmpty tyconKindMap

        [aArrA, aArrB, aArrC] = annotationsStar annArrow
        [aInt]                = annotationsStar annInt
        [aChar]               = annotationsStar annChar
        [aRecEmpty]           = annotationsStar annEmptyRec
        [aRecRow, aRecStar]   = annotationsStar annRec

        cs    = Set.fromList [ uid2 #.. (annBelow =>=! aArrA) softFlowSem ..# ()
                             , uid3 #.. (annBelow =>=! aArrB) softFlowSem ..# ()
                             , uid4 #.. (aRecStar =>=! aRecRow) hardFlowSem ..# ()
                             ]
        subst = Map.fromList [(touchAnn, BLNotBelow), (annBelow, BLBelow)]
     in SolverSem { initialConstr  = cs
                  , initialSubst   = subst
                  , initialValue   = BLBottom
                  , excludeAnns    = Set.singleton annBelow
                  , preStagedSem   = [ StageSem { matchSem = match
                                                , compSem  = comp
                                                }
                                     ]
                  , finalStagedSem = []
                  , edgeEqFilter   = const True
                  }

%%]

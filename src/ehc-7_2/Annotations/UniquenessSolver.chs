%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver for uniqueness constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs module {%{EH}Annotations.UniquenessSolver}
%%]

%%[7_2 import({%{EH}Base.Common}, {%{EH}Ty}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), EH.Util.Pretty)
%%]

%%[7_2 import({%{EH}Annotations.Constraints}, {%{EH}Annotations.ConstraintSolver})
%%]

%%[7_2 export(mkUniquenessSem, UniquenessLattice(..))
%%]


Uniqueness Lattice

%%[7_2

data UniquenessLattice
  = Unused
  | Unique
  | Shared
  | UniquenessConflict
  deriving (Eq, Ord)

data UniquenessSem = UniquenessSem deriving (Show, Eq, Ord)

instance PP UniquenessSem where
  pp _ = error "<<unused>>"

instance PP UniquenessLattice where
  pp = text . show

instance Show UniquenessLattice where
  show Unused = "0"
  show Unique = "1"
  show Shared = "*"
  show UniquenessConflict = "!"

%%]


Transfer functions.

%%[7_2

matchRefCount :: FlowSem UniquenessSem -> UniquenessLattice -> UniquenessLattice -> (UniquenessLattice, UniquenessLattice)
matchRefCount _ a b  -- if 1 < a then 1 <= b else a <= b
  | a > Unique  = (a, joinRefLat b Unique)
  | otherwise   = (a, joinRefLat a b)

compRefCount :: AnnComp UniquenessLattice -> UniquenessLattice -> (AnnComp UniquenessLattice, UniquenessLattice)
compRefCount tree a
  = (tree, joinRefLat (sumComp tree) a)

sumComp :: AnnComp UniquenessLattice -> UniquenessLattice
sumComp (Plus a b) = sumComp a `plusRefLat` sumComp b
sumComp (Max a b)  = sumComp a `joinRefLat`  sumComp b
sumComp (Embed a)  = a

joinRefLat :: UniquenessLattice -> UniquenessLattice -> UniquenessLattice
joinRefLat x y = max x y

plusRefLat :: UniquenessLattice -> UniquenessLattice -> UniquenessLattice
plusRefLat Unused x = x
plusRefLat x Unused = x
plusRefLat x y      = Shared `joinRefLat` x `joinRefLat` y

matchUnqProp :: FlowSem UniquenessSem -> UniquenessLattice -> UniquenessLattice -> (UniquenessLattice, UniquenessLattice)
matchUnqProp SoftFlow a b
  = (a, b)
matchUnqProp HardFlow a b
  = let z = max a b in (z, z)
-- case (a, b) of
--       z@(x, y) | x >= y -> z
--       _ -> let z = max a b in (z, b)

compUnqProp :: AnnComp UniquenessLattice -> UniquenessLattice -> (AnnComp UniquenessLattice, UniquenessLattice)
compUnqProp tree a
  = let as = collect tree
        a' = foldr max a as
     in (distribute (repeat a') tree, a')

%%]


Constructions of solver semantics.

%%[7_2

mkUniquenessSem :: SolverSem UniquenessSem UniquenessLattice context
mkUniquenessSem
  = let cs    = Set.empty
        subst = Map.fromList [(touchAnn, Unique)]
     in SolverSem { initialConstr  = cs
                  , initialSubst   = subst
                  , initialValue   = Unused
                  , excludeAnns    = Set.empty
                  , preStagedSem   = [ StageSem { matchSem = matchRefCount
                                                , compSem  = compRefCount
                                                }
                                     ]
                  , finalStagedSem = [ StageSem { matchSem = matchRefCount
                                                , compSem  = compRefCount
                                                }
                                     , StageSem { matchSem = matchUnqProp
                                                , compSem  = compUnqProp
                                                }
                                     ]
                  , edgeEqFilter   = const True
                  }

%%]

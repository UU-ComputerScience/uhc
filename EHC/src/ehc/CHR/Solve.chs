%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

Assumptions (to be documented further)
- The key [Trie.TrieKey Key] used to lookup a constraint in a CHR should be distinguishing enough to be used for the prevention
  of the application of a propagation rule for a 2nd time.

%%[(9 hmtyinfer) module {%{EH}CHR.Solve}
%%]

%%[(9 hmtyinfer) import(qualified Data.Set as Set)
%%]

%%[(9 hmtyinfer) import(UHC.Util.Utils, UHC.Util.CHR, UHC.Util.CHR.Rule, UHC.Util.Substitutable) export(module UHC.Util.CHR.Rule)
%%]

%%[(9 hmtyinfer) import({%{EH}CHR.Instances}, {%{EH}CHR.Key}, {%{EH}VarMp}, {%{EH}Ty}, {%{EH}Ty.FitsInCommon2})
%%]

%%[(9 hmtyinfer) import(UHC.Util.CHR.Solve.TreeTrie.Mono hiding(IsCHRSolvable(..), SolveState, SolveTrace, SolveStep, CHRStore), qualified UHC.Util.CHR.Solve.TreeTrie.Mono as Mono) export(module UHC.Util.CHR.Solve.TreeTrie.Mono, IsCHRSolvable(..), SolveState, SolveTrace, SolveStep, CHRStore', CHRSolverConstraint)
instance Mono.IsCHRSolvable FIIn Constraint Guard VarMp

type CHRSolverConstraint = Constraint

-- | (Class alias) API for solving requirements, hiding Mono/Poly differences
class ( Mono.IsCHRSolvable env c g s
      ) => IsCHRSolvable env c g s

instance IsCHRSolvable FIIn Constraint Guard VarMp

type CHRStore'  e c g s = Mono.CHRStore   c g
type SolveState e c g s = Mono.SolveState c g s
type SolveTrace e c g s = Mono.SolveTrace c g s
type SolveStep  e c g s = Mono.SolveStep  c g s
%%]

%%[(9999 hmtyinfer) import(UHC.Util.CHR.Solve.TreeTrie.Poly hiding(IsCHRSolvable(..), SolveState, SolveTrace, SolveStep, CHRStore), qualified UHC.Util.CHR.Solve.TreeTrie.Poly as Poly) export(module UHC.Util.CHR.Solve.TreeTrie.Poly, IsCHRSolvable(..), SolveState, SolveTrace, SolveStep, CHRStore', CHRSolverConstraint)
instance Poly.IsCHRSolvable FIIn VarMp

type CHRSolverConstraint = CHRConstraint FIIn VarMp

type instance ExtrValVarKey (CHRConstraint FIIn VarMp) = TyVarId
type instance ExtrValVarKey (CHRGuard FIIn VarMp) = TyVarId

-- | (Class alias) API for solving requirements, hiding Mono/Poly differences
class ( Poly.IsCHRSolvable env s
      ) => IsCHRSolvable env c g s

instance IsCHRSolvable FIIn Constraint Guard VarMp

type instance TTKey (CHRConstraint FIIn VarMp) = TTKey Constraint

type CHRStore'  e c g s = Poly.CHRStore   e s
type SolveState e c g s = Poly.SolveState e s
type SolveTrace e c g s = Poly.SolveTrace e s
type SolveStep  e c g s = Poly.SolveStep  e s

{-
instance Serialize (CHRRule FIIn VarMp) where
  sput (CHRRule (Rule a b c s)) = sput a
  sget = liftM CHRRule sget
-}

%%]

%%[(9 hmtyinfer) export(CHRStore)
type CHRStore = CHRStore' FIIn Constraint Guard VarMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lattice ordering, for annotations which have no ordering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be put in some library

%%[(9 hmtyinfer) export(PartialOrdering(..),toOrdering,toPartialOrdering)
data PartialOrdering
  = P_LT | P_EQ | P_GT | P_NE
  deriving (Eq,Show)

toPartialOrdering :: Ordering -> PartialOrdering
toPartialOrdering o
  = case o of
      EQ -> P_EQ
      LT -> P_LT
      GT -> P_GT

toOrdering :: PartialOrdering -> Maybe Ordering
toOrdering o
  = case o of
      P_EQ -> Just EQ
      P_LT -> Just LT
      P_GT -> Just GT
      _    -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Criterium for proving in a let expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(isLetProveCandidate,isLetProveFailure)
-- | Consider a pred for proving if: no free tvars, or its free tvars do not coincide with those globally used
isLetProveCandidate :: (VarExtractable x) => Set.Set (ExtrValVarKey x) -> x -> Bool
isLetProveCandidate glob x
  = Set.null fv || Set.null (fv `Set.intersection` glob)
  where fv = varFreeSet x

isLetProveFailure :: (VarExtractable x) => Set.Set (ExtrValVarKey x) -> x -> Bool
isLetProveFailure glob x
  = Set.null fv
  where fv = varFreeSet x
%%]



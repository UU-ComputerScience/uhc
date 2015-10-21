%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

Assumptions (to be documented further)
- The key [Trie.TrieKey Key] used to lookup a constraint in a CHR should be distinguishing enough to be used for the prevention
  of the application of a propagation rule for a 2nd time.

%%[(9 hmtyinfer) module {%{EH}CHR.Solve} import(UHC.Util.CHR.Solve.TreeTrie.Mono hiding(IsCHRSolvable(..), SolveState), qualified UHC.Util.CHR.Solve.TreeTrie.Mono as Mono) export(module UHC.Util.CHR.Solve.TreeTrie.Mono, IsCHRSolvable(..), SolveState)
instance Mono.IsCHRSolvable FIIn CHRPredConstraint Guard VarMp

-- | (Class alias) API for solving requirements, hiding Mono/Poly differences
class ( Mono.IsCHRSolvable env c g s
      ) => IsCHRSolvable env c g s

instance IsCHRSolvable FIIn CHRPredConstraint Guard VarMp

type SolveState e c g s = Mono.SolveState c g s
%%]

%%[(9999 hmtyinfer) module {%{EH}CHR.Solve} import(UHC.Util.CHR.Solve.TreeTrie.Poly hiding(IsCHRSolvable(..), SolveState), qualified UHC.Util.CHR.Solve.TreeTrie.Poly as Poly) export(module UHC.Util.CHR.Solve.TreeTrie.Poly, IsCHRSolvable(..), SolveState)
instance Poly.IsCHRSolvable FIIn VarMp

-- | (Class alias) API for solving requirements, hiding Mono/Poly differences
class ( Poly.IsCHRSolvable env s
      ) => IsCHRSolvable env c g s

instance IsCHRSolvable FIIn CHRPredConstraint Guard VarMp

type instance TTKey (CHRConstraint FIIn VarMp) = Key

type SolveState e c g s = Poly.SolveState e s
%%]

%%[(9 hmtyinfer) import({%{EH}Pred.CommonCHR}, {%{EH}Pred.CHR}, {%{EH}CHR.Key}, {%{EH}VarMp}, {%{EH}Ty.FitsInCommon2})
%%]


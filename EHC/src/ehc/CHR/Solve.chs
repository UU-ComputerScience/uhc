%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

Assumptions (to be documented further)
- The key [Trie.TrieKey Key] used to lookup a constraint in a CHR should be distinguishing enough to be used for the prevention
  of the application of a propagation rule for a 2nd time.

%%[(9 hmtyinfer || hmtyast) module {%{EH}CHR.Solve} import(UHC.Util.CHR.Solve.TreeTrie.Mono) export(module UHC.Util.CHR.Solve.TreeTrie.Mono)
-- no additions, for now, moved to uhc-util
%%]

%%[(9999 hmtyinfer || hmtyast) module {%{EH}CHR.Solve} import(UHC.Util.CHR.Solve.TreeTrie.Poly) export(module UHC.Util.CHR.Solve.TreeTrie.Poly)
-- no additions, for now, moved to uhc-util
%%]


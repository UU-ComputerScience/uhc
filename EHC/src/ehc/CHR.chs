%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but with searching structures for predicates
to avoid explosion of search space during resolution.

%%[9 module {%{EH}CHR} import(qualified {%{EH}Base.Trie} as Trie)
%%]

%%[9 import(Data.Monoid)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Base structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHR(..),CHRStore)
data CHR cnstr subst
  = CHR
      { chrHead     :: [cnstr]
      , chrGuard    :: subst -> Maybe subst
      , chrBody     :: [cnstr]
      }

type CHRStore cnstr subst = Trie.Trie cnstr (CHR cnstr subst)
type CHRWorkList cnstr = Trie.Trie cnstr cnstr

class (Ord cnstr, Monoid subst) => CHRMatchable cnstr subst | cnstr -> subst where
  chrMatch      :: cnstr -> cnstr -> Maybe subst
  chrSubst      :: subst -> cnstr -> cnstr
  chrTrieKey    :: cnstr -> [Trie.TrieKey cnstr]
%%]

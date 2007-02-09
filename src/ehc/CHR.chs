%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but with searching structures for predicates
to avoid explosion of search space during resolution.

%%[9 module {%{EH}CHR} import(qualified {%{EH}Base.Trie} as Trie,{%{EH}Substitutable},{%{EH}Cnstr})
%%]

%%[9 import(Data.Monoid,qualified Data.Set as Set)
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%[9 import({%{EH}CHR.Key}) export(module {%{EH}CHR.Key})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR, derived structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHR(..),CHRStore)
data CHR cnstr subst
  = CHR
      { chrSimpHead     :: [cnstr]
      , chrPropHead     :: [cnstr]
      , chrGuard        :: subst -> Maybe subst
      , chrBody         :: [cnstr]
      }

emptyCHRGuard :: (Monoid a) => t -> Maybe a
emptyCHRGuard = const $ Just mempty

type CHRStore cnstr subst = Trie.Trie (Trie.TrieKey Key) (CHR cnstr subst)
type CHRWorkList cnstr = Trie.Trie (Trie.TrieKey Key) cnstr
%%]

%%[9
instance (Show c) => Show (CHR c s) where
  showsPrec _ chr
    = case chr of
        (CHR []       ph@(_:_) _ b) -> shows "{ " . showList ph . shows  " ==> " . showList b . shows " }"
        (CHR sh@(_:_) []       _ b) -> shows "{ " . showList sh . shows " <==> " . showList b . shows " }"
        (CHR sh@(_:_) ph@(_:_) _ b) -> shows "{ " . showList sh . shows " | " . showList ph . shows " <==> " . showList b . shows " }"
        (CHR []       []       _ b) -> shows "{ " . showList b . shows " }"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRSubstitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRSubstitutable(..))
class Ord var => CHRSubstitutable x var subst | x -> var, x -> subst where
  chrFtv       :: x -> Set.Set var
  chrAppSubst  :: subst -> x -> x
%%]

%%[9
instance (Ord var,Substitutable x var subst) => CHRSubstitutable x var subst where
  chrFtv        x = Set.fromList (ftv x)
  chrAppSubst s x = s |=> x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRMatchable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRMatchable(..))
class (Keyable x) => CHRMatchable env x subst | x -> subst env where
  chrMatch      :: env -> x -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ppCHRStore)
instance PP c => PP (CHR c s) where
  pp = pp . show

ppCHRStore :: PP c => CHRStore c s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> k >-< indent 2 (":" >#< v)) . Trie.toList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export((<==>), (==>), (|>))
infix   1 <==>, ==>
infixr  0 |>

(<==>), (==>) :: Monoid s => [c] -> [c] -> CHR c s
hs <==>  bs = CHR hs [] emptyCHRGuard bs
hs  ==>  bs = CHR [] hs emptyCHRGuard bs

(|>) :: CHR c s -> (s -> Maybe s) -> CHR c s
chr |> g = chr {chrGuard = g}
%%]


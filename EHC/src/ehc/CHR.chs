%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but with searching structures for predicates
to avoid explosion of search space during resolution.

%%[9 module {%{EH}CHR} import(qualified {%{EH}Base.Trie} as Trie,{%{EH}Base.Common},{%{EH}Substitutable},{%{EH}Cnstr})
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

%%[9 export(CHR(..))
data CHR cnstr guard subst
  = CHR
      { chrSimpHead     :: [cnstr]
      , chrPropHead     :: [cnstr]
      , chrGuard        :: [guard] -- subst -> Maybe subst
      , chrBody         :: [cnstr]
      }

emptyCHRGuard :: [a]
emptyCHRGuard = []
%%]

%%[9
instance Show (CHR c g s) where
  show _ = "CHR"
%%]
    = case chr of
        (CHR []       ph@(_:_) _ b) -> showString "{ " . showList ph . showString  " ==> " . showList b . showString " }"
        (CHR sh@(_:_) []       _ b) -> showString "{ " . showList sh . showString " <==> " . showList b . showString " }"
        (CHR sh@(_:_) ph@(_:_) _ b) -> showString "{ " . showList sh . showString " | " . showList ph . showString " <==> " . showList b . showString " }"
        (CHR []       []       _ b) -> showString "{ " . showList b  . showString " }"

%%[9
instance (PP c,PP g) => PP (CHR c g s) where
  pp chr
    = case chr of
        (CHR []       ph@(_:_) g b) -> ppChr ([ppL ph, pp  "==>"] ++ ppGB g b)
        (CHR sh@(_:_) []       g b) -> ppChr ([ppL sh, pp "<==>"] ++ ppGB g b)
        (CHR sh@(_:_) ph@(_:_) g b) -> ppChr ([ppL sh, pp "|", ppL ph, pp "<==>"] ++ ppGB g b)
        (CHR []       []       g b) -> ppChr (ppGB g b)
    where ppGB g@(_:_) b@(_:_) = [ppL g, "|" >#< ppL b]
          ppGB g@(_:_) []      = [ppL g >#< "|"]
          ppGB []      b@(_:_) = [ppL b]
          ppL [x] = pp x
          ppL xs  = ppBracketsCommasV xs -- ppParensCommasBlock xs
          ppChr l = vlist l -- ppCurlysBlock
%%]

%%[9
instance Keyable cnstr => Keyable (CHR cnstr guard subst) where
  toKey chr = toKey $ head $ chrSimpHead chr ++ chrPropHead chr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRSubstitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRSubstitutable(..))
class Ord var => CHRSubstitutable x var subst | x -> var, x -> subst where
  chrFtv       :: x -> Set.Set var
  chrAppSubst  :: subst -> x -> x
--  chrCmbSubst  :: subst -> subst -> subst
%%]

%%[9
%%]
instance (Ord var,Substitutable x var subst) => CHRSubstitutable x var subst where
  chrFtv        x = Set.fromList (ftv x)
  chrAppSubst s x = s |=> x

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHREmptySubstitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Capability to yield an empty substitution.

%%[9 export(CHREmptySubstitution(..))
class CHREmptySubstitution subst where
  chrEmptySubst :: subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRMatchable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Matchable participates in the reduction process as a reducable constraint.

%%[9 export(CHRMatchable(..))
class (Keyable x) => CHRMatchable env x subst | x -> subst env where
  chrMatchTo      :: env -> x -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Checkable participates in the reduction process as a guard, to be checked.

%%[9 export(CHRCheckable(..))
class CHRCheckable x subst | x -> subst where
  chrCheck      :: x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
%%]
instance PP c => PP (CHR c g s) where
  pp = pp . show

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export((<==>), (==>), (|>))
infix   1 <==>, ==>
infixr  0 |>

(<==>), (==>) :: [c] -> [c] -> CHR c g s
hs <==>  bs = CHR hs [] emptyCHRGuard bs
hs  ==>  bs = CHR [] hs emptyCHRGuard bs

(|>) :: CHR c g s -> [g] -> CHR c g s
chr |> g = chr {chrGuard = chrGuard chr ++ g}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but with searching structures for predicates
to avoid explosion of search space during resolution.

%%[9 module {%{EH}CHR} import(qualified {%{EH}Base.Trie} as Trie,{%{EH}Base.Common},{%{EH}Substitutable},{%{EH}VarMp})
%%]

%%[9 import(Data.Monoid,qualified Data.Set as Set)
%%]

%%[9 import(EH.Util.Pretty)
%%]

%%[9 import({%{EH}CHR.Key}) export(module {%{EH}CHR.Key})
%%]

%%[20 import({%{EH}Base.CfgPP})
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR, derived structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHR(..))
data CHR cnstr guard subst
  = CHR
      { chrHead     	:: ![cnstr]
      , chrSimpSz       :: !Int				-- length of the part of the head which is the simplification part
      , chrGuard        :: ![guard] 		-- subst -> Maybe subst
      , chrBody         :: ![cnstr]
      }

emptyCHRGuard :: [a]
emptyCHRGuard = []
%%]

%%[9
instance Show (CHR c g s) where
  show _ = "CHR"
%%]

%%[9
instance (PP c,PP g) => PP (CHR c g s) where
  pp chr
    = case chr of
        (CHR h@(_:_)  sz g b) | sz == 0        -> ppChr ([ppL h, pp  "==>"] ++ ppGB g b)
        (CHR h@(_:_)  sz g b) | sz == length h -> ppChr ([ppL h, pp "<==>"] ++ ppGB g b)
        (CHR h@(_:_)  sz g b)                  -> ppChr ([ppL (take sz h), pp "|", ppL (drop sz h), pp "<==>"] ++ ppGB g b)
        (CHR []       _  g b)                  -> ppChr (ppGB g b)
    where ppGB g@(_:_) b@(_:_) = [ppL g, "|" >#< ppL b]
          ppGB g@(_:_) []      = [ppL g >#< "|"]
          ppGB []      b@(_:_) = [ppL b]
          ppGB []      []      = []
          ppL [x] = pp x
          ppL xs  = ppBracketsCommasV xs -- ppParensCommasBlock xs
          ppChr l = vlist l -- ppCurlysBlock
%%]

%%[20
instance (PPForHI c, PPForHI g) => PPForHI (CHR c g s) where
  ppForHI chr
    = ppCurlysSemisBlock
        [ ppCurlysSemisBlock $ map ppForHI $ chrHead   chr
        , ppForHI                          $ chrSimpSz chr
        , ppCurlysSemisBlock $ map ppForHI $ chrGuard  chr
        , ppCurlysSemisBlock $ map ppForHI $ chrBody   chr
        ]
%%]

%%[9
instance Keyable cnstr => Keyable (CHR cnstr guard subst) where
  toKey chr = toKey $ head $ chrHead chr
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
instance (CHRSubstitutable c v s,CHRSubstitutable g v s) => CHRSubstitutable (CHR c g s) v s where
  chrFtv          (CHR {chrHead=h, chrGuard=g, chrBody=b})
    = Set.unions $ concat [map chrFtv h, map chrFtv g, map chrFtv b]
  chrAppSubst s r@(CHR {chrHead=h, chrGuard=g, chrBody=b})
    = r {chrHead = map (chrAppSubst s) h, chrGuard = map (chrAppSubst s) g, chrBody = map (chrAppSubst s) b}
%%]

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
  chrMatchTo      :: env -> subst -> x -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Checkable participates in the reduction process as a guard, to be checked.

%%[9 export(CHRCheckable(..))
class CHRCheckable env x subst | x -> subst env where
  chrCheck      :: env -> subst -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export((<==>), (==>), (|>))
infix   1 <==>, ==>
infixr  0 |>

(<==>), (==>) :: [c] -> [c] -> CHR c g s
hs <==>  bs = CHR hs (length hs) emptyCHRGuard bs
hs  ==>  bs = CHR hs 0 emptyCHRGuard bs

(|>) :: CHR c g s -> [g] -> CHR c g s
chr |> g = chr {chrGuard = chrGuard chr ++ g}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance (ForceEval c, ForceEval g) => ForceEval (CHR c g s) where
  forceEval x@(CHR h sz g b) | forceEval h `seq` forceEval g `seq` forceEval b `seq` True = x
%%[[101
  fevCount (CHR h sz g b) = cm1 "CHR" `cmUnion` fevCount h `cmUnion` fevCount sz `cmUnion` fevCount g `cmUnion` fevCount b
%%]]
%%]

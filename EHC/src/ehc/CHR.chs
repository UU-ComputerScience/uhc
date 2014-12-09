%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Derived from work by Gerrit vd Geest, but with searching structures for predicates
to avoid explosion of search space during resolution.
%%]

%%[(9 hmtyinfer || hmtyast) module {%{EH}CHR} import(qualified {%{EH}Base.TreeTrie} as TreeTrie, {%{EH}Base.Common},{%{EH}Substitutable},{%{EH}VarMp})
%%]

%%[(9 hmtyinfer || hmtyast) import(Data.Monoid,qualified Data.Set as Set)
%%]

%%[(9 hmtyinfer || hmtyast) import(UHC.Util.Pretty)
%%]

%%[(9 hmtyinfer || hmtyast) import({%{EH}CHR.Key}) export(module {%{EH}CHR.Key})
%%]

%%[(50 hmtyinfer || hmtyast) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[(9999 hmtyinfer || hmtyast) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR, derived structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(CHR(..))
-- | A CHR (rule) consist of head (simplification + propagation, boundary indicated by an Int), guard, and a body. All may be empty, but not all at the same time.
data CHR cnstr guard subst
  = CHR
      { chrHead     	:: ![cnstr]
      , chrSimpSz       :: !Int				-- length of the part of the head which is the simplification part
      , chrGuard        :: ![guard] 		-- subst -> Maybe subst
      , chrBody         :: ![cnstr]
      }
%%[[50
  deriving (Typeable, Data)
%%]]

emptyCHRGuard :: [a]
emptyCHRGuard = []
%%]

%%[(9 hmtyinfer || hmtyast)
instance Show (CHR c g s) where
  show _ = "CHR"
%%]

%%[(9 hmtyinfer || hmtyast)
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
          ppL xs  = ppBracketsCommasBlock xs -- ppParensCommasBlock xs
          ppChr l = vlist l -- ppCurlysBlock
%%]

%%[(9999 hmtyinfer || hmtyast)
instance Keyable cnstr => Keyable (CHR cnstr guard subst) where
  toKey chr = toKey $ head $ chrHead chr
%%]

%%[(9 hmtyinfer || hmtyast)
instance TTKeyable cnstr => TTKeyable (CHR cnstr guard subst) where
  toTTKey' o chr = toTTKey' o $ head $ chrHead chr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Var instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
instance (VarExtractable c v,VarExtractable g v) => VarExtractable (CHR c g s) v where
  varFreeSet          (CHR {chrHead=h, chrGuard=g, chrBody=b})
    = Set.unions $ concat [map varFreeSet h, map varFreeSet g, map varFreeSet b]

instance (VarUpdatable c s,VarUpdatable g s) => VarUpdatable (CHR c g s) s where
  varUpd s r@(CHR {chrHead=h, chrGuard=g, chrBody=b})
    = r {chrHead = map (varUpd s) h, chrGuard = map (varUpd s) g, chrBody = map (varUpd s) b}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHREmptySubstitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Capability to yield an empty substitution.

%%[(9 hmtyinfer || hmtyast) export(CHREmptySubstitution(..))
class CHREmptySubstitution subst where
  chrEmptySubst :: subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRMatchable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Matchable participates in the reduction process as a reducable constraint.

%%[(9 hmtyinfer || hmtyast) export(CHRMatchable(..))
class (TTKeyable x) => CHRMatchable env x subst where --- | x -> subst env where
  chrMatchTo      :: env -> subst -> x -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Checkable participates in the reduction process as a guard, to be checked.

%%[(9 hmtyinfer || hmtyast) export(CHRCheckable(..))
class CHRCheckable env x subst where
  chrCheck      :: env -> subst -> x -> Maybe subst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export((<==>), (==>), (|>))
infix   1 <==>, ==>
infixr  0 |>

(<==>), (==>) :: [c] -> [c] -> CHR c g s
hs <==>  bs = CHR hs (length hs) emptyCHRGuard bs
hs  ==>  bs = CHR hs 0 emptyCHRGuard bs

(|>) :: CHR c g s -> [g] -> CHR c g s
chr |> g = chr {chrGuard = chrGuard chr ++ g}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: ForceEval, Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer || hmtyast)
instance (ForceEval c, ForceEval g) => ForceEval (CHR c g s) where
  forceEval x@(CHR h sz g b) | forceEval h `seq` forceEval g `seq` forceEval b `seq` True = x
%%[[102
  fevCount (CHR h sz g b) = cm1 "CHR" `cmUnion` fevCount h `cmUnion` fevCount sz `cmUnion` fevCount g `cmUnion` fevCount b
%%]]
%%]

%%[(50 hmtyinfer || hmtyast)
instance (Serialize c,Serialize g,Serialize s) => Serialize (CHR c g s) where
  sput (CHR a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 CHR sget sget sget sget
%%]

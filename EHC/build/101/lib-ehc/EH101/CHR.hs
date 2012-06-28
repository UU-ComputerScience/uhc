module EH101.CHR
( module EH101.CHR.Key
, CHR (..)
, CHREmptySubstitution (..)
, CHRMatchable (..)
, CHRCheckable (..)
, (<==>), (==>), (|>) )
where
import qualified EH101.Base.TreeTrie as TreeTrie
import EH101.Base.Common
import EH101.Substitutable
import EH101.VarMp
import Data.Monoid
import qualified Data.Set as Set
import EH.Util.Pretty
import EH101.CHR.Key
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize



{-# LINE 33 "src/ehc/CHR.chs" #-}
-- | A CHR (rule) consist of head (simplification + propagation, boundary indicated by an Int), guard, and a body. All may be empty, but not all at the same time.
data CHR cnstr guard subst
  = CHR
      { chrHead     	:: ![cnstr]
      , chrSimpSz       :: !Int				-- length of the part of the head which is the simplification part
      , chrGuard        :: ![guard] 		-- subst -> Maybe subst
      , chrBody         :: ![cnstr]
      }
  deriving (Typeable, Data)

emptyCHRGuard :: [a]
emptyCHRGuard = []

{-# LINE 50 "src/ehc/CHR.chs" #-}
instance Show (CHR c g s) where
  show _ = "CHR"

{-# LINE 55 "src/ehc/CHR.chs" #-}
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

{-# LINE 77 "src/ehc/CHR.chs" #-}
instance TTKeyable cnstr => TTKeyable (CHR cnstr guard subst) where
  toTTKey' o chr = toTTKey' o $ head $ chrHead chr

{-# LINE 86 "src/ehc/CHR.chs" #-}
instance (VarExtractable c v,VarExtractable g v) => VarExtractable (CHR c g s) v where
  varFreeSet          (CHR {chrHead=h, chrGuard=g, chrBody=b})
    = Set.unions $ concat [map varFreeSet h, map varFreeSet g, map varFreeSet b]

instance (VarUpdatable c s,VarUpdatable g s) => VarUpdatable (CHR c g s) s where
  varUpd s r@(CHR {chrHead=h, chrGuard=g, chrBody=b})
    = r {chrHead = map (varUpd s) h, chrGuard = map (varUpd s) g, chrBody = map (varUpd s) b}

{-# LINE 102 "src/ehc/CHR.chs" #-}
class CHREmptySubstitution subst where
  chrEmptySubst :: subst

{-# LINE 113 "src/ehc/CHR.chs" #-}
class (TTKeyable x) => CHRMatchable env x subst where -- | x -> subst env where
  chrMatchTo      :: env -> subst -> x -> x -> Maybe subst

{-# LINE 124 "src/ehc/CHR.chs" #-}
class CHRCheckable env x subst where
  chrCheck      :: env -> subst -> x -> Maybe subst

{-# LINE 133 "src/ehc/CHR.chs" #-}
infix   1 <==>, ==>
infixr  0 |>

(<==>), (==>) :: [c] -> [c] -> CHR c g s
hs <==>  bs = CHR hs (length hs) emptyCHRGuard bs
hs  ==>  bs = CHR hs 0 emptyCHRGuard bs

(|>) :: CHR c g s -> [g] -> CHR c g s
chr |> g = chr {chrGuard = chrGuard chr ++ g}

{-# LINE 157 "src/ehc/CHR.chs" #-}
instance (Serialize c,Serialize g,Serialize s) => Serialize (CHR c g s) where
  sput (CHR a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 CHR sget sget sget sget

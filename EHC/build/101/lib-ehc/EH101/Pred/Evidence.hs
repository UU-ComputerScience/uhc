module EH101.Pred.Evidence
( Evidence (..)
, evidUnresolved
, evidUpdateUnresolved
, InfoToEvidenceMap, evidMpInsert, evidMpUnion, evidMpSubst )
where
import EH101.CHR
import EH101.Pred.CHR
import EH101.Substitutable
import EH101.Base.Common
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import EH.Util.Pretty
import EH.Util.Utils

{-# LINE 36 "src/ehc/Pred/Evidence.chs" #-}
data Evidence  p info
  =  Evid_Unresolved { evidPred :: !p                       , evidTrace     :: ![UnresolvedTrace p info]    }
  -- =  Evid_Unresolved { evidPred :: !p                       , evidAttempts  :: ![Evidence p info]           }
  |  Evid_Proof      { evidPred :: !p  , evidInfo :: !info  , evidProofSubs :: ![Evidence p info]           }
  |  Evid_Recurse    { evidPred :: !p                                                                       }
  |  Evid_Ambig      { evidPred :: !p                       , evidAmbigSubs :: ![(info,[Evidence p info])]  }

{-# LINE 45 "src/ehc/Pred/Evidence.chs" #-}
instance (Show info, Show p) => Show (Evidence p info) where
  show _ = "Evidence"

instance (Eq p, Eq info) => Eq (Evidence p info) where
  (Evid_Proof _ i1 evs1) == (Evid_Proof _ i2 evs2) = i1 == i2 && evs1 == evs2
  (Evid_Recurse p1     ) == (Evid_Recurse p2     ) = p1 == p2
  _                      == _                      = False

instance (Ord p, Ord info) => Ord (Evidence p info) where
  Evid_Unresolved _ _    `compare` _                      = LT
  _                      `compare` Evid_Unresolved _ _    = GT
  Evid_Proof _ i1 evs1   `compare` Evid_Proof _ i2 evs2   = orderingLexic (i1 `compare` i2 : zipWith compare evs1 evs2)
  Evid_Proof _ _  _      `compare` _                      = LT
  _                      `compare` Evid_Proof _ _  _      = GT
  Evid_Recurse p1        `compare` Evid_Recurse p2        = p1 `compare` p2
  Evid_Recurse _         `compare` _                      = LT
  _                      `compare` Evid_Recurse _         = GT

{-# LINE 65 "src/ehc/Pred/Evidence.chs" #-}
instance (PP info, PP p) => PP (Evidence p info) where
  pp (Evid_Proof _ info []) = "Ev:"             >#< info
  pp (Evid_Proof _ info es) = "Ev:"             >#< info >#< ppBracketsCommas' es
  pp (Evid_Recurse p      ) = "Ev: recurse:"    >#< p
  pp (Evid_Ambig _     ess) = "Ev: ambiguous:"  >#< ppBracketsCommas' (map (ppBracketsCommas . snd) ess)
  pp (Evid_Unresolved p  _) = "Ev: unresolved:" >#< p

{-# LINE 74 "src/ehc/Pred/Evidence.chs" #-}
instance VarExtractable p v => VarExtractable (Evidence p info) v where
  varFreeSet            (Evid_Unresolved  p   _ )    = varFreeSet p
  varFreeSet            (Evid_Proof       p _ es)    = Set.unions $ varFreeSet p : map varFreeSet es
  varFreeSet            (Evid_Recurse     p     )    = varFreeSet p
  varFreeSet            (Evid_Ambig       p  ess)    = Set.unions $ varFreeSet p : map (Set.unions . map varFreeSet . snd) ess

instance VarUpdatable p s => VarUpdatable (Evidence p info) s where
  varUpd s     (Evid_Unresolved  p   u )    = Evid_Unresolved (varUpd s p) u
  varUpd s     (Evid_Proof       p i es)    = Evid_Proof      (varUpd s p) i (map (varUpd s) es)
  varUpd s     (Evid_Recurse     p     )    = Evid_Recurse    (varUpd s p)
  varUpd s     (Evid_Ambig       p  ess)    = Evid_Ambig      (varUpd s p) (assocLMapElt (map (varUpd s)) ess)

{-# LINE 92 "src/ehc/Pred/Evidence.chs" #-}
-- | Get unresolved trace from evidence from which is known is belongs to unresolved predicate
evidUnresolved :: Eq p => Evidence p info -> [UnresolvedTrace p info]
evidUnresolved (Evid_Unresolved p us)	= -- concatMap evidUnresolved us
                                          us
evidUnresolved (Evid_Proof p i ps)
               | null us            	= []
               | otherwise          	= [UnresolvedTrace_Red p i us]
               where us = {- nub $ -} concatMap evidUnresolved ps
evidUnresolved (Evid_Ambig p  pss)
               | null us            	= []
               | otherwise          	= [UnresolvedTrace_Overlap p us]
               where us = {- nub $ concatMap -} map (\(i,ps) -> (i,concatMap evidUnresolved ps)) pss
evidUnresolved _                    	= []

{-# LINE 108 "src/ehc/Pred/Evidence.chs" #-}
-- | Choose proof over unresolved, to be used when patching evidence with newfound proofs
evidUpdateUnresolved :: Eq p => Evidence p info -> Evidence p info -> Evidence p info
evidUpdateUnresolved e                        (Evid_Unresolved _ _)= e
evidUpdateUnresolved (Evid_Proof p i qs)      e                    = Evid_Proof   p i [evidUpdateUnresolved q e | q <- qs]
evidUpdateUnresolved (Evid_Recurse p   )      e                    = Evid_Recurse p
evidUpdateUnresolved u@(Evid_Unresolved q _)  e     | q == evidPred e = e
                                                    | otherwise       = u

{-# LINE 118 "src/ehc/Pred/Evidence.chs" #-}
evidSubstUnresolved :: (p -> Maybe (Evidence p info)) -> Evidence p info -> Evidence p info
evidSubstUnresolved lkup ev
  = s ev
  where s ev = case ev of
                 Evid_Unresolved p _
                   | isJust mbev   -> fromJust mbev
                   where mbev = lkup p
                 Evid_Proof p i es -> Evid_Proof p i $ map s es
                 Evid_Ambig p  ess -> Evid_Ambig p $ assocLMapElt (map s) ess
                 _                 -> ev

{-# LINE 135 "src/ehc/Pred/Evidence.chs" #-}
type InfoToEvidenceMap p info = Map.Map info (Evidence p info)

evidMpInsert :: (Eq p, Ord info) => info -> Evidence p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpInsert = Map.insertWith evidUpdateUnresolved

evidMpUnion :: (Eq p, Ord info) => InfoToEvidenceMap p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpUnion = Map.unionWith evidUpdateUnresolved

evidMpSubst :: (p -> Maybe (Evidence p info)) -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpSubst lkup = Map.map (evidSubstUnresolved lkup)


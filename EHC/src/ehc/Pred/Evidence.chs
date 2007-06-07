%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence of Pred prove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[9 module {%{EH}Pred.Evidence} import({%{EH}CHR},{%{EH}Pred.CHR})
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[9 import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Representation of evidence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Evidence(..))
data Evidence  p info
  =  Evid_Unresolved !p
  |  Evid_Proof      !p  !info  ![Evidence p info]
  |  Evid_Ambig      !p         ![(info,[Evidence p info])]
%%]

%%[9
instance (Show info, Show p) => Show (Evidence p info) where
  show _ = "Evidence"

instance Eq info => Eq (Evidence p info) where
  (Evid_Proof _ i1 evs1) == (Evid_Proof _ i2 evs2) = i1 == i2 && evs1 == evs2
  _                      == _                      = False

instance Ord info => Ord (Evidence p info) where
  Evid_Unresolved _      `compare` _                      = LT
  _                      `compare` Evid_Unresolved _      = GT
  Evid_Proof _ i1 evs1   `compare` Evid_Proof _ i2 evs2   = orderingLexic (i1 `compare` i2 : zipWith compare evs1 evs2)
  Evid_Proof _ _  _      `compare` _                      = LT
  _                      `compare` Evid_Proof _ _  _      = GT
%%]

%%[9
instance (PP info, PP p) => PP (Evidence p info) where
  pp (Evid_Proof _ info []) = "Ev:" >#< info
  pp (Evid_Proof _ info es) = "Ev:" >#< info >#< ppBracketsCommas' es
  pp (Evid_Ambig _     ess) = "Ev: ambiguous:" >#< ppBracketsCommas' (map (ppBracketsCommas . snd) ess)
  pp (Evid_Unresolved p   ) = "Ev: unresolved:" >#< p
%%]

%%[9
instance CHRSubstitutable p v s => CHRSubstitutable (Evidence p info) v s where
  chrFtv            (Evid_Unresolved  p     )    = chrFtv p
  chrFtv            (Evid_Proof       p _ es)    = Set.unions $ chrFtv p : map chrFtv es
  chrFtv            (Evid_Ambig       p  ess)    = Set.unions $ chrFtv p : map (Set.unions . map chrFtv . snd) ess
  chrAppSubst s     (Evid_Unresolved  p     )    = Evid_Unresolved $ chrAppSubst s p
  chrAppSubst s     (Evid_Proof       p i es)    = Evid_Proof (chrAppSubst s p ) i (map (chrAppSubst s) es)
  chrAppSubst s     (Evid_Ambig       p  ess)    = Evid_Ambig (chrAppSubst s p ) (assocLMapElt (map (chrAppSubst s)) ess)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resolution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(evidUnresolved)
evidUnresolved :: Eq p => Evidence p info -> [p]
evidUnresolved (Evid_Unresolved p)  = [p]
evidUnresolved (Evid_Proof _ _ ps)  = nub $ concatMap evidUnresolved ps
evidUnresolved (Evid_Ambig _  pss)  = nub $ concatMap (concatMap evidUnresolved . snd) pss
%%]

%%[9 export(evidUpdateUnresolved)
evidUpdateUnresolved :: Eq p => Evidence p info -> Evidence p info -> Evidence p info
evidUpdateUnresolved e                      (Evid_Unresolved _)  = e
evidUpdateUnresolved (Evid_Proof p i qs)    e                    = Evid_Proof p i [evidUpdateUnresolved q e | q <- qs] 
evidUpdateUnresolved u@(Evid_Unresolved q)  e@(Evid_Proof p _ _)
                                                    | q == p     = e
                                                    | otherwise  = u
%%]

%%[9
evidSubstUnresolved :: (p -> Maybe (Evidence p info)) -> Evidence p info -> Evidence p info
evidSubstUnresolved lkup ev
  = s ev
  where s ev = case ev of
                 Evid_Unresolved p
                   | isJust mbev   -> fromJust mbev
                   where mbev = lkup p
                 Evid_Proof p i es -> Evid_Proof p i $ map s es
                 Evid_Ambig p  ess -> Evid_Ambig p $ assocLMapElt (map s) ess
                 _                 -> ev
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mapping: info -> evidence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(InfoToEvidenceMap,evidMpInsert,evidMpUnion,evidMpSubst)
type InfoToEvidenceMap p info = Map.Map info (Evidence p info)

evidMpInsert :: (Eq p, Ord info) => info -> Evidence p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpInsert = Map.insertWith evidUpdateUnresolved

evidMpUnion :: (Eq p, Ord info) => InfoToEvidenceMap p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpUnion = Map.unionWith evidUpdateUnresolved

evidMpSubst :: (p -> Maybe (Evidence p info)) -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpSubst lkup = Map.map (evidSubstUnresolved lkup)
%%]


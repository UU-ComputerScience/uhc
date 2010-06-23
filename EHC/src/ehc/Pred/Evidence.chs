%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence of Pred prove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[(9 hmtyinfer) module {%{EH}Pred.Evidence} import({%{EH}CHR},{%{EH}Pred.CHR})
%%]

%%[(9 hmtyinfer) import({%{EH}Base.Common})
%%]

%%[(9 hmtyinfer) import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[(9 hmtyinfer) import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[doesWhat doclatex
Evidence is the witness for predicates: a dictionary for class instances, an offset for lacking predicates, etc.
\begin{itemize}
\item \verb|Evid_Proof| represents the succesfully computed witness.
\item \verb|Evid_Recurse| represents the succesfully computed witness which is under construction.
\item \verb|Evid_Unresolved| represents a failed proof of a predicate proof obligation.
\item \verb|Evid_Ambig| represents overlapping instances.
\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Representation of evidence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Eviden

%%[(9 hmtyinfer) export(Evidence(..))
data Evidence  p info
  =  Evid_Unresolved { evidPred :: !p                                                                       }
  |  Evid_Proof      { evidPred :: !p  , evidInfo :: !info  , evidProofSubs :: ![Evidence p info]           }
  |  Evid_Recurse    { evidPred :: !p                                                                       }
  |  Evid_Ambig      { evidPred :: !p                       , evidAmbigSubs :: ![(info,[Evidence p info])]  }
%%]

%%[(9 hmtyinfer)
instance (Show info, Show p) => Show (Evidence p info) where
  show _ = "Evidence"

instance (Eq p, Eq info) => Eq (Evidence p info) where
  (Evid_Proof _ i1 evs1) == (Evid_Proof _ i2 evs2) = i1 == i2 && evs1 == evs2
  (Evid_Recurse p1     ) == (Evid_Recurse p2     ) = p1 == p2
  _                      == _                      = False

instance (Ord p, Ord info) => Ord (Evidence p info) where
  Evid_Unresolved _      `compare` _                      = LT
  _                      `compare` Evid_Unresolved _      = GT
  Evid_Proof _ i1 evs1   `compare` Evid_Proof _ i2 evs2   = orderingLexic (i1 `compare` i2 : zipWith compare evs1 evs2)
  Evid_Proof _ _  _      `compare` _                      = LT
  _                      `compare` Evid_Proof _ _  _      = GT
  Evid_Recurse p1        `compare` Evid_Recurse p2        = p1 `compare` p2
  Evid_Recurse _         `compare` _                      = LT
  _                      `compare` Evid_Recurse _         = GT
%%]

%%[(9 hmtyinfer)
instance (PP info, PP p) => PP (Evidence p info) where
  pp (Evid_Proof _ info []) = "Ev:"             >#< info
  pp (Evid_Proof _ info es) = "Ev:"             >#< info >#< ppBracketsCommas' es
  pp (Evid_Recurse p      ) = "Ev: recurse:"    >#< p
  pp (Evid_Ambig _     ess) = "Ev: ambiguous:"  >#< ppBracketsCommas' (map (ppBracketsCommas . snd) ess)
  pp (Evid_Unresolved p   ) = "Ev: unresolved:" >#< p
%%]

%%[(9 hmtyinfer)
instance CHRSubstitutable p v s => CHRSubstitutable (Evidence p info) v s where
  chrFtv            (Evid_Unresolved  p     )    = chrFtv p
  chrFtv            (Evid_Proof       p _ es)    = Set.unions $ chrFtv p : map chrFtv es
  chrFtv            (Evid_Recurse     p     )    = chrFtv p
  chrFtv            (Evid_Ambig       p  ess)    = Set.unions $ chrFtv p : map (Set.unions . map chrFtv . snd) ess
  chrAppSubst s     (Evid_Unresolved  p     )    = Evid_Unresolved (chrAppSubst s p)
  chrAppSubst s     (Evid_Proof       p i es)    = Evid_Proof      (chrAppSubst s p) i (map (chrAppSubst s) es)
  chrAppSubst s     (Evid_Recurse     p     )    = Evid_Recurse    (chrAppSubst s p)  
  chrAppSubst s     (Evid_Ambig       p  ess)    = Evid_Ambig      (chrAppSubst s p) (assocLMapElt (map (chrAppSubst s)) ess)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resolution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(evidUnresolved)
evidUnresolved :: Eq p => Evidence p info -> [p]
evidUnresolved (Evid_Unresolved p)  = [p]
evidUnresolved (Evid_Proof _ _ ps)  = nub $ concatMap evidUnresolved ps
evidUnresolved (Evid_Ambig _  pss)  = nub $ concatMap (concatMap evidUnresolved . snd) pss
evidUnresolved _                    = []
%%]

%%[(9 hmtyinfer) export(evidUpdateUnresolved)
-- | Choose proof over unresolved, to be used when patching evidence with newfound proofs
evidUpdateUnresolved :: Eq p => Evidence p info -> Evidence p info -> Evidence p info
evidUpdateUnresolved e                      (Evid_Unresolved _)  = e
evidUpdateUnresolved (Evid_Proof p i qs)    e                    = Evid_Proof   p i [evidUpdateUnresolved q e | q <- qs] 
evidUpdateUnresolved (Evid_Recurse p   )    e                    = Evid_Recurse p
evidUpdateUnresolved u@(Evid_Unresolved q)  e       | q == evidPred e = e
                                                    | otherwise       = u
%%]

%%[(9 hmtyinfer)
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

%%[(9 hmtyinfer) export(InfoToEvidenceMap,evidMpInsert,evidMpUnion,evidMpSubst)
type InfoToEvidenceMap p info = Map.Map info (Evidence p info)

evidMpInsert :: (Eq p, Ord info) => info -> Evidence p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpInsert = Map.insertWith evidUpdateUnresolved

evidMpUnion :: (Eq p, Ord info) => InfoToEvidenceMap p info -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpUnion = Map.unionWith evidUpdateUnresolved

evidMpSubst :: (p -> Maybe (Evidence p info)) -> InfoToEvidenceMap p info -> InfoToEvidenceMap p info
evidMpSubst lkup = Map.map (evidSubstUnresolved lkup)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation of Evidence (of Pred) to Core fragments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred.EvidenceToCore} import({%{EH}Pred.Evidence},{%{EH}Pred.CommonCHR})
%%]

%%[9 import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import({%{EH}Base.Common},{%{EH}Ty})
%%]

%%[9 import({%{EH}Ty.FitsIn}(FIEnv(..),FIIn(..)),{%{EH}Core},{%{EH}Core.Utils})
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%[9 import(EH.Util.Utils)
%%]

%%[9 import(Control.Monad.State)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

All code fragments are identified by a UID.

The translation to core yields:
- a set of bindings for each assumed predicate, together with the set of all assumptions which should be in scope, including the one defined.
  Each binding uses the assumed predicate (and the others in scope), and can safely be introduced when all assumed predicates are in scope.
- a set of bindings which do not depend on assumptions.

%%[9 export(EvidKeyToCBindMap)
type EvidKeyToCExprMap = Map.Map UID (CExpr,Set.Set (UID,PredScope))
type EvidKeyToCBindMap = Map.Map UID [CBind]

data ToCoreState p info
  = ToCoreState
      { tcsMp       :: Map.Map UID ToCoreRes
      , tcsEvMp     :: Map.Map (Evidence p info) ToCoreRes
      , tcsUniq     :: UID
      }

data ToCoreRes
  = ToCoreRes
      { tcrCExpr    :: CExpr
      , tcrUsed     :: Set.Set (UID,PredScope)
      }
%%]

%%[9 export(evidMpToCore)
evidMpToCore :: FIIn -> InfoToEvidenceMap CHRPredOcc RedHowAnnotation -> EvidKeyToCExprMap
evidMpToCore env evidMp
  = Map.map (\r -> (tcrCExpr r,tcrUsed r)) $ tcsMp
    $ foldr mke (ToCoreState Map.empty Map.empty (fiUniq env))
    $ Map.toList $ Map.map strip evidMp
  where mke (RedHow_ProveObl i _,ev) st = fst $ mk1 st (Just i) ev
        mk1 st mbevk ev@(Evid_Proof p info evs)
                      = ins (insk || isJust mbevk) (maybe evk id mbevk) evnm ev c (Set.unions (uses : map tcrUsed rs)) (st' {tcsUniq=u'})
                      where (st'@(ToCoreState {tcsUniq=u}),rs) = mkn st evs
                            c               = ann info $ map tcrCExpr rs
                            (u',evk,insk,evnm,uses)
                                            = case info of
                                                RedHow_ProveObl   i   _ -> (u,i,True,mkHNm i,Set.empty)
                                                RedHow_Assumption i n s -> (u,i,False,n,Set.singleton (i,s))
                                                _                       -> (u1,u2,True,mkHNm u2,Set.empty)
                                                                        where (u1,u2) = mkNewUID u
        mkn st        = foldr (\ev (st,rs) -> let (st',r) = mk1 st Nothing ev in (st',r:rs)) (st,[])
        mkv x         = mknm $ mkHNm x
        mknm          = CExpr_Var
        ins insk k evnm ev c uses st
                      = maybe (mkc r $ mkk (ToCoreRes c uses) st, r) (\r -> (mkk r st,vc r)) $ Map.lookup ev $ tcsEvMp st
                      where mkk c st = if insk then st {tcsMp = Map.insert k c $ tcsMp st} else st
                            mkc v st = st {tcsEvMp = Map.insert ev v $ tcsEvMp st}
                            v = mknm evnm
                            r = ToCoreRes v uses
                            vc r = case tcrCExpr r of
                                     CExpr_Var v -> r
                                     _           -> r {tcrCExpr = v}
        ann (RedHow_Assumption   _ n _) _     = mknm n
        ann (RedHow_ByInstance   n _ _) ctxt  = mknm n `mkCExprApp` ctxt
        ann (RedHow_BySuperClass n o t) [sub] = mkCExprSatSelsCase
                                                  (emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                  (Just $ hsnSuffix n "!") sub t
                                                  [(n,n,o)] Nothing (CExpr_Var n)
        strip (Evid_Proof _ RedHow_ByScope [ev]) = strip ev
        strip (Evid_Proof p i              evs ) = Evid_Proof p i (map strip evs)
        strip ev                                 = ev
%%]

%%[9 export(evidKeyCoreMpToBinds)
evidKeyCoreMpToBinds :: EvidKeyToCExprMap -> (EvidKeyToCBindMap,[CBind])
evidKeyCoreMpToBinds m
  = ( Map.unionsWith (++)
      $ map (\(b,uses)
               -> let deepestScope = fst . maximumBy (\(_,sc1) (_,sc2) -> sc1 `pscpCmpByLen` sc2) . Set.toList
                  in  Map.singleton (deepestScope uses) [b]
            )
      $ [ (CBind_Bind (mkHNm i) e,u) | (i,(e,u)) <- Map.toList dependentOnAssumes   ]
    ,   [  CBind_Bind (mkHNm i) e    | (i,(e,_)) <- Map.toList independentOfAssumes ]
    )
  where (independentOfAssumes, dependentOnAssumes) = Map.partition (\(_,uses) -> Set.null uses) m
%%]


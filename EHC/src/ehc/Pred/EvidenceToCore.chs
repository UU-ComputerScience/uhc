%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation of Evidence (of Pred) to Core fragments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred.EvidenceToCore} import({%{EH}Pred.Evidence},{%{EH}Pred.CommonCHR})
%%]

%%[9 import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import({%{EH}Base.Common},{%{EH}Ty})
%%]

%%[9 import({%{EH}Ty.FitsIn}(FIEnv(..),FIIn(..)),{%{EH}Core},{%{EH}Core.Utils},{%{EH}Core.Subst})
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

%%[9 export(EvidKeyToCBindMap,PredScopeToCBindMap)
type EvidKeyToCExprMap = Map.Map UID (CExpr,Set.Set (UID,PredScope),PredScope)
type EvidKeyToCBindMap = Map.Map UID [CBind]
type PredScopeToCBindMap = Map.Map PredScope [CBind]

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
      , tcrScope    :: PredScope
      }
%%]

%%[9 export(AmbigEvid(..))
data AmbigEvid
  = AmbigEvid
      { ambigevidPredOcc 	:: CHRPredOcc
      , ambigevidInfos   	:: [RedHowAnnotation]
      }
%%]

%%[9 export(evidKeyToCBindMapUnion,predScopeToCBindMapUnion)
evidKeyToCBindMapUnion :: EvidKeyToCBindMap -> EvidKeyToCBindMap -> EvidKeyToCBindMap
evidKeyToCBindMapUnion = Map.unionWith (++)

predScopeToCBindMapUnion :: PredScopeToCBindMap -> PredScopeToCBindMap -> PredScopeToCBindMap
predScopeToCBindMapUnion = Map.unionWith (++)
%%]

%%[9 export(evidMpToCore)
evidMpToCore :: FIIn -> InfoToEvidenceMap CHRPredOcc RedHowAnnotation -> (EvidKeyToCExprMap,[AmbigEvid])
evidMpToCore env evidMp
  = ( Map.map (\r -> (tcrCExpr r,tcrUsed r,tcrScope r)) $ tcsMp
      $ foldr mke (ToCoreState Map.empty Map.empty (fiUniq env))
      $ evidMp'
    , concat ambigs
    )
  where (evidMp',ambigs) = unzip [ ((i,ev3),as) | (i,ev) <- Map.toList evidMp, let (ev2,as) = splitAmbig ev ; ev3 = strip ev2 ]
        mke (RedHow_ProveObl i _,ev) st = fst $ mk1 st (Just i) ev
        mk1 st mbevk ev@(Evid_Proof p info evs)
                      = ins (insk || isJust mbevk) (maybe evk id mbevk) evnm ev c sc (Set.unions (uses : map tcrUsed rs)) (st' {tcsUniq=u'})
                      where (st'@(ToCoreState {tcsUniq=u}),rs) = mkn st evs
                            (c,sc)          = ann info rs
                            (u',evk,insk,evnm,uses)
                                            = case info of
                                                RedHow_ProveObl   i   _ -> (u,i,True,mkHNm i,Set.empty)
                                                RedHow_Assumption i n s -> (u,i,False,n,Set.singleton (i,s))
                                                _                       -> (u1,u2,True,mkHNm u2,Set.empty)
                                                                        where (u1,u2) = mkNewUID u
        mkn st        = foldr (\ev (st,rs) -> let (st',r) = mk1 st Nothing ev in (st',r:rs)) (st,[])
        mkv x         = mknm $ mkHNm x
        mknm          = CExpr_Var
        ins insk k evnm ev c sc uses st
                      = case Map.lookup ev $ tcsEvMp st of
                          Just r -> (        mkk r                     st,vr r)
                          _      -> (mkc r $ mkk (ToCoreRes c uses sc) st,   r)
                      where mkk r st = if insk then st {tcsMp = Map.insert k r $ tcsMp st} else st
                            mkc v st = st {tcsEvMp = Map.insert ev v $ tcsEvMp st}
                            v = mknm evnm
                            r = ToCoreRes (vc c v) uses sc
                            vr r = case tcrCExpr r of
                                     CExpr_Var _ -> r
                                     _           -> r {tcrCExpr = v}
                            vc c c' = case c of
                                        CExpr_Var _ -> c
                                        _           -> c'
        ann (RedHow_Assumption   _ n sc) _     = ( mknm n, sc )
        ann (RedHow_ByInstance   n _ sc) ctxt  = ( mknm n `mkCExprApp` map tcrCExpr ctxt, maximumBy pscpCmpByLen $ sc : map tcrScope ctxt )
        ann (RedHow_BySuperClass n o t ) [sub] = ( mkCExprSatSelsCase
                                                     (emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                     (Just $ hsnSuffix n "!") (tcrCExpr sub) t
                                                     [(n,n,o)] Nothing (CExpr_Var n)
                                                 , tcrScope sub
                                                 )
        strip (Evid_Proof _ RedHow_ByScope [ev]) = strip ev
        strip (Evid_Proof p i              evs ) = Evid_Proof p i (map strip evs)
        strip ev                                 = ev
        splitAmbig  (Evid_Proof p i es            ) = let (es',as) = splitAmbigs es in (Evid_Proof p i es',as)
        splitAmbig  (Evid_Ambig p   ess@((i,es):_)) = let (es',_ ) = splitAmbigs es in (Evid_Proof p i es',[AmbigEvid p (map fst ess)])
        splitAmbig  ev                              = (ev,[])
        splitAmbigs es                              = let (es',as) = unzip $ map splitAmbig es in (es',concat as)
%%]
                          Just r -> trp "XX" ("ev" >#< ev >#< insk >#< "k" >#< k >#< v >#< "r" >#< tcrCExpr r >#< tcrCExpr (vr r)) $ (        mkk r                  st,vr r)
                      = maybe (mkc r $ mkk (ToCoreRes c uses) st, r) (\r -> (mkk r st,vr r)) $ Map.lookup ev $ tcsEvMp st

%%[9 export(evidKeyCoreMpToBinds)
evidKeyCoreMpToBinds :: EvidKeyToCExprMap -> (EvidKeyToCBindMap,PredScopeToCBindMap)
evidKeyCoreMpToBinds m
  = ( Map.unionsWith (++)
      $ map (\(b,uses)
               -> let deepestScope = fst . maximumBy (\(_,sc1) (_,sc2) -> sc1 `pscpCmpByLen` sc2) . Set.toList
                  in  Map.singleton (deepestScope uses) [b]
            )
      $ [ (CBind_Bind (mkHNm i) e,u)    | (i,(e,u,_ )) <- Map.toList dependentOnAssumes   ]
    , Map.fromListWith (++)
      $ [ (sc,[CBind_Bind (mkHNm i) e]) | (i,(e,_,sc)) <- Map.toList independentOfAssumes ]
    )
  where (independentOfAssumes, dependentOnAssumes) = Map.partition (\(_,uses,_) -> Set.null uses) m
%%]

%%[9 export(evidKeyBindMpToCSubst)
evidKeyBindMpToCSubst :: EvidKeyToCBindMap -> CSubst
evidKeyBindMpToCSubst
  = uidCBindLLToCSubst . Map.toList
%%]


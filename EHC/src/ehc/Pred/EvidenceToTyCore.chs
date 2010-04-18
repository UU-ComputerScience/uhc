%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation of Evidence (of Pred) to TyCore fragments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) module {%{EH}Pred.EvidenceToTyCore} import({%{EH}Pred.Evidence},{%{EH}Pred.CommonCHR})
%%]

%%[(9 codegen) import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[(9 codegen) import({%{EH}Base.Common})
%%]

%%[(9 codegen hmtyinfer) import({%{EH}Ty.FitsInCommon2}(FIEnv(..),FIIn(..)),qualified {%{EH}TyCore.Full2} as C,{%{EH}Ty})
%%]

%%[(9 codegen) import(EH.Util.Pretty)
%%]

%%[(9 codegen) import(EH.Util.Utils)
%%]

%%[(9 codegen) import(Control.Monad.State)
%%]

%%[(9 codegen) import({%{EH}Base.Debug} as Debug)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sub evidence encodes the evidence needed to construct other evidence, as in Eq Int required for Eq [Int].
The subevidence is identified/introduced by a UID and defined in scope.
Subevidence can be an assumption (encoded below) or an already known instance (dealt with otherwise, but must be here too. 20090416)

%%[(9 codegen)
data SubEvid
  = SubEvid_Assume
      { subevdId    :: UID
      , subevdScope :: PredScope
      }
  deriving (Eq,Ord,Show)

instance PP SubEvid where
  pp (SubEvid_Assume i s) = "SubEvd" >#< i >#< s
%%]

All code fragments are identified by a UID.

The translation to core yields:
- a set of bindings for each assumed predicate, together with the set of all assumptions which should be in scope, including the one defined.
  Each binding uses the assumed predicate (and the others in scope), and can safely be introduced when all assumed predicates are in scope.
- a set of bindings which do not depend on assumptions.

%%[(9 codegen) export(EvidKeyToValBindMap,PredScopeToValBindMap)
type EvidKeyToExprMap = Map.Map UID (C.Expr,Set.Set SubEvid,PredScope)
type EvidKeyToValBindMap = Map.Map UID [C.ValBind]
type PredScopeToValBindMap = Map.Map PredScope [C.ValBind]

data ToCoreState p info
  = ToCoreState
      { tcsMp       :: !(Map.Map UID ToCoreRes)
      , tcsEvMp     :: !(Map.Map (Evidence p info) ToCoreRes)
      , tcsUniq     :: !UID
      }

data ToCoreRes
  = ToCoreRes
      { tcrExpr     :: !C.Expr
      , tcrUsed     :: !(Set.Set SubEvid)
      , tcrScope    :: !PredScope
      }
%%]

%%[(9 codegen)
instance Show ToCoreRes where
  show _ = "ToCoreRes"

instance PP ToCoreRes where
  pp r = "TCR" >#< tcrExpr r
%%]

%%[(9 codegen) export(AmbigEvid(..))
data AmbigEvid
  = AmbigEvid
      { ambigevidPredOcc 	:: !CHRPredOcc
      , ambigevidInfos   	:: ![RedHowAnnotation]
      }
%%]

%%[(9 codegen) export(evidKeyToValBindMapUnion,predScopeToValBindMapUnion)
evidKeyToValBindMapUnion :: EvidKeyToValBindMap -> EvidKeyToValBindMap -> EvidKeyToValBindMap
evidKeyToValBindMapUnion = Map.unionWith (++)

predScopeToValBindMapUnion :: PredScopeToValBindMap -> PredScopeToValBindMap -> PredScopeToValBindMap
predScopeToValBindMapUnion = Map.unionWith (++)
%%]

%%[(9 codegen) export(evidMpToCore,EvidKeyToExprMap)
evidMpToCore :: FIIn -> InfoToEvidenceMap CHRPredOcc RedHowAnnotation -> (EvidKeyToExprMap,[AmbigEvid])
evidMpToCore env evidMp
  = ( Map.map (\r -> (tcrExpr r,tcrUsed r,tcrScope r)) $ tcsMp
      $ foldr mke (ToCoreState Map.empty Map.empty (fiUniq env))
      $ evidMp'
    , concat ambigs
    )
  where (evidMp',ambigs) = dbg "evidMpToCore.mk1.evidMp'" $ unzip [ ((i,ev3),as) | (i,ev) <- filter (not.ignore) (Map.toList evidMp), let (ev2,as) = splitAmbig ev ; ev3 = strip ev2 ]
        mke (RedHow_ProveObl i _,ev) st = fst $ mk1 st (Just i) ev
        mk1 st mbevk ev@(Evid_Proof p info evs)
                      = dbg "evidMpToCore.mk1.a" $ ins (insk || isJust mbevk) evk evnm ev c sc (Set.unions (uses : map tcrUsed rs)) (st' {tcsUniq=u'})
                      where (st'@(ToCoreState {tcsUniq=u}),rs) = mkn st evs
                            (c,sc)          = ann info rs
                            (u',evk,insk,evnm,uses)
                                            = case info of
                                                RedHow_ProveObl   i   _ -> (u,choosek i,True,choosen $ mkHNm i,Set.empty)
                                                RedHow_Assumption vun s -> (u,choosek i,False,choosen (vunmNm vun),Set.singleton (SubEvid_Assume i s))
                                                                        where i = vunmId vun
                                                _                       -> (u1,choosek u2,True,choosen $ mkHNm u2,Set.empty)
                                                                        where (u1,u2) = mkNewUID u
                            choosek k = maybe k id mbevk
                            choosen n = maybe n mkHNm mbevk
        mk1 st _    _ = dbg "evidMpToCore.mk1.b" $ (st,ToCoreRes (C.tcUndefined $ feEHCOpts $ fiEnv env) Set.empty initPredScope)
        mkn st        = dbg "evidMpToCore.mkn" $ foldr (\ev (st,rs) -> let (st',r) = mk1 st Nothing ev in (st',r:rs)) (st,[])
        mkv x         = mknm $ mkHNm x
        mknm          = C.Expr_Var
        ins insk k evnm ev c sc uses st
                      = {- trp "XX" ((ppAssocLV $ Map.toList $ tcsMp st') >-< (ppAssocLV $ Map.toList $ tcsEvMp st')) $ -} res
                      where res@(st',_)
                              = case Map.lookup ev $ tcsEvMp st of
                                  Just r -> (        mkk r                     st,vr r)
                                  _      -> (mkc r $ mkk (ToCoreRes c uses sc) st,   r)
                            mkk r st = if insk then st {tcsMp = Map.insert k r $ tcsMp st} else st
                            mkc v st = st {tcsEvMp = Map.insert ev v $ tcsEvMp st}
                            v = mknm evnm
                            r = ToCoreRes (vc c v) uses sc
                            vr r = case tcrExpr r of
                                     C.Expr_Var _ -> r
                                     _           -> r {tcrExpr = v}
                            vc c c' = case c of
                                        C.Expr_Var _ -> c
                                        _           -> c'
        ann (RedHow_Assumption   vun sc) _     = ( mknm $ vunmNm vun, sc )
        ann (RedHow_ByInstance   n _ sc) ctxt  = ( C.mkExprAppMeta (mknm n) (map (\c -> (tcrExpr c,(C.MetaVal_Dict Nothing))) ctxt), maximumBy pscpCmpByLen $ sc : map tcrScope ctxt )
        ann (RedHow_BySuperClass n o t ) [sub] = ( C.mkExprSatSelsCaseMeta
                                                     (C.emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                     (Just (hsnUniqifyEval n,ty n)) (C.MetaVal_Dict (Just o)) (tcrExpr sub) t
                                                     [(n,o)] Nothing (C.Expr_Var n)
                                                 , tcrScope sub
                                                 )
                                               where ty x = C.tyErr ("evidMpToCore.RedHow_BySuperClass: " ++ show x)
%%[[10
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) []     = ( C.tcInt o, sc )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) [roff] = ( C.tcAddInt (feEHCOpts $ fiEnv env) (tcrExpr roff) o, sc )
%%]]
%%[[13
        ann (RedHow_Lambda  i sc) [body]       = ( [(mkHNm i,C.tyErr ("evidMpToCore.RedHow_Lambda: " ++ show i))] `C.mkExprLam` tcrExpr body, sc )
%%]]
%%[[16
        ignore (_, (Evid_Proof _ red  _))
          | red `elem` [RedHow_ByEqSymmetry, RedHow_ByEqTrans, RedHow_ByEqCongr, RedHow_ByPredSeqUnpack, RedHow_ByEqFromAssume, RedHow_ByEqIdentity]
          = True
        ignore (_, (Evid_Proof _ (RedHow_ByEqTyReduction _ _) _))
          = True
%%]]
        ignore _ = False

        strip (Evid_Proof _ (RedHow_ByScope _) [ev]) = strip ev
        strip (Evid_Proof p i                  evs ) = Evid_Proof p i (map strip evs)
        strip ev                                     = ev
        splitAmbig  (Evid_Proof p i es            )  = let (es',as) = splitAmbigs es in (Evid_Proof p i es',as)
        splitAmbig  (Evid_Ambig p   ess@((i,es):_))  = let (es',_ ) = splitAmbigs es in (Evid_Proof p i es',[AmbigEvid p (map fst ess)])
        splitAmbig  ev                               = (ev,[])
        splitAmbigs es                               = let (es',as) = unzip $ map splitAmbig es in (es',concat as)
        dbg m = id -- Debug.tr m empty
%%]
                          Just r -> trp "XX" ("ev" >#< ev >#< insk >#< "k" >#< k >#< v >#< "r" >#< tcrExpr r >#< tcrExpr (vr r)) $ (        mkk r                  st,vr r)
                      = maybe (mkc r $ mkk (ToCoreRes c uses) st, r) (\r -> (mkk r st,vr r)) $ Map.lookup ev $ tcsEvMp st


%%[(9 codegen)
getMetaDictMbPos :: C.Expr -> Maybe Int
getMetaDictMbPos (C.Expr_Let _ (C.ValBind_Val _ (Just (_,C.MetaVal_Dict m)) _ _ : _) _) = m
getMetaDictMbPos _ = Nothing
%%]

%%[(9 codegen) export(evidKeyCoreMpToBinds)
evidKeyCoreMpToBinds :: EvidKeyToExprMap -> (EvidKeyToValBindMap,PredScopeToValBindMap)
evidKeyCoreMpToBinds m
  = dbg "evidKeyCoreMpToBinds.res"
    $!
    ( dbg "evidKeyCoreMpToBinds.res1"
      $! Map.unionsWith (++)
      $ map (\(b,uses)
               -> let deepestScope = subevdId . maximumBy (\evd1 evd2 -> subevdScope evd1 `pscpCmpByLen` subevdScope evd2) . Set.toList
                  in  Map.singleton (deepestScope uses) [b]
            )
      $ [ (C.mkValBind1Meta (mkHNm i) (C.MetaVal_Dict (getMetaDictMbPos e)) (ty "1" i) e,u)    
        | (i,(e,u,_ )) <- dbg "evidKeyCoreMpToBinds.dependentOnAssumes"   $! Map.toList dependentOnAssumes   
        ]
    , dbg "evidKeyCoreMpToBinds.res2"
      $! Map.fromListWith (++)
      $ [ (sc,[C.mkValBind1Meta (mkHNm i) (C.MetaVal_Dict Nothing) (ty "2" i) e]) 
        | (i,(e,_,sc)) <- dbg "evidKeyCoreMpToBinds.independentOfAssumes" $! Map.toList independentOfAssumes 
        ]
    )
  where (independentOfAssumes, dependentOnAssumes)
          = dbg "evidKeyCoreMpToBinds.partition"
            $! Map.partition (\(_,uses,_) -> Set.null uses)
            $ dbg "evidKeyCoreMpToBinds.m"
            $! m
        dbg m = id -- Debug.tr m (pp m)
        ty m x = C.tyErr ("evidKeyCoreMpToBinds." ++ m ++ ": " ++ show x)
%%]

20090416.
The above code accidently swaps independentOfAssumes and dependentOnAssumes in the computation of maps.
The resulting code is ok, as long as it does not concern scope dependent instances, which is the case in Haskell98.
Below is -for now- at least the start to do it right.
The code using these mappings has to be checked as well.

Extract from the basic bindings for prove obligations the following:
- bindings independent of assumption or scope.
- foreach introduced assumption, the bindings required for depending prove obligations which only require that assumption.
- the rest, which depends on multiple assumptions, and thus can only be introduced at the deepest scope. Hence a map from scope to such bindings.

%%[(9 codegen) export(EvidValBindL,evidKeyCoreMpToBinds2)
type EvidValBindL = [C.ValBind]

evidKeyCoreMpToBinds2 :: EvidKeyToExprMap -> (EvidValBindL,EvidKeyToValBindMap,PredScopeToValBindMap)
evidKeyCoreMpToBinds2 m
  = (   [ mkd i e
        | (i,(e,_,_)) <- Map.toList independentOfAssumes
        ]
    , Map.unionsWith (++)
      $ [ Map.singleton (subevdId $ head $ Set.toList u) [mkd i e]
        | (i,(e,u,_)) <- Map.toList dependentOn1Assume
        ]
    , Map.unionsWith (++)
      $ [ Map.singleton (deepestScope sc u) [mkd i e]
        | (i,(e,u,sc)) <- Map.toList dependentOnNAssumes
        ]
    )
  where (independentOfAssumes, dependentOnAssumes)
          = Map.partition (\(_,uses,_) -> Set.null uses) m
        (dependentOn1Assume, dependentOnNAssumes)
          = Map.partition (\(_,uses,_) -> Set.size uses == 1) m
        mkd i e           = C.mkValBind1Meta (mkHNm i) (C.MetaVal_Dict (getMetaDictMbPos e)) (ty i) e
        deepestScope sc u = maximumBy pscpCmpByLen $ sc : (map subevdScope $ Set.toList u)
        ty x = C.tyErr ("evidKeyCoreMpToBinds2:" ++ show x)
%%]


%%[(9 codegen) export(evidKeyBindMpToSubst)
evidKeyBindMpToSubst :: EvidKeyToValBindMap -> C.CSubst
evidKeyBindMpToSubst
  = C.uidValBindLLToCSubst . Map.toList
%%]


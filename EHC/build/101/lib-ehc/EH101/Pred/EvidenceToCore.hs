module EH101.Pred.EvidenceToCore
( EvidKeyToCBindMap, PredScopeToCBindMap, EvidKeyToCExprMap
, OverlapEvid (..)
, evidKeyToBindMapUnion, predScopeToBindMapUnion
, evidMpToCore
, evidKeyCoreMpToBinds
, EvidCBindL, evidKeyCoreMpToBinds2
, evidKeyBindMpToCSubst )
where
import EH101.Pred.Evidence
import EH101.Pred.CommonCHR
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import EH101.Base.Common
import EH101.Ty.FitsInCommon2
import EH101.Core
import EH101.Ty
import EH101.Core.Utils
import EH101.Core.Subst
import EH101.AbstractCore
import EH.Util.Pretty
import EH.Util.Utils
import Control.Monad.State
import EH101.Base.Debug as Debug
import Debug.Trace



{-# LINE 50 "src/ehc/Pred/EvidenceToCore.chs" #-}
type EvidKeyToExprMap' e t = Map.Map UID (ToCoreRes e t) -- (e,t,Set.Set SubEvid,PredScope)
type EvidKeyToBindMap' b = Map.Map UID [b]
type PredScopeToBindMap' b = Map.Map PredScope [b]

type EvidKeyToCExprMap = EvidKeyToExprMap' CExpr Ty
type EvidKeyToCBindMap = EvidKeyToBindMap' CBind
type PredScopeToCBindMap = PredScopeToBindMap' CBind

{-# LINE 63 "src/ehc/Pred/EvidenceToCore.chs" #-}
data OverlapEvid p info
  = OverlapEvid
      { overlapevidPredOcc  :: !p
      , overlapevidInfos    :: ![info]
      }

{-# LINE 71 "src/ehc/Pred/EvidenceToCore.chs" #-}
evidKeyToBindMapUnion :: EvidKeyToBindMap' e -> EvidKeyToBindMap' e -> EvidKeyToBindMap' e
evidKeyToBindMapUnion = Map.unionWith (++)

predScopeToBindMapUnion :: PredScopeToBindMap' b -> PredScopeToBindMap' b -> PredScopeToBindMap' b
predScopeToBindMapUnion = Map.unionWith (++)

{-# LINE 83 "src/ehc/Pred/EvidenceToCore.chs" #-}
-- | Translate evidence to actual core, taking into account the need to share (i.e. do CSE, by state accumulating maps).
--   Cleanup dd 20120209, rewrite to State version.
evidMpToCore2 :: FIIn' gm -> InfoToEvidenceMap CHRPredOcc RedHowAnnotation -> (EvidKeyToCExprMap,[OverlapEvid CHRPredOcc RedHowAnnotation])
evidMpToCore2 env evidMp
  = ( -- Map.map (\r -> (tcrCExpr r,tcrTy r,tcrUsed r,tcrScope r)) $
      tcsMp
      $ execState (mapM (\(RedHow_ProveObl i _,ev) -> mk1 (Just i) ev) evidMp')
                  (ToCoreState Map.empty Map.empty Map.empty (fiUniq env))
    , concat overlaps
    )
  where -- | Split off overlapping instances (i.e. ambiguous evidence)
        (evidMp',overlaps)
          = unzip [ ((i,ev3),as)
                  | (i,ev) <- Map.toList evidMp, not (ignore ev)
                  , let (ev2,as) = splitOverlap ev ; ev3 = stripScope ev2
                  ]

        -- | Make Core for a single Evidence, recursing for subevidence
        mk1 :: Maybe UID									-- an UID already may have been assigned, it then should be used (the case for start evidence of this fun)
            -> Evidence CHRPredOcc RedHowAnnotation			-- the evidence
            -> State (ToCoreState CHRPredOcc RedHowAnnotation CExpr Ty) (ToCoreRes CExpr Ty)
        mk1 mbevk ev
          = case ev of
              Evid_Proof p info evs
                ->  do { st@(ToCoreState {tcsUniq=u}) <- get
                       ; let (u',evk,insk,evnm,uses)
                                = case info of
                                    RedHow_ProveObl   i   _ -> (u,choosek i,True,choosen $ mkHNm i,Set.empty)
                                    RedHow_Assumption vun s -> (u,choosek i,False,choosen (vunmNm vun),Set.singleton (SubEvid_Assume i s))
                                                            where i = vunmId vun
                                    _                       -> (u1,choosek u2,True,choosen $ mkHNm u2,Set.empty)
                                                            where (u1,u2) = mkNewUID u
                       ; modify (\st -> st {tcsUniq = u', tcsPrMp = Map.insert p evnm $ tcsPrMp st})
                       ; rs <- mapM (mk1 Nothing) evs
                       ; let (c,sc) = ann info rs
                       ; ins (insk || isJust mbevk)
                             evk evnm ev c (pred2DataTy $ cpoPr p) sc
                             (Set.unions (uses : map tcrUsed rs))
                       }
                where choosek k = maybe k id mbevk
                      choosen n = maybe n mkHNm mbevk
              Evid_Recurse p
                ->  do { st@(ToCoreState {tcsUniq=u}) <- get
                       ; let (u1,u2) = mkNewUID u
                             recnm = panicJust "evidMpToCore.Evid_Recurse" $ Map.lookup p $ tcsPrMp st
                       ; modify (\st -> st {tcsUniq = u1})
                       ; ins True
                             u2 (mkHNm u2) ev (mknm recnm) (pred2DataTy $ cpoPr p) (cpoScope p)
                             Set.empty
                       }
              ev
                ->  return (ToCoreRes (acoreBuiltinUndefined $ feEHCOpts $ fiEnv env)
                                      (pred2DataTy $ cpoPr $ evidPred ev)
                                      Set.empty initPredScope
                           )

        -- | Update state (insert given required ingredients) & compute actual result
        ins :: Bool                                     -- insert in UID map?
            -> UID                                      -- the UID for this evidence
            -> HsName                                   -- the name as used in Core for this evidence
            -> Evidence CHRPredOcc RedHowAnnotation     -- the evidence
            -> CExpr                                    -- its Core expr
            -> Ty                                       -- its type
            -> PredScope                                -- its scope
            -> Set.Set SubEvid                          -- its sub evidence
            -> State (ToCoreState CHRPredOcc RedHowAnnotation CExpr Ty) (ToCoreRes CExpr Ty)
        ins insk k evnm ev c ty sc uses
          = do { st <- get
               ; case Map.lookup ev $ tcsEvMp st of
                   Just r
                     -- Is already in map, update UID/key map
                     | insk
                       -> do { modify (mkk k r)
                             ; return (vr evnm r)
                             }

                     -- No map update required
                     | otherwise
                       -> return (vr evnm r)

                     -- Insert in both maps, i.e. UID and Evidence map
                   _ | insk
                       -> do { let r1 = ToCoreRes     c              ty uses sc
                                   r2 = ToCoreRes (vc c (mknm evnm)) ty uses sc
                             ; modify (mkk k r1 . mkc ev r2)
                             ; return r2
                             }

                     -- Update only Evidence map
                     | otherwise
                       -> do { let r2 = ToCoreRes (vc c (mknm evnm)) ty uses sc
                             ; modify (mkc ev r2)
                             ; return r2
                             }
               }
          where mkk k  r st = st {tcsMp   = Map.insert k  r $ tcsMp   st}
                mkc ev r st = st {tcsEvMp = Map.insert ev r $ tcsEvMp st}
                vr evnm r = case acoreExprMbVar $ tcrCExpr r of
                              Just _ -> r
                              _      -> r {tcrCExpr = mknm evnm}
                vc c c' = case acoreExprMbVar c of
                            Just _ -> c
                            _      -> c'
        mknm = acoreVar
        ann (RedHow_Assumption   vun sc) _     = ( mknm $ vunmNm vun, sc )
        ann (RedHow_ByInstance   n _   sc) ctxt= ( acoreApp (mknm n) (map (\c -> (tcrCExpr c)) ctxt), maximumBy pscpCmpByLen $ sc : map tcrScope ctxt )
        ann (RedHow_BySuperClass n o t ) [sub] = let res = acoreSatSelsCaseMetaTy
                                                             (emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                             (Just (hsnUniqifyEval n,acoreTyErr "evidMpToCore2.ann.RedHow_BySuperClass"))
                                                             acoreMetavalDfltDict
                                                             (tcrCExpr sub)
                                                             t
                                                             [(n,{-n,-}o)]
                                                             Nothing
                                                             (acoreVar n)
                                                 in ( res
                                                    , tcrScope sub
                                                    )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) []     = ( acoreInt o, sc )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) [roff] = ( acoreBuiltinAddInt (feEHCOpts $ fiEnv env) (tcrCExpr roff) o, sc )
        ann (RedHow_Lambda  i sc) [body]       = ( [mkHNm i] `acoreLam` tcrCExpr body, sc )

        -- | Ignore proofs without real evidence
        ignore _ = False

        -- | Strip off scope related reductions, no longer required
        stripScope (Evid_Proof _ (RedHow_ByScope _) [ev]) = stripScope ev
        stripScope (Evid_Proof p i                  evs ) = Evid_Proof p i (map stripScope evs)
        stripScope ev                                     = ev

        -- | Split off overlapping instances
        splitOverlap  (Evid_Proof p i es            )  = let (es',as) = splitOverlaps es in (Evid_Proof p i es',as)
        splitOverlap  (Evid_Ambig p   ess@((i,es):_))  = let (es',_ ) = splitOverlaps es in (Evid_Proof p i es',[OverlapEvid p (map fst ess)])
        splitOverlap  ev                               = (ev,[])
        splitOverlaps es                               = let (es',as) = unzip $ map splitOverlap es in (es',concat as)

{-# LINE 236 "src/ehc/Pred/EvidenceToCore.chs" #-}
-- | Translate evidence to actual core, taking into account the need to share (i.e. do CSE, by state accumulating maps).
--   Cleanup dd 20120209, rewrite to State version, plus abstraction over Core variant
{-
--   For now: breaks on pred2DataTy, which is not (yet) abstracted over
evidMpToCore3
  :: forall gm p e m b basp bcat mbind t pat pr pf a .
     (AbstractCore e m b basp bcat mbind t pat pr pf a)
     => FIIn' gm
     -> InfoToEvidenceMap p RedHowAnnotation
     -> ( EvidKeyToExprMap' e t
        , [OverlapEvid p RedHowAnnotation]
        )
evidMpToCore3 env evidMp
  = ( Map.map (\r -> (tcrCExpr r,tcrTy r,tcrUsed r,tcrScope r)) $ tcsMp $ snd
      $ runState (mapM (\(RedHow_ProveObl i _,ev) -> mk1 (Just i) ev) evidMp')
                 (ToCoreState Map.empty Map.empty Map.empty (fiUniq env))
    , concat overlaps
    )
  where -- | Split off overlapping instances (i.e. ambiguous evidence)
        (evidMp',overlaps)
          = unzip [ ((i,ev3),as)
                  | (i,ev) <- Map.toList evidMp, not (ignore ev)
                  , let (ev2,as) = splitOverlap ev ; ev3 = stripScope ev2
                  ]

        -- | Make Core for a single Evidence, recursing for subevidence
        mk1 :: Maybe UID									-- an UID already may have been assigned, it then should be used (the case for start evidence of this fun)
            -> Evidence p RedHowAnnotation								-- the evidence
            -> State (ToCoreState p RedHowAnnotation e t) (ToCoreRes e t)
        mk1 mbevk ev
          = case ev of
              Evid_Proof p info evs
                ->  do { st@(ToCoreState {tcsUniq=u}) <- get
                       ; let (u',evk,insk,evnm,uses)
                                = case info of
                                    RedHow_ProveObl   i   _ -> (u,choosek i,True,choosen $ mkHNm i,Set.empty)
                                    RedHow_Assumption vun s -> (u,choosek i,False,choosen (vunmNm vun),Set.singleton (SubEvid_Assume i s))
                                                            where i = vunmId vun
                                    _                       -> (u1,choosek u2,True,choosen $ mkHNm u2,Set.empty)
                                                            where (u1,u2) = mkNewUID u
                       ; modify (\st -> st {tcsUniq = u', tcsPrMp = Map.insert p evnm $ tcsPrMp st})
                       ; rs <- mapM (mk1 Nothing) evs
                       ; let (c,sc) = ann info rs
                       ; ins (insk || isJust mbevk)
                             evk evnm ev c (pred2DataTy $ cpoPr p) sc
                             (Set.unions (uses : map tcrUsed rs))
                       }
                where choosek k = maybe k id mbevk
                      choosen n = maybe n mkHNm mbevk
              Evid_Recurse p
                ->  do { st@(ToCoreState {tcsUniq=u}) <- get
                       ; let (u1,u2) = mkNewUID u
                             recnm = panicJust "evidMpToCore.Evid_Recurse" $ Map.lookup p $ tcsPrMp st
                       ; modify (\st -> st {tcsUniq = u1})
                       ; ins True
                             u2 (mkHNm u2) ev (mknm recnm) (pred2DataTy $ cpoPr p) (cpoScope p)
                             Set.empty
                       }
              ev
                ->  return (ToCoreRes (acoreBuiltinUndefined $ feEHCOpts $ fiEnv env)
                                      undefined -- (pred2DataTy $ cpoPr $ evidPred ev)
                                      Set.empty initPredScope
                           )

        -- | Update state (insert given required ingredients) & compute actual result
        ins :: Bool                                     -- insert in UID map?
            -> UID                                      -- the UID for this evidence
            -> HsName                                   -- the name as used in Core for this evidence
            -> Evidence p RedHowAnnotation		     				-- the evidence
            -> e                                    	-- its Core expr
            -> t                                       	-- its type
            -> PredScope                                -- its scope
            -> Set.Set SubEvid                          -- its sub evidence
            -> State (ToCoreState p RedHowAnnotation e t) (ToCoreRes e t)
        ins insk k evnm ev c ty sc uses
          = do { st <- get
               ; case Map.lookup ev $ tcsEvMp st of
                   Just r
                     -- Is already in map, update UID/key map
                     | insk
                       -> do { modify (mkk k r)
                             ; return (vr evnm r)
                             }

                     -- No map update required
                     | otherwise
                       -> return (vr evnm r)

                     -- Insert in both maps, i.e. UID and Evidence map
                   _ | insk
                       -> do { let r1 = ToCoreRes     c              ty uses sc
                                   r2 = ToCoreRes (vc c (mknm evnm)) ty uses sc
                             ; modify (mkk k r1 . mkc ev r2)
                             ; return r2
                             }

                     -- Update only Evidence map
                     | otherwise
                       -> do { let r2 = ToCoreRes (vc c (mknm evnm)) ty uses sc
                             ; modify (mkc ev r2)
                             ; return r2
                             }
               }
          where mkk k  r st = st {tcsMp   = Map.insert k  r $ tcsMp   st}
                mkc ev r st = st {tcsEvMp = Map.insert ev r $ tcsEvMp st}
                vr evnm r = case acoreExprMbVar $ tcrCExpr r of
                              Just _ -> r
                              _      -> r {tcrCExpr = mknm evnm}
                vc c c' = case acoreExprMbVar c of
                            Just _ -> c
                            _      -> c'
        mknm = acoreVar
        ann (RedHow_Assumption   vun sc) _     = ( mknm $ vunmNm vun, sc )
        ann (RedHow_ByInstance   n _   sc) ctxt= ( acoreApp (mknm n) (map (\c -> (tcrCExpr c)) ctxt), maximumBy pscpCmpByLen $ sc : map tcrScope ctxt )
        ann (RedHow_BySuperClass n o t ) [sub] = let res = acoreSatSelsCaseMetaTy
                                                             (emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                             (Just (hsnUniqifyEval n,Ty_Any))
                                                             acoreMetavalDfltDict
                                                             (tcrCExpr sub)
                                                             t
                                                             [(n,{-n,-}o)]
                                                             Nothing
                                                             (acoreVar n)
                                                 in ( res
                                                    , tcrScope sub
                                                    )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) []     = ( acoreInt o, sc )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) [roff] = ( acoreBuiltinAddInt (feEHCOpts $ fiEnv env) (tcrCExpr roff) o, sc )
        ann (RedHow_Lambda  i sc) [body]       = ( [mkHNm i] `acoreLam` tcrCExpr body, sc )

        -- | Ignore proofs without real evidence
        ignore _ = False

        -- | Strip off scope related reductions, no longer required
        stripScope (Evid_Proof _ (RedHow_ByScope _) [ev]) = stripScope ev
        stripScope (Evid_Proof p i                  evs ) = Evid_Proof p i (map stripScope evs)
        stripScope ev                                     = ev

        -- | Split off overlapping instances
        splitOverlap  (Evid_Proof p i es            )  = let (es',as) = splitOverlaps es in (Evid_Proof p i es',as)
        splitOverlap  (Evid_Ambig p   ess@((i,es):_))  = let (es',_ ) = splitOverlaps es in (Evid_Proof p i es',[OverlapEvid p (map fst ess)])
        splitOverlap  ev                               = (ev,[])
        splitOverlaps es                               = let (es',as) = unzip $ map splitOverlap es in (es',concat as)
-}

{-# LINE 398 "src/ehc/Pred/EvidenceToCore.chs" #-}
-- | State maintained during evid -> core
data ToCoreState p info e t
  = ToCoreState
      { tcsMp       :: !(Map.Map UID (ToCoreRes e t))                 	-- accumulating map: uid -> Core (+additional info)
      , tcsEvMp     :: !(Map.Map (Evidence p info) (ToCoreRes e t))   	-- accumulating map: evidence -> Core (+additional info)
      , tcsPrMp     :: !(Map.Map p HsName)                      		-- accumulating map: for recursive proof, names for predicates to be introduced
      , tcsUniq     :: !UID                                     		-- threaded unique id
      }

-- | The core expression, plus additional info
data ToCoreRes e t
  = ToCoreRes
      { tcrCExpr    :: !e                                   	-- evidence Core expr
      , tcrTy       :: !t                                       -- its type
      , tcrUsed     :: !(Set.Set SubEvid)                       -- which subevidence has been used to construct this evidence
      , tcrScope    :: !PredScope                               -- its scope
      }

instance Show (ToCoreRes e t) where
  show _ = "ToCoreRes"

instance (PP e, PP t) => PP (ToCoreRes e t) where
  pp r = "TCR" >#< tcrCExpr r >#< "::" >#< tcrTy r

{-# LINE 424 "src/ehc/Pred/EvidenceToCore.chs" #-}
-- | Sub evidence encodes the evidence needed to construct other evidence, as in Eq Int required for Eq [Int].
--   The subevidence is identified/introduced by a UID and defined in scope.
--   Subevidence can be an assumption (encoded below) or an already known instance (dealt with otherwise, but must be here too. 20090416)
data SubEvid
  = SubEvid_Assume
      { subevdId    :: UID
      , subevdScope :: PredScope
      }
  deriving (Eq,Ord,Show)

instance PP SubEvid where
  pp (SubEvid_Assume i s) = "SubEvd" >#< i >#< s

{-# LINE 441 "src/ehc/Pred/EvidenceToCore.chs" #-}
-- | Translate evidence to actual core, taking into account the need to share (i.e. do CSE)
evidMpToCore :: FIIn' gm -> InfoToEvidenceMap CHRPredOcc RedHowAnnotation -> (EvidKeyToCExprMap,[OverlapEvid CHRPredOcc RedHowAnnotation])
evidMpToCore = evidMpToCore2
{-
evidMpToCore env evidMp
  = ( Map.map (\r -> (tcrCExpr r,tcrTy r,tcrUsed r,tcrScope r)) $ tcsMp
      $ foldr mke (ToCoreState Map.empty Map.empty Map.empty (fiUniq env))
      $ evidMp'
    , concat ambigs
    )
  where (evidMp',ambigs)
                      = dbg "evidMpToCore.mk1.evidMp'" $
                        unzip [ ((i,ev3),as)
                              | (i,ev) <- Map.toList evidMp, not (ignore ev)
                              , let (ev2,as) = splitOverlap ev ; ev3 = stripScope ev2
                              ]
        mke (RedHow_ProveObl i _,ev) st = fst $ mk1 st (Just i) ev
        mk1 st mbevk ev@(Evid_Proof p info evs)
                      = dbg "evidMpToCore.mk1.a" $
                        ins (insk || isJust mbevk)
                            evk evnm ev c (pred2DataTy $ cpoPr p) sc
                            (Set.unions (uses : map tcrUsed rs))
                            (st' {tcsUniq=u'})
                      where (st'@(ToCoreState {tcsUniq=u}),rs) = mkn (st {tcsPrMp = Map.insert p evnm $ tcsPrMp st}) evs
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
        mk1 st@(ToCoreState {tcsUniq=u}) mbevk ev@(Evid_Recurse p)
                      = ins True
                            u2 (mkHNm u2) ev (mknm recnm) (pred2DataTy $ cpoPr p) (cpoScope p)
                            Set.empty
                            (st {tcsUniq=u1})
                      where (u1,u2) = mkNewUID u
                            recnm = panicJust "evidMpToCore.Evid_Recurse" $ Map.lookup p $ tcsPrMp st
        mk1 st _   ev = dbg "evidMpToCore.mk1.b" $ (st,ToCoreRes (acoreBuiltinUndefined $ feEHCOpts $ fiEnv env) (pred2DataTy $ cpoPr $ evidPred ev) Set.empty initPredScope)
        mkn st        = dbg "evidMpToCore.mkn" $ foldr (\ev (st,rs) -> let (st',r) = mk1 st Nothing ev in (st',r:rs)) (st,[])
        mkv x         = mknm $ mkHNm x
        mknm          = acoreVar
        ins insk k evnm ev c ty sc uses st
                      = {- trp "XX" ((ppAssocLV $ Map.toList $ tcsMp st') >-< (ppAssocLV $ Map.toList $ tcsEvMp st')) $ -} res
                      where res@(st',_)
                              = case Map.lookup ev $ tcsEvMp st of
                                  Just r -> (        mkk r                     st,vr r)
                                  _      -> (mkc r $ mkk (ToCoreRes c ty uses sc) st,   r)
                            mkk r st = if insk then st {tcsMp = Map.insert k r $ tcsMp st} else st
                            mkc v st = st {tcsEvMp = Map.insert ev v $ tcsEvMp st}
                            v = mknm evnm
                            r = ToCoreRes (vc c v) ty uses sc
                            vr r = case acoreExprMbVar $ tcrCExpr r of
                                     Just _ -> r
                                     _      -> r {tcrCExpr = v}
                            vc c c' = case acoreExprMbVar c of
                                        Just _ -> c
                                        _      -> c'
        ann (RedHow_Assumption   vun sc) _     = ( mknm $ vunmNm vun, sc )
        ann (RedHow_ByInstance   n _   sc) ctxt= ( acoreApp (mknm n) (map (\c -> (tcrCExpr c)) ctxt), maximumBy pscpCmpByLen $ sc : map tcrScope ctxt )
        ann (RedHow_BySuperClass n o t ) [sub] = let res = acoreSatSelsCaseMetaTy
                                                             (emptyRCEEnv $ feEHCOpts $ fiEnv env)
                                                             (Just (hsnUniqifyEval n,Ty_Any))
                                                             CMetaVal_Dict
                                                             (tcrCExpr sub)
                                                             t
                                                             [(n,{-n,-}o)]
                                                             Nothing
                                                             (acoreVar n)
                                                 in ( res
                                                    , tcrScope sub
                                                    )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) []     = ( acoreInt o, sc )
        ann (RedHow_ByLabel _ (LabelOffset_Off o) sc) [roff] = ( acoreBuiltinAddInt (feEHCOpts $ fiEnv env) (tcrCExpr roff) o, sc )
        ann (RedHow_Lambda  i sc) [body]       = ( [mkHNm i] `acoreLam` tcrCExpr body, sc )
        ignore _ = False

        stripScope (Evid_Proof _ (RedHow_ByScope _) [ev]) = stripScope ev
        stripScope (Evid_Proof p i                  evs ) = Evid_Proof p i (map stripScope evs)
        stripScope ev                                     = ev

        splitOverlap  (Evid_Proof p i es            )  = let (es',as) = splitOverlaps es in (Evid_Proof p i es',as)
        splitOverlap  (Evid_Ambig p   ess@((i,es):_))  = let (es',_ ) = splitOverlaps es in (Evid_Proof p i es',[OverlapEvid p (map fst ess)])
        splitOverlap  ev                               = (ev,[])
        splitOverlaps es                               = let (es',as) = unzip $ map splitOverlap es in (es',concat as)

        dbg m = id -- Debug.tr m empty
-}

{-# LINE 549 "src/ehc/Pred/EvidenceToCore.chs" #-}
evidKeyCoreMpToBinds :: EvidKeyToCExprMap -> (EvidKeyToCBindMap,PredScopeToCBindMap)
evidKeyCoreMpToBinds m
  = dbg "evidKeyCoreMpToBinds.res"
    $!
    ( dbg "evidKeyCoreMpToBinds.res1"
      $! Map.unionsWith (++)
      $ map (\(b,uses)
               -> let deepestScope = subevdId . maximumBy (\evd1 evd2 -> subevdScope evd1 `pscpCmpByLen` subevdScope evd2) . Set.toList
                  in  Map.singleton (deepestScope uses) [b]
            )
      $ [ (acoreBind1MetaTy (mkHNm i) CMetaVal_Dict (tcrTy r) (tcrCExpr r),tcrUsed r)
        | (i,r) <- dbg "evidKeyCoreMpToBinds.dependentOnAssumes"   $! Map.toList dependentOnAssumes
        ]
    , dbg "evidKeyCoreMpToBinds.res2"
      $! Map.fromListWith (++)
      $ [ (tcrScope r,[acoreBind1MetaTy (mkHNm i) CMetaVal_Dict (tcrTy r) (tcrCExpr r)])
        | (i,r) <- dbg "evidKeyCoreMpToBinds.independentOfAssumes" $! Map.toList independentOfAssumes
        ]
    )
  where (independentOfAssumes, dependentOnAssumes)
          = dbg "evidKeyCoreMpToBinds.partition"
            $! Map.partition (\r -> Set.null (tcrUsed r))
            $ dbg "evidKeyCoreMpToBinds.m"
            $! m
        dbg m = id -- Debug.tr m (pp m)

{-# LINE 588 "src/ehc/Pred/EvidenceToCore.chs" #-}
type EvidCBindL = [CBind]

evidKeyCoreMpToBinds2 :: EvidKeyToCExprMap -> (EvidCBindL,EvidKeyToCBindMap,PredScopeToCBindMap)
evidKeyCoreMpToBinds2 m
  = (   [ mkd i (tcrCExpr r) (tcrTy r)
        | (i,r) <- Map.toList independentOfAssumes
        ]
    , Map.unionsWith (++)
      $ [ Map.singleton (subevdId $ head $ Set.toList $ tcrUsed r) [mkd i (tcrCExpr r) (tcrTy r)]
        | (i,r) <- Map.toList dependentOn1Assume
        ]
    , Map.unionsWith (++)
      $ [ Map.singleton (deepestScope (tcrScope r) (tcrUsed r)) [mkd i (tcrCExpr r) (tcrTy r)]
        | (i,r) <- Map.toList dependentOnNAssumes
        ]
    )
  where (independentOfAssumes, dependentOnAssumes)
          = Map.partition (\r -> Set.null $ tcrUsed r) m
        (dependentOn1Assume, dependentOnNAssumes)
          = Map.partition (\r -> Set.size (tcrUsed r) == 1) m
        mkd i e t         = acoreBind1MetaTy (mkHNm i) CMetaVal_Dict t e
        deepestScope sc u = maximumBy pscpCmpByLen $ sc : (map subevdScope $ Set.toList u)

{-# LINE 614 "src/ehc/Pred/EvidenceToCore.chs" #-}
evidKeyBindMpToCSubst :: EvidKeyToCBindMap -> CSubst
evidKeyBindMpToCSubst
  = acoreCSubstFromUidBindLL . Map.toList


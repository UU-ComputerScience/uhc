-- $Id: Ruler.ag 231 2005-06-07 14:39:41Z atze $

-------------------------------------------------------------------------
-- Supporting functions for admin building
-------------------------------------------------------------------------

%%[1 hs module (MkAdmin)
%%]

%%[1 hs export (bldDtInfo, bldScInfo, bldRsInfo)
%%]

%%[1 hs import (Maybe, qualified Data.Set as Set, qualified Data.Map as Map, Data.List, EH.Util.Nm, EH.Util.Utils)
%%]

%%[1 hs import (KeywParser( propsSynInhMp ), Opts, Err, Common, Expr.Utils, ARule.Utils( exprSubst ), ViewSel.Utils)
%%]

%%[1 hs import (FmGam, Utils, Admin)
%%]

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

%%[1 hs
prevWRTDpd :: Nm -> DpdGr Nm -> Map.Map Nm v -> v -> v
prevWRTDpd n g m v
  = maybeHd v (\n -> maybe v id . Map.lookup n $ m) (dgDpdsOn g n)
%%]

-------------------------------------------------------------------------
-- Data/AST
-------------------------------------------------------------------------

%%[1 hs
data BldDtState
  = BldDtState
      { bdDtAltGam     	:: DtAltGam
      }

emptyBldDtState
  = BldDtState
      { bdDtAltGam     	= emptyGam
      }

bldDtInfo :: DpdGr Nm -> DtInfo -> (DtInfo,[Err])
bldDtInfo vwDpdGr dtInfo
  = (dtInfo {dtVwGam = g},e)
  where (g,_,e)
          = foldr
              (\vwNm (vdGam,bdMp,errs)
                -> let bd = prevWRTDpd vwNm vwDpdGr bdMp emptyBldDtState
                       (vwDtInfo,bldErrs)
                         = case gamLookup vwNm vdGam of
                             Just i
                               -> (i {vdFullAltGam = g},[])
                               where g = vdAltGam i `gamUnionShadow` bdDtAltGam bd
                             Nothing
                               -> (emptyDtVwInfo {vdNm = vwNm, vdFullAltGam = bdDtAltGam bd},[])
                       bd' = bd {bdDtAltGam = vdFullAltGam vwDtInfo}
                       vdGam' = gamInsertShadow vwNm vwDtInfo vdGam
                       bdMp' = Map.insert vwNm bd' bdMp
                   in  (vdGam', bdMp', bldErrs ++ errs)
              )
              (dtVwGam dtInfo,Map.empty,[])
              (dgTopSort vwDpdGr)
%%]

-------------------------------------------------------------------------
-- Scheme
-------------------------------------------------------------------------

%%[1 hs
type ScmAtBldGam = Gam Nm [(Nm, AtInfo)]

data BldScmState
  = BldScmState
      { bsAtGam     :: ScmAtBldGam
      , bsAtBldGam  :: AtGam
      , bsAtBldL    :: [ScAtBld]
      , bsJdShpGam  :: JdShpGam Expr
      , bsExGam     :: ExplGam Expr
      }

emptyBldScmState
  = BldScmState
      { bsAtGam     = emptyGam
      , bsAtBldGam  = emptyGam
      , bsAtBldL    = []
      , bsJdShpGam  = emptyGam
      , bsExGam     = emptyGam
      }

bldNewScVw :: String -> ScGam Expr -> [ScAtBld] -> VwScInfo Expr -> (ScmAtBldGam,AtGam,[Err])
bldNewScVw cx scGam prevAtBldL vw
  = (gaNew,gbNew,bldErr)
  where (gaNew,gbNew,bldErr) = mkAtGam (vwscAtBldL vw) gaPrv gbPrv
        (gaPrv,gbPrv,_     ) = mkAtGam prevAtBldL emptyGam emptyGam
        mkAtGam atBldL ga gb
          = foldl
              (\(ga,gb,e) b
                 -> case b of
                      ScAtBldDirect g
                        -> (mkPropForGam g2 `gamUnionShadow` ga,g2 `gamUnionShadow` gb,gamCheckDups emptySPos cx "hole" g ++ errChk ++ e)
                        where (g2,errChk) = chkUpdNewAts g gb
                      ScAtBldScheme frScNm pos rnL
                        -> case scVwGamLookup frScNm (vwscNm vw) scGam of
                             Just (frScInfo,frVwScInfo)
                               -> ( mkPropForGam g3 `gamUnionShadow` ga, g3 `gamUnionShadow` gb, errUndefHls ++ errChk ++ e )
                               where errUndefHls
                                       = if null undefNmL then [] else [Err_UndefNm pos ("use of scheme `" ++ show frScNm ++ "` for hole definition for " ++ cx) "hole" undefNmL]
                                     (g2,undefNmL)
                                       = sabrGamRename rnL (vwscFullAtBldGam frVwScInfo)
                                     (g3,errChk) = chkUpdNewAts g2 gb
                             _ -> (ga,gb,[Err_UndefNm pos ("hole definition for " ++ cx) "scheme" [frScNm]] ++ e)
              )
              (ga,gb,[])
              atBldL
        chkUpdNewAts gNew g
          = gamFold
              (\i ge@(gNew,e)
                -> case gamLookup (atNm i) g of
                     Just j
                       | isExternNew
                         -> (gamDelete (atNm i) gNew,e)
                       | otherwise
                         -> ge
                       where isExternNew = AtExtern `atHasProp` i
                             isExternOld = AtExtern `atHasProp` j
                     Nothing
                       -> ge
              )
              (gNew,[])
              gNew
        mkPropForGam
          = gamMapWithKey
              (\n a
                 -> case [AtThread] `atFilterProps` a of
                      (_:_) -> [ (ns,AtInfo ns [AtSyn] (atProps a) (atTy a)), (ni,AtInfo ni [AtInh] (atProps a) (atTy a)) ]
                            where ns = nmSetSuff n "syn"
                                  ni = nmSetSuff n "inh"
                      _     -> [(n,a)]
              )

bldScInfo :: DpdGr Nm -> ScGam Expr -> ScInfo Expr -> (ScInfo Expr,[Err])
bldScInfo vwDpdGr scGam si@(ScInfo pos nm mbAGNm scKind vwScGam)
  = (si {scVwGam = g},e)
  where (g,_,e)
          = foldr
              (\nVw (vsg,bsMp,errs)
                  -> let bs = prevWRTDpd nVw vwDpdGr bsMp emptyBldScmState
                         cx = "scheme '" ++ show nm ++ "'"
                         (vw,bs',bldErrs)
                           = case gamLookup nVw vsg of
                               Just vw
                                 -> ( vw
                                    , bs
                                        { bsAtGam       = newAtGam
                                        , bsAtBldGam    = newBldGam
                                        , bsAtBldL      = bsAtBldL bs ++ vwscAtBldL vw
                                        , bsJdShpGam    = vwscJdShpGam vw `jdshpgUnionShadow` bsJdShpGam bs
                                        , bsExGam       = vwscExplGam vw `gamUnionShadow` bsExGam bs
                                        }
                                    , errDups ++ bldErr
                                    )
                                 where (newAtGam,newBldGam,bldErr) = bldNewScVw cx scGam (bsAtBldL bs) vw
                                       errDups = gamCheckDups pos cx "judgespec/use" (vwscJdShpGam vw)
                                                 ++ gamCheckDups pos cx "explanation" (vwscExplGam vw)
                               Nothing
                                 -> ( vw
                                    , bs
                                        { bsAtGam       = newAtGam
                                        , bsAtBldGam    = newBldGam
                                        }
                                    , bldErr
                                    )
                                 where vw = emptyVwScInfo { vwscNm = nVw }
                                       (newAtGam,newBldGam,bldErr) = bldNewScVw cx scGam (bsAtBldL bs) vw
                         vwag = gamFromAssocs . concat . gamElemsShadow $ bsAtGam bs'
                         bsMp' = Map.insert nVw bs' bsMp
                     in  ( gamInsertShadow nVw (vw {vwscFullAtGam = vwag, vwscFullAtBldGam = bsAtBldGam bs', vwscJdShpGam = bsJdShpGam bs', vwscFullAtBldL = bsAtBldL bs', vwscExplGam = bsExGam bs'}) vsg
                         , bsMp'
                         , bldErrs ++ errs
                         )
              )
              (vwScGam,Map.empty,[])
              (dgTopSort vwDpdGr)
%%]

-------------------------------------------------------------------------
-- Rule set building, util functions
-------------------------------------------------------------------------

%%[1 hs
-- attr directions for names in gam
gamAtDirMp :: VwScInfo e -> Gam Nm v -> Map.Map Nm [AtDir]
gamAtDirMp vi g = gamToMap $ gamMapWithKey (\n _ -> maybe [] atDirs . gamLookup n . vwscFullAtGam $ vi) $ g

-- split attr dir map into sets of syn/inh attrs
atDirMpSynInh :: Map.Map Nm [AtDir] -> (Set.Set Nm,Set.Set Nm)
atDirMpSynInh m
  = Map.foldWithKey (\n d (s,i) -> (if AtSyn `elem` d then Set.insert n s else s
                                   ,if AtInh `elem` d then Set.insert n i else i))
                    (Set.empty,Set.empty) m

-- union of all judge attr defs in a set (of names with a specific direction)
jaGamUseInS :: JAGam e -> Set.Set Nm -> Set.Set Nm
jaGamUseInS g s = Set.unions [ jaNmS i | (n,i) <- gamAssocsShadow g, n `Set.member` s ]

-- default attr gam of judgement, based on scheme
jaGamDflt :: Bool -> (Nm -> Expr) -> Nm -> Nm -> ScGam Expr -> JAGam Expr
jaGamDflt isPre mkE sn nVw scGam
  = case scVwGamLookup sn nVw scGam of
      Just (_,vi) -> gamMapWithKey (\n ai -> mkJAInfo n (mk n ai)) . vwscFullAtGam $ vi
      Nothing     -> emptyGam
  where mk n ai
          = if atIsExternUndef isPre ai
            then Expr_Undefined
            else mkE n

-- determine sets if inh/syn var's
reGamUpdInOut :: Nm -> ScGam e -> REGam e -> REGam e
reGamUpdInOut nVw scGam pg
  = gamMap
       (\i ->
           case i of
               REInfoJudge _ sn _ _ jg | isJust mvi
                 -> i  {reInNmS = jaGamUseInS jg aInhS, reOutNmS = jaGamUseInS jg aSynS}
                 where mvi = scVwGamLookup sn nVw scGam
                       aDirMp = gamAtDirMp (snd . maybe (panic "reGamUpdInOut") id $ mvi) jg
                       (aSynS,aInhS) = atDirMpSynInh aDirMp
               _ -> i
       )
       pg
%%]

-------------------------------------------------------------------------
-- Judgements building, based on scheme description
-------------------------------------------------------------------------

%%[1 hs
type JdBldInfo = (REInfo Expr,Maybe ([ScAtBldRename],Nm,Nm,VwScInfo Expr,VwScInfo Expr))
type ScRnmMp = Map.Map Nm Nm
type RlJdBldInfo = (RlJdBld Expr,Maybe (VwRlInfo Expr,ScAtBld,Gam Nm JdBldInfo,Gam Nm JdBldInfo))

checkJdAndAtBldL :: SPos -> String -> ScGam Expr -> RsGam Expr -> Nm -> Nm -> [ScAtBld] -> [RlJdBld Expr] -> ([RlJdBldInfo],[Err])
checkJdAndAtBldL pos cx scGam rsGam postScNm vwNm atBldL jdBldL
  = (jdBldL2,errFirst [e1])
  where (jdBldL2,atBldL2,e1)
          = foldr
              (\b i@(jbL,abL,e)
                -> case b of
                     RlJdBldFromRuleset pos rsNm rlNm scRnmL
                       -> ((b,Just (vwRlInfo,maybeHd emptyScAtBld id abRsL,preb,postb)):jbL,{-abRestL-}abL,errFirst [e1,e2,e3,e4++e5] ++ e)
                       where ((rsInfo,rlInfo,vwRlInfo),e1)
                                         = maybe ((emptyRsInfo,emptyRlInfo,emptyVwRlInfo)
                                                 ,[Err_UndefNm pos (cx ++ " build item ruleset '" ++ show rsNm ++ "' rule '" ++ show rlNm ++ "'") ("ruleset+rule") [rsNm, rlNm]]
                                                 )
                                                 (\i -> (i,[]))
                                           $ rsRlVwGamLookup rsNm rlNm vwNm rsGam
                             scNm        = rsScNm rsInfo
                             ((scInfo,vwScInfo),e2)
                                         = maybe ((emptyScInfo,emptyVwScInfo),[Err_UndefNm pos cx "scheme" [scNm]]) (\i -> (i,[]))
                                           $ scVwGamLookup scNm vwNm scGam
                             (abRsL,abRestL)
                                         = partition (isOkSc scNm) abL
                             isOkSc scNm (ScAtBldScheme n _ _) = n == scNm
                             isOkSc scNm _                     = False
                             e3          = if null abRsL then [Err_UndefNm pos cx "scheme build item" [scNm]] else []
                             scRnmMp     = Map.insert scNm postScNm $ Map.fromList [ (brNmFrom i,brNmTo i) | i <- scRnmL ]
                             gx jdg      = gamFoldWithKey
                                             (\jn i (g,e)
                                               -> case reMbJd i of
                                                    Just i
                                                      -> (gamInsert jn (i,Just (b,scNmFr,scNmTo,vwScInfoFr,vwScInfoTo)) g,errFirst [e1,e2] ++ e)
                                                      where scNmFr = reScNm i
                                                            scNmTo = Map.findWithDefault scNmFr scNmFr scRnmMp
                                                            (e2,(_,vwScInfoTo))
                                                              = maybe ([Err_UndefNm pos cx "scheme" [scNmTo]],(emptyScInfo,emptyVwScInfo)) ((,) [])
                                                                $ scVwGamLookup scNmTo vwNm scGam
                                                            vwScInfoFr = maybe emptyVwScInfo snd $ scVwGamLookup scNmFr vwNm scGam
                                                            b = concat $ map sabRenameL $ sabFilterScheme $ vwscFullAtBldL vwScInfoTo
                                                    _ -> (gamInsert jn (i,Nothing) g,e)
                                             )
                                             (emptyGam,[])
                                             jdg
                             (preb ,e4)  = gx (vwrlFullNoDfltPreGam  vwRlInfo)
                             (postb,e5)  = gx (vwrlFullNoDfltPostGam vwRlInfo)
{-
-}
                     _ -> ((b,Nothing):jbL,abL,e)
              )
              ([],atBldL,[])
              jdBldL

bldJdsFromRlBlds :: Nm -> Nm -> ScGam Expr -> [RlJdBldInfo] -> (REGam Expr,REGam Expr,[Err])
bldJdsFromRlBlds scNm vwNm scGam rlBldInfoL
  = r
  where r@(preg,postg,e)
          = foldl
              (\g@(preg,postg,e) b
                -> case b of
                     (RlJdBldFromRuleset _ rsNm rlNm scRnmL,Just (vwRlInfo,scAtBld,jprebg,jpostbg))
                       -> (bpreg `reGamUnionShadow` preg,bpostg `reGamUnionShadow` postg,be++e)
                       where (bpreg,bpostg,be)
                               = case scAtBld of
                                   ScAtBldScheme _ _ _
                                     -> (gamMap (upd2 True) jprebg,gamMap (upd2 False) jpostbg,[])
                                     where upd2 isPre (i,Just (rnL,scNmFr,scNmTo,vwScInfoFr,vwScInfoTo))
                                             = i {reScNm = scNmTo, reJAGam = fst $ sabrGamRename rnL $ fst $ chkUpdNewAts isPre mkg vwScInfoFr $ reJAGam i}
                                             where mkg i = gamDelete (jaNm i)
                                           upd2 isPre (i,_)
                                             = i
                     (RlJdBldDirect _ dpreg dpostg,_)
                       -> (mkjg True dpreg `reGamUnionShadow` preg,mkjg False dpostg `reGamUnionShadow` postg,e)
                       where mkjg isPre
                               = gamMap mk
                               where mk i
                                       = reUpdJAGam (fst $ chkUpdNewAts isPre mkg vwScInfo $ reJAGam i) i
                                       where (_,vwScInfo)
                                               = maybe (emptyScInfo,emptyVwScInfo) id $ scVwGamLookup (reScNm i) vwNm scGam
                                             mkg i = gamInsertShadow (jaNm i) (i {jaExpr = Expr_Undefined})
              )
              (emptyGam,emptyGam,[])
              rlBldInfoL
        chkUpdNewAts isPre mkg vwScInfo gNew
          = gamFold
              (\i ge@(gNew,e)
                -> case gamLookup (jaNm i) g of
                     Just j
                       | atIsExternUndef isPre j
                         -> (mkg i gNew,e)
                       | otherwise
                         -> ge
                     Nothing
                       -> ge
              )
              (gNew,[])
              gNew
          where g = vwscAtGam vwScInfo

bldDfltForJds :: Nm -> ScGam Expr -> (REGam Expr,REGam Expr) -> (REGam Expr,REGam Expr)
bldDfltForJds vwNm scGam (preg,postg)
  = (mkjg True preg, mkjg False postg)
  where mkag isPre sn = jaGamDflt isPre Expr_Var sn vwNm scGam
        mkjg isPre = gamMap (\i -> i {reJAGam = mkag isPre (reScNm i)}) . reGamFilterOutDel
%%]

-------------------------------------------------------------------------
-- Rule set building, top function
-- build views of a rule by extending each view along view order dependency
-------------------------------------------------------------------------

%%[1 hs
data BldRlsState
  = BldRlsState
      { brPreGam        :: REGam Expr
      , brPostGam       :: REGam Expr
      , brJdBldL        :: [RlJdBld Expr]
      , brRlChGam       :: RlChGam
      }

emptyBldRlsState
  = BldRlsState
      { brPreGam        = emptyGam
      , brPostGam       = emptyGam
      , brJdBldL        = []
      , brRlChGam       = emptyGam
      }

rlGamUpdVws :: String -> Opts -> DpdGr Nm -> Set.Set Nm -> DtInvGam -> ScGam Expr -> RsGam Expr -> RlGam Expr -> RsInfo Expr -> RlInfo Expr -> (RlInfo Expr,[Err])
rlGamUpdVws cxRs opts vwDpdGr extNmS dtInvGam scGam rsGam rlGam rsInfo rlInfo
  = let vwSel
          = vs `Set.intersection` rsInclVwS rsInfo
          where vs = case rlMbInclVwS rlInfo of
                       Just s -> s
                       _      -> case dtInvGamRlVwS (rsScNm rsInfo) (rlNm rlInfo) dtInvGam of
                                   Just vs -- | optGenFM opts /= FmTeX
                                           -> vs
                                   _       -> dgVertices vwDpdGr
        vwIsIncl n = n `Set.member` vwSel
        doMarkChngForVw
          = case optMbMarkChange opts of
              Just vs
                -> \vw -> (vw `Set.member` vs',dgIsFirst vwDpdGr vw vs')
                where vs' = viewSelsNmS vwDpdGr vs `Set.intersection` vwSel
              _ -> const (False,False)
        (g,_,eg,errs)
            = foldr
                (\nVw (vrg,brMp,errg,errs)
                  -> let -- info from previous view (in view hierarchy)
                         br = prevWRTDpd nVw vwDpdGr brMp emptyBldRlsState
                         (vwRlInfo,vwHasDefs)
                           = case gamLookup nVw vrg of
                               Just i -> (i,True)
                               _      -> (emptyVwRlInfo {vwrlNm=nVw},False)
                         vrgOfVwRlInfo = gamLookup nVw . rlVwGam
                         (doMarkChng,isFstMarkChng) = doMarkChngForVw nVw

                         --
                         revRlOnL = rlVwRlOnL dtInvGam rlGam (rsScNm rsInfo) nVw rlInfo
                         (initBldL,rlJdBldOnL)
                           = case revRlOnL of
                               (i:_) | dgIsFirst vwDpdGr nVw vwSel
                                 -> ([RlJdBldDirect Set.empty (vwrlFullNoDfltPreGam i) (vwrlFullNoDfltPostGam i)],[])
                               _ -> (brJdBldL br,b)
                           where b = concat $ map vwrlJdBldL $ reverse $ revRlOnL
{-
                         vwIsFirst = dgIsFirst vwDpdGr nVw vwSel
                         rlJdBldOnL rlInfo
                           = case maybe Nothing (\n -> gamLookup n rlGam) (mbOnNm rlInfo) of
                               Just i -> case gamLookup nVw (rlVwGam i) of
                                           Just j | vwIsFirst -> [RlJdBldDirect Set.empty (vwrlFullPreGam j) (vwrlFullPostGam j)]
                                                  | otherwise -> rlJdBldOnL i ++ vwrlJdBldL j
                                           _                  -> []
                               _      -> []
                         mbOnNm rlInfo = dtInvGamRlMbOn dtInvGam rlInfo (rsScNm rsInfo) nVw
-}
                         rlJdBldL = initBldL ++ rlJdBldOnL ++ vwrlJdBldL vwRlInfo
                         (rlJdBldInfoL,errChkBldL) = checkJdAndAtBldL (vwrlPos vwRlInfo) cx scGam rsGam (rsScNm rsInfo) nVw (vwscFullAtBldL vwScInfo) rlJdBldL
                         (pregBld,postgBld,errBldL) = bldJdsFromRlBlds (rsScNm rsInfo) nVw scGam rlJdBldInfoL
                         (pregBldDflt,postgBldDflt) = bldDfltForJds nVw scGam (pregBld,postgBld)
                         pregBldFull  = pregBld  `reGamUnionShadow` pregBldDflt
                         postgBldFull = postgBld `reGamUnionShadow` postgBldDflt
                         
                         -- rule info
                         (scInfo,vwScInfo) = maybe (emptyScInfo,emptyVwScInfo) id $ scVwGamLookup (rsScNm rsInfo) nVw scGam

                         -- updating pre/post judgements
                         (preg',postg') = (reGamFilterOutDel pregBldFull,reGamFilterOutDel postgBldFull)

                         -- changes
                         vwRlChs
                           = gamMapWithKey (\jn ji -> gamMapWithKey (\an _ -> RlChInfo jn an) (maybe emptyGam id $ reMbJAGam ji))
                             $ (preg' `gamUnionShadow` postg') `reGamJAGamDifferenceOnExpr` (brPreGam br `gamUnionShadow` brPostGam br)
                         vwRlChsWtPrev = vwRlChs `rcGamUnionShadow` brRlChGam br
                         prevVwRlChs' = if doMarkChng then emptyGam else vwRlChsWtPrev

                         -- updating the view
                         vwRlInfo2
                           = vwRlInfo
                               { vwrlFullPreGam = reGamUpdInOut nVw scGam preg'
                               , vwrlFullPostGam = reGamUpdInOut nVw scGam  postg'
                               , vwrlFullNoDfltPreGam = pregBld
                               , vwrlFullNoDfltPostGam = postgBld
                               , vwrlMbChGam = if doMarkChng && not isFstMarkChng then Just vwRlChsWtPrev else Nothing
                               , vwrlJdBldOnAL = rlJdBldOnL, vwrlJdBldOnBL = initBldL
                               }
                         vwRlInfo3 = vwrlDelEmptyJd vwRlInfo2
                         vwRlInfo4 = vwRlInfo3 {vwrlPreScc = vwrlScc vwRlInfo3}

                         -- errors
                         cx = cxRs ++ " view '" ++ show nVw ++ "' for rule '" ++ show (rlNm rlInfo) ++ "'"
                         errVwNotSel = if not vwHasDefs || vwIsIncl nVw then [] else [Err_NotInSel (vwrlPos vwRlInfo) cx (Set.toList vwSel)]
                         vwUndefs = vwrlUndefs vwRlInfo3 `Set.difference` (vwrlExtNmS vwRlInfo `Set.union` extNmS)
                         errUndefs = if Set.null vwUndefs then [] else [Err_UndefNm (rlPos rlInfo) cx "identifier" (Set.toList vwUndefs)]
                         errDups = gamCheckDups (rlPos rlInfo) cx "judgement" (vwrlPreGam vwRlInfo `gamUnion` vwrlPostGam vwRlInfo)
                         postOfScG = gamFilter (\i -> reScNm i == rsScNm rsInfo) (vwrlFullPostGam vwRlInfo4)
                         errPost
                           = if (not . gamIsEmpty $ vwrlFullPostGam vwRlInfo4) && gamIsEmpty postOfScG && scKind scInfo == ScJudge
                             then [Err_RlPost (rlPos rlInfo) cx (rsScNm rsInfo)]
                             else []
                         errTr = []
                                 -- [mkTr (ppBracketsCommas [rsScNm rsInfo,rlNm rlInfo,nVw]) (pp (mbOnNm rlInfo))]
                                 -- [mkTr (ppBracketsCommas [rsScNm rsInfo,rlNm rlInfo,nVw]) (ppBracketsCommasV rlJdBldL >-< ppGam pregBld)]
                                 -- [mkTr (ppBracketsCommas [rsScNm rsInfo,rlNm rlInfo,nVw]) (ppBracketsCommas $ Set.toList $ vwSel)]
                         errsVw = errFirst [errPost,errDups,errUndefs,errChkBldL,errBldL] ++ errTr
                         errsOther = errVwNotSel
                         
                         -- next build state
                         br' = br {brPreGam = preg', brPostGam = postg', brJdBldL = rlJdBldL, brRlChGam = prevVwRlChs'}

                     in  ( if vrwlIsEmpty vwRlInfo4 then gamDelete nVw vrg else gamInsertShadow nVw vwRlInfo4 vrg
                         , Map.insert nVw br' brMp
                         , if null errsVw then errg else gamInsertShadow nVw errsVw errg
                         , errsOther ++ errs
                         )
                )
                (rlVwGam rlInfo,Map.empty,emptyGam,[])
                (dgTopSort vwDpdGr)
        errsG = concat . gamElemsShadow . gamFilterWithKey (\n _ -> vwIsIncl n) $ eg
    in  (rlInfo { rlVwGam = gamFilterWithKey (\n _ -> vwIsIncl n) g, rlMbInclVwS = Just vwSel },errFirst [errs,errsG])


bldRsInfo :: DpdGr Nm -> Set.Set Nm -> Opts -> DtInvGam -> ScGam Expr -> RsGam Expr -> RsInfo Expr -> (RsInfo Expr,[Err])
bldRsInfo vwDpdGr extNmS opts dtInvGam scGam rsGam rsInfo@(RsInfo nm pos schemeNm _ info rlGam)
  = (rsInfo {rsRlGam = g},mutErrs ++ errTr ++ errs)
  where (g,errs)
          = foldr
              (\rNm (rlGam,errs)
                -> let (rlInfo,errs')
                         = rlGamUpdVws cx opts vwDpdGr extNmS dtInvGam scGam rsGam rlGam rsInfo (maybe (panic "bldRsInfo") id . gamLookup rNm $ rlGam)
                   in  (gamInsertShadow rNm rlInfo rlGam,errs' ++ errs)
              )
              (rlGam,[])
              (dgTopSort rlDpdGr)
        errTr = [] -- [mkTr (ppBracketsCommas [nm]) (pp rlDpdGr)]
        rlDpdGr
          = mkDpdGrFromAssocWithMissing misL dpdL
          where -- dpdL = [ (rlNm i,onNm) | i <- gamElemsShadow rlGam, onNm <- maybeToList (rlMbOnNm i) ]
                dpdL = [ (rlNm i,onNm) | i <- gamElemsShadow rlGam, nVw <- gamKeys (rlVwGam i), onNm <- maybeToList (dtInvGamRlMbOn dtInvGam i (rsScNm rsInfo) nVw) ]
                misL = gamKeys rlGam \\ map fst dpdL
        cx = "ruleset '" ++ show (rsNm rsInfo) ++ "'"
        mutErrs = dgCheckSCCMutuals (Err_MutDpds pos cx "rule") rlDpdGr

%%]

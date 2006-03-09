-- $Id: Ruler.ag 231 2005-06-07 14:39:41Z atze $

-------------------------------------------------------------------------
-- Supporting functions for admin building
-------------------------------------------------------------------------

module RulerMkAdmin
  ( bldScInfo
  , bldRsInfo
  )
  where

import Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Nm
import Utils

import KeywParser( propsSynInhMp )
import Opts
import Err
import Common
import ExprUtils
import ARuleUtils( exprSubst )
import ViewSelUtils
import FmGam
import RulerUtils
import RulerAdmin

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

prevWRTDpd :: Nm -> DpdGr Nm -> Map.Map Nm v -> v -> v
prevWRTDpd n g m v
  = maybeHd v (\n -> maybe v id . Map.lookup n $ m) (vgDpdsOn g n)

-------------------------------------------------------------------------
-- Scheme
-------------------------------------------------------------------------

bldScInfo :: DpdGr Nm -> ScInfo Expr -> (ScInfo Expr,[Err])
bldScInfo vwDpdGr si@(ScInfo pos nm mbAGNm scKind vwScGam)
  = (si {scVwGam = g},e)
  where (g,_,e)
          = foldr
              (\nVw (vsg,agMp,errs)
                  -> let (ag,jdg,explg) = prevWRTDpd nVw vwDpdGr agMp (emptyGam,emptyGam,emptyGam)
                         (vw,ag',jdg',explg',errDups)
                            = case gamLookup nVw vsg of
                                  Just vw
                                    -> (vw
                                       ,new `gamUnionShadow` ag
                                       ,vwscJdGam vw `jdgUnionShadow` jdg
                                       ,vwscExplGam vw `gamUnionShadow` explg
                                       ,errDups
                                       )
                                    where new = gamMapWithKey
                                                  (\n a
                                                     -> case atProps a `intersect` Map.elems propsSynInhMp of
                                                          (_:_) -> [(ns,AtInfo ns [AtSyn] (atProps a) (atTy a))
                                                                   ,(ni,AtInfo ni [AtInh] (atProps a) (atTy a))
                                                                   ]
                                                                where ns = nmSetSuff n "syn"
                                                                      ni = nmSetSuff n "inh"
                                                          _     -> [(n,a)]
                                                  )
                                                  (vwscAtGam vw)
                                          cx = "scheme '" ++ show nm ++ "'"
                                          errDups = gamCheckDups pos cx "hole" (vwscAtGam vw)
                                                    ++ gamCheckDups pos cx "judgespec/use" (vwscJdGam vw)
                                                    ++ gamCheckDups pos cx "explanation" (vwscExplGam vw)
                                  Nothing
                                    -> (emptyVwScInfo { vwscNm = nVw },ag,jdg,explg,[])
                         vwag = gamFromAssocs . concat . gamElemsShadow $ ag'
                         agMp' = Map.insert nVw (ag',jdg',explg') agMp
                     in  (gamInsertShadow nVw (vw {vwscFullAtGam = vwag, vwscJdGam = jdg', vwscExplGam = explg'}) vsg,agMp',errDups ++ errs)
              )
              (vwScGam,Map.empty,[])
              (vgTopSort vwDpdGr)

-------------------------------------------------------------------------
-- Rule sets
-------------------------------------------------------------------------

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
jaGamDflt :: (Nm -> e) -> Nm -> Nm -> ScGam e -> JAGam e
jaGamDflt mkE sn nVw scGam
  = case scVwGamLookup sn nVw scGam of
      Just (_,vi) -> gamMapWithKey (\n _ -> JAInfo n (mkE n) (Set.singleton n)) . vwscFullAtGam $ vi
      Nothing     -> emptyGam

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

-- extend rule expr's gam with defaults, additionaly return increment
reGamExtDflt' :: (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> (REGam e,REGam e)
reGamExtDflt' dfltScJaGam nVw scGam g
  = gamMapAccumWithKey
      (\gIncr n i
        -> case i of
             REInfoJudge _ sn _ _ jg
               -> (gamInsertShadow n (i {reJAGam = d `gamDifference` jg}) gIncr,i {reJAGam = jg `gamUnionShadow` d})
               where d = dfltScJaGam sn n
      )
      emptyGam g

reGamExtDflt :: (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> REGam e
reGamExtDflt dfltScJaGam nVw scGam
  = snd . reGamExtDflt' dfltScJaGam nVw scGam

-- named expressions for each judgement
reGamExprFmGam :: (e -> FmGam e) -> REGam e -> Gam Nm (FmGam e)
reGamExprFmGam exprNmGam
  = gamMap (gamFold (\i g -> exprNmGam (jaExpr i) `gamUnionShadow` g) emptyGam . reJAGam)

-- extend rule expr's gam with new one
reGamExtWithNew :: (e -> FmGam e) -> (FmGam e -> e -> e) -> (e -> Set.Set Nm) -> (Nm -> Nm -> JAGam e) -> Nm -> ScGam e -> REGam e -> REGam e -> REGam e
reGamExtWithNew exprNmGam exprSubst exprNmS dfltJaGam nVw scGam gamPrev g
  = gamFoldWithKey
      (\n i gamPrev
        -> case i of
             REInfoJudge _ sn _ _ jg
               -> gamInsertShadow n (pi {reJAGam = jg'}) gamPrev
               where (base,pi)
                       = case gamLookup n gamPrev of
                           Just pi | reScNm pi == sn
                             -> (reJAGam pi,pi)
                           _ -> (dfltJG,i)
                     dfltJG = dfltJaGam sn n
                     jg' = gamMap (\i -> let e = exprSubst nmfg (jaExpr i)
                                         in  i {jaExpr = e,jaNmS = exprNmS e})
                                  (jg `gamUnionShadow` base)
                     nmfg = gamFindWithDefault emptyGam n nmFmGams
             REInfoDel ns
               -> foldr gamDelete gamPrev ns
      )
      gamPrev g
  where nmFmGams = reGamExprFmGam exprNmGam gamPrev

-- build views of a rule by extending each view along view order dependency
rlGamUpdVws :: Opts -> DpdGr Nm -> Set.Set Nm -> ScGam Expr -> RlGam Expr -> RsInfo Expr -> RlInfo Expr -> (RlInfo Expr,[Err])
rlGamUpdVws opts vwDpdGr extNmS scGam rlGam rsInfo rlInfo
  = let vwSel = rlInclVwS rlInfo `Set.intersection` rsInclVwS rsInfo
        vwIsIncl n = n `Set.member` vwSel
        doMarkChngForVw
          = case optMbMarkChange opts of
              Just vs
                -> \vw -> (vw `Set.member` vs',vgIsFirst vwDpdGr vw vs')
                where vs' = viewSelsNmS vwDpdGr vs `Set.intersection` vwSel
              _ -> const (False,False)
        mbOnVwRlInfo = maybe Nothing (\n -> gamLookup n rlGam) (rlMbOnNm rlInfo)
        (g,_,eg)
            = foldr
                (\nVw (vrg,prePostGamMp,errg)
                  -> let -- info from previous view (in view hierarchy)
                         (preg,postg,prevVwRlChs)   = prevWRTDpd nVw vwDpdGr prePostGamMp (emptyGam,emptyGam,emptyGam)
                         vrgOfVwRlInfo      = gamLookup nVw . rlVwGam
                         dfltJaGam
                           = case mbOnVwRlInfo of
                               Just i  -> \sn jn -> maybe (jaGamDflt Expr_Var sn nVw scGam) reJAGam . gamLookup jn $ g
                                       where g = maybe emptyGam (\i -> vwrlFullPreGam i `gamUnionShadow` vwrlFullPostGam i) (vrgOfVwRlInfo i)
                               Nothing -> \sn _  -> jaGamDflt Expr_Var sn nVw scGam
                         (pregIncr ,pregDflt )      = reGamExtDflt' dfltJaGam nVw scGam preg
                         (postgIncr,postgDflt)      = reGamExtDflt' dfltJaGam nVw scGam postg
                         ext newG prevG     = reGamExtWithNew exprNmGam (exprSubst (defaultOpts {optSubstFullNm=False})) exprNmS dfltJaGam nVw scGam prevG newG
                         (doMarkChng,isFstMarkChng) = doMarkChngForVw nVw
                         -- updating pre/post judgements
                         (vw,preg',postg')
                           = case maybe Nothing vrgOfVwRlInfo mbOnVwRlInfo of
                               Just i
                                 -> case gamLookup nVw vrg of
                                      Just vw -> (vw                        ,ext (vwrlPreGam vw) preD,ext (vwrlPostGam vw) postD)
                                      Nothing -> (emptyVwRlInfo {vwrlNm=nVw},ext emptyGam        preD,ext emptyGam         postD)
                                 where preD  = ext (vwrlPreGam  i) pregDflt
                                       postD = ext (vwrlPostGam i) postgDflt
                               Nothing
                                 -> case gamLookup nVw vrg of
                                      Just vw -> (vw                        ,ext (vwrlPreGam vw) pregDflt,ext (vwrlPostGam vw) postgDflt)
                                      Nothing -> (emptyVwRlInfo {vwrlNm=nVw},ext emptyGam        pregDflt,ext emptyGam         postgDflt)
                         vwRlChs
                           = let pg  = preg `gamUnionShadow` postg
                                 pg' = preg' `gamUnionShadow` postg'
                                 pd  = pg' `gamDifference` pg
                                 un  = gamUnionWith (\i1 i2 -> i1 {reJAGam = reJAGam i1 `gamUnionShadow` reJAGam i2})
                                 pn  = (vwrlPreGam vw `gamUnionShadow` vwrlPostGam vw) `un` pregIncr `un` postgIncr
                             in  gamMapWithKey (\jn ji -> gamMapWithKey (\an _ -> RlChInfo jn an) (maybe emptyGam id $ reMbJAGam ji))
                                 $ pd `gamUnionShadow` (pn `gamDifference` pd)
                         vwRlChsWtPrev = vwRlChs `rcGamUnionShadow` prevVwRlChs
                         prevVwRlChs' = if doMarkChng then emptyGam else vwRlChsWtPrev
                         -- updating the view
                         vw2 = vw {vwrlFullPreGam = reGamUpdInOut nVw scGam preg'
                                  ,vwrlFullPostGam = reGamUpdInOut nVw scGam  postg'
                                  ,vwrlMbChGam = if doMarkChng && not isFstMarkChng then Just vwRlChsWtPrev else Nothing
                                  }
                         vw3 = vwrlDelEmptyJd vw2
                         vw4 = vw3 {vwrlPreScc = vwrlScc vw3}
                         -- errors
                         scInfo = maybe emptyScInfo fst $ scVwGamLookup (rsScNm rsInfo) nVw scGam
                         cx = "ruleset '" ++ show (rsNm rsInfo) ++ "' view '" ++ show nVw ++ "' for rule '" ++ show (rlNm rlInfo) ++ "'"
                         vwUndefs = vwrlUndefs vw3 `Set.difference` extNmS
                         errUndefs = if Set.null vwUndefs then [] else [Err_UndefNm (rlPos rlInfo) cx "identifier" (Set.toList vwUndefs)]
                         errDups = gamCheckDups (rlPos rlInfo) cx "judgement" (vwrlPreGam vw `gamUnion` vwrlPostGam vw)
                         postOfScG = gamFilter (\i -> reScNm i == rsScNm rsInfo) (vwrlFullPostGam vw4)
                         errPost
                           = if (not . gamIsEmpty $ vwrlFullPostGam vw4) && gamIsEmpty postOfScG && scKind scInfo == ScJudge
                             then [Err_RlPost (rlPos rlInfo) cx (rsScNm rsInfo)]
                             else []
                         errs = errDups ++ errUndefs ++ errPost
                     in  (if vrwlIsEmpty vw4 then gamDelete nVw vrg else gamInsertShadow nVw vw4 vrg
                         ,Map.insert nVw (preg',postg',prevVwRlChs') prePostGamMp
                         ,if null errs then errg else gamInsertShadow nVw errs errg
                         )
                )
                (rlVwGam rlInfo,Map.empty,emptyGam)
                (vgTopSort vwDpdGr)
        errs = concat . gamElemsShadow . gamFilterWithKey (\n _ -> vwIsIncl n) $ eg
    in  (rlInfo { rlVwGam = gamFilterWithKey (\n _ -> vwIsIncl n) g, rlInclVwS = vwSel },errs)

bldRsInfo :: DpdGr Nm -> Set.Set Nm -> Opts -> ScGam Expr -> RsInfo Expr -> (RsInfo Expr,[Err])
bldRsInfo vwDpdGr extNmS opts scGam rsInfo@(RsInfo nm schemeNm _ info rlGam)
  = (rsInfo {rsRlGam = g},errs)
  where (g,errs)
          = foldr
              (\rNm (rlGam,errs)
                -> let (rlInfo,errs')
                         = rlGamUpdVws opts vwDpdGr extNmS scGam rlGam rsInfo (maybe (panic "bldRsInfo") id . gamLookup rNm $ rlGam)
                   in  (gamInsertShadow rNm rlInfo rlGam,errs' ++ errs)
              )
              (rlGam,[])
              rlDpdTopsort
        rlDpdTopsort
          = vgTopSort dpdG
          where dpdL = [ [n] | n <- gamKeys rlGam ]
                       ++ [ [onNm,n] | i <- gamElemsShadow rlGam, onNm <- maybeToList (rlMbOnNm i), let n = rlNm i ]
                dpdG = mkVwDpdGr dpdL


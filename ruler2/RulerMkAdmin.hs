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
                        -> (mkProp g `gamUnion` ga,g `gamUnion` gb,gamCheckDups emptySPos cx "hole" g ++ e)
                      ScAtBldScheme frScNm pos rnL
                        -> case scVwGamLookup frScNm (vwscNm vw) scGam of
                             Just (frScInfo,frVwScInfo)
                               -> ( mkProp g' `gamUnion` ga, g' `gamUnion` gb, errUndefHls ++ e )
                               where errUndefHls
                                       = if null undefNmL then [] else [Err_UndefNm pos ("use of scheme `" ++ show frScNm ++ "` for hole definition for " ++ cx) "hole" undefNmL]
                                     (g',undefNmL)
                                       = sabrGamRename rnL (vwscFullAtBldGam frVwScInfo)
                             _ -> (ga,gb,[Err_UndefNm pos ("hole definition for " ++ cx) "scheme" [frScNm]] ++ e)
              )
              (ga,gb,[])
              atBldL
        mkProp
          = gamMapWithKey
              (\n a
                 -> case atProps a `intersect` [AtThread] of
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
                                        , bsJdShpGam       = vwscJdShpGam vw `jdshpgUnionShadow` bsJdShpGam bs
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
              (vgTopSort vwDpdGr)

-------------------------------------------------------------------------
-- Rule set building, util functions
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
jaGamDflt :: (Nm -> Expr) -> Nm -> Nm -> ScGam Expr -> JAGam Expr
jaGamDflt mkE sn nVw scGam
  = case scVwGamLookup sn nVw scGam of
      Just (_,vi) -> gamMapWithKey (\n _ -> mkJAInfo n (mkE n)) . vwscFullAtGam $ vi
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

-------------------------------------------------------------------------
-- Judgements building, based on scheme description
-------------------------------------------------------------------------

type RlJdBldInfo = (RlJdBld Expr,Maybe (VwScInfo Expr,VwRlInfo Expr,ScAtBld))

checkJdAndAtBldL :: SPos -> String -> ScGam Expr -> RsGam Expr -> Nm -> [ScAtBld] -> [RlJdBld Expr] -> ([RlJdBldInfo],[Err])
checkJdAndAtBldL pos cx scGam rsGam vwNm atBldL jdBldL
  = (jdBldL2,errFirst [e1,e2])
  where (jdBldL2,atBldL2,e1)
          = foldr
              (\b i@(jbL,abL,e)
                -> case b of
                     RlJdBldFromRuleset pos rsNm rlNm
                       -> ((b,Just (vwScInfo,vwRlInfo,maybeHd emptyScAtBld id abRsL)):jbL,abRestL,errFirst [e1,e2,e3] ++ e)
                       where ((rsInfo,rlInfo,vwRlInfo),e1)
                                         = maybe ((emptyRsInfo,emptyRlInfo,emptyVwRlInfo),[Err_UndefNm pos (cx ++ " build item ruleset '" ++ show rsNm ++ "' rule '" ++ show rlNm ++ "'") ("ruleset+rule") [rsNm, rlNm]])
                                                 (\i -> (i,[]))
                                           $ rsRlVwGamLookup rsNm rlNm vwNm rsGam
                             scNm        = rsScNm rsInfo
                             ((scInfo,vwScInfo),e2)
                                         = maybe ((emptyScInfo,emptyVwScInfo),[Err_UndefNm pos cx "scheme" [scNm]]) (\i -> (i,[]))
                                           $ scVwGamLookup scNm vwNm scGam
                             (abRsL,abRestL)
                                         = partition isOkSc abL
                                         where isOkSc (ScAtBldScheme n _ _) = n == scNm
                                               isOkSc _                     = False
                             e3          = if null abRsL then [Err_UndefNm pos cx "scheme build item" [scNm]] else []
                     _ -> ((b,Nothing):jbL,abL,e)
              )
              ([],atBldL,[])
              jdBldL
        atBldL2NmL = [ n | (ScAtBldScheme n _ _) <- atBldL2 ]
        e2 = if null atBldL2NmL then [] else [Err_UndefNm pos cx "corresponding ruleset+rule build item" atBldL2NmL]

bldJdsFromRlBlds :: Nm -> [RlJdBldInfo] -> (REGam Expr,REGam Expr,[Err])
bldJdsFromRlBlds scNm rlBldInfoL
  = r
  where r@(preg,postg,e)
          = foldl
              (\g@(preg,postg,e) b
                -> case b of
                     (RlJdBldFromRuleset _ rsNm rlNm,Just (_,vwRlInfo,scAtBld))
                       -> (bpreg `reGamUnionShadow` preg,bpostg `reGamUnionShadow` postg,be++e)
                       where (bpreg,bpostg,be)
                               = case scAtBld of
                                   ScAtBldScheme frScNm _ rnL
                                     -> (gamMap upd $ vwrlFullPreGam $ vwRlInfo,gamMap upd $ vwrlFullPostGam $ vwRlInfo,[])
                                     where upd i
                                             = if reScNm i == frScNm
                                               then i {reScNm = scNm, reJAGam = fst $ sabrGamRename rnL $ reJAGam i}
                                               else i
                     (RlJdBldDirect dpreg dpostg,_)
                       -> (dpreg `reGamUnionShadow` preg,dpostg `reGamUnionShadow` postg,e)
              )
              (emptyGam,emptyGam,[])
              rlBldInfoL

bldDfltForJds :: Nm -> ScGam Expr -> (REGam Expr,REGam Expr) -> (REGam Expr,REGam Expr)
bldDfltForJds vwNm scGam (preg,postg)
  = (mkjg preg, mkjg postg)
  where mkag sn = jaGamDflt Expr_Var sn vwNm scGam
        mkjg = gamMap (\i -> i {reJAGam = mkag (reScNm i)}) . reGamFilterOutDel

-------------------------------------------------------------------------
-- Rule set building, top function
-- build views of a rule by extending each view along view order dependency
-------------------------------------------------------------------------

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

rlGamUpdVws :: String -> Opts -> DpdGr Nm -> Set.Set Nm -> ScGam Expr -> RsGam Expr -> RlGam Expr -> RsInfo Expr -> RlInfo Expr -> (RlInfo Expr,[Err])
rlGamUpdVws cxRs opts vwDpdGr extNmS scGam rsGam rlGam rsInfo rlInfo
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
                (\nVw (vrg,brMp,errg)
                  -> let -- info from previous view (in view hierarchy)
                         br = prevWRTDpd nVw vwDpdGr brMp emptyBldRlsState
                         vwRlInfo = gamFindWithDefault (emptyVwRlInfo {vwrlNm=nVw}) nVw vrg
                         vrgOfVwRlInfo = gamLookup nVw . rlVwGam
                         (doMarkChng,isFstMarkChng) = doMarkChngForVw nVw

                         --
                         rlJdBldOnL rlInfo
                           = case maybe Nothing (\n -> gamLookup n rlGam) (rlMbOnNm rlInfo) of
                               Just i -> case gamLookup nVw (rlVwGam i) of
                                           Just j -> rlJdBldOnL i ++ vwrlJdBldL j
                                           _      -> []
                               _      -> []
                         rlJdBldL = brJdBldL br ++ rlJdBldOnL rlInfo ++ vwrlJdBldL vwRlInfo
                         (rlJdBldInfoL,errChkBldL) = checkJdAndAtBldL (vwrlPos vwRlInfo) cx scGam rsGam nVw (vwscFullAtBldL vwScInfo) rlJdBldL
                         (pregBld,postgBld,errBldL) = bldJdsFromRlBlds (rsScNm rsInfo) rlJdBldInfoL
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
                             $ p2
                           where p2 = (preg' `gamUnionShadow` postg') `reGamJAGamDifferenceOnExpr` (brPreGam br `gamUnionShadow` brPostGam br)
                         vwRlChsWtPrev = vwRlChs `rcGamUnionShadow` brRlChGam br
                         prevVwRlChs' = if doMarkChng then emptyGam else vwRlChsWtPrev

                         -- updating the view
                         rlJdBldDfltL = [RlJdBldDirect (pregBldDflt `reGamJAGamDifference` pregBld) (postgBldDflt `reGamJAGamDifference` postgBld)]
                         vwRlInfo2
                           = vwRlInfo
                               {vwrlFullPreGam = reGamUpdInOut nVw scGam preg'
                               ,vwrlFullPostGam = reGamUpdInOut nVw scGam  postg'
                               ,vwrlMbChGam = if doMarkChng && not isFstMarkChng then Just vwRlChsWtPrev else Nothing
                               }
                         vwRlInfo3 = vwrlDelEmptyJd vwRlInfo2
                         vwRlInfo4 = vwRlInfo3 {vwrlPreScc = vwrlScc vwRlInfo3}

                         -- errors
                         cx = cxRs ++ " view '" ++ show nVw ++ "' for rule '" ++ show (rlNm rlInfo) ++ "'"
                         vwUndefs = vwrlUndefs vwRlInfo3 `Set.difference` extNmS
                         errUndefs = if Set.null vwUndefs then [] else [Err_UndefNm (rlPos rlInfo) cx "identifier" (Set.toList vwUndefs)]
                         errDups = gamCheckDups (rlPos rlInfo) cx "judgement" (vwrlPreGam vwRlInfo `gamUnion` vwrlPostGam vwRlInfo)
                         postOfScG = gamFilter (\i -> reScNm i == rsScNm rsInfo) (vwrlFullPostGam vwRlInfo4)
                         errPost
                           = if (not . gamIsEmpty $ vwrlFullPostGam vwRlInfo4) && gamIsEmpty postOfScG && scKind scInfo == ScJudge
                             then [Err_RlPost (rlPos rlInfo) cx (rsScNm rsInfo)]
                             else []
                         errs = errDups ++ errUndefs ++ errPost ++ errChkBldL ++ errBldL
                         
                         -- next build state
                         br' = br {brPreGam = preg', brPostGam = postg', brJdBldL = rlJdBldL, brRlChGam = prevVwRlChs'}

                     in  ( if vrwlIsEmpty vwRlInfo4 then gamDelete nVw vrg else gamInsertShadow nVw vwRlInfo4 vrg
                         , Map.insert nVw br' brMp
                         , if null errs then errg else gamInsertShadow nVw errs errg
                         )
                )
                (rlVwGam rlInfo,Map.empty,emptyGam)
                (vgTopSort vwDpdGr)
        errs = concat . gamElemsShadow . gamFilterWithKey (\n _ -> vwIsIncl n) $ eg
    in  (rlInfo { rlVwGam = gamFilterWithKey (\n _ -> vwIsIncl n) g, rlInclVwS = vwSel },errs)


bldRsInfo :: DpdGr Nm -> Set.Set Nm -> Opts -> ScGam Expr -> RsGam Expr -> RsInfo Expr -> (RsInfo Expr,[Err])
bldRsInfo vwDpdGr extNmS opts scGam rsGam rsInfo@(RsInfo nm pos schemeNm _ info rlGam)
  = (rsInfo {rsRlGam = g},mutErrs ++ errs)
  where (g,errs)
          = foldr
              (\rNm (rlGam,errs)
                -> let (rlInfo,errs')
                         = rlGamUpdVws cx opts vwDpdGr extNmS scGam rsGam rlGam rsInfo (maybe (panic "bldRsInfo") id . gamLookup rNm $ rlGam)
                   in  (gamInsertShadow rNm rlInfo rlGam,errs' ++ errs)
              )
              (rlGam,[])
              (vgTopSort rlDpdGr)
        rlDpdGr
          = mkScDpdGr misL dpdL
          where dpdL = [ (rlNm i,onNm) | i <- gamElemsShadow rlGam, onNm <- maybeToList (rlMbOnNm i) ]
                misL = gamKeys rlGam \\ map fst dpdL
        cx = "ruleset '" ++ show (rsNm rsInfo) ++ "'"
        mutErrs = vgCheckSCCMutuals (Err_MutDpds pos cx "rule") rlDpdGr


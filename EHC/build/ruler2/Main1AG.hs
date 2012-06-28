

-- UUAGC 0.9.39.1 (build/ruler2/Main1AG.ag)
module Main1AG where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Pretty
import EH.Util.FPath
import EH.Util.Utils
import Err
import Common
import Opts
import LaTeXFmtUtils
import Expr.Utils
import ARule.Utils
import ViewSel.Utils
import Config (cfgStrSel,cfgFmFmtCmdAsc)
import FmGam
import RwExprGam
import ECnstrGam
import AbsSyn.AbsSyn1
import qualified AbsSyn.AbsSyn2 as AS2
import qualified Main2AG as M2
import Utils
import Admin
import MkAdmin















atGamUpdDirs :: ([AtDir] -> Bool) -> [AtDir] -> AtGam -> AtGam
atGamUpdDirs f d = gamMap (\i -> if f (atDirs i) then i {atDirs = d} else i)


type MkDerivScInfo = (ScInfo Expr -> ScInfo Expr,ScGam Expr -> RsInfo Expr)
type DrvGam = Gam Nm [MkDerivScInfo]

drvGamUnion :: DrvGam -> DrvGam -> DrvGam
drvGamUnion = gamUnionWith (++)



rsSelect :: RlSelIsSel -> RsGam Expr -> [(Nm,RsInfo Expr)]
rsSelect rlSelIsSel rsGam
  = rs
  where rs = [ (rsNm,rsInfo {rsRlGam = gamFromAssocs rls})
             | (rsNm,rsInfo) <- gamAssocsShadow rsGam
             , rsInfoIsPlain rsInfo
             , rlSelIsSel nmAny rsNm nmAny
             , let rls
                     = [ (rlNm,rlInfo {rlVwGam = gamFromAssocs vws})
                       | (rlNm,rlInfo) <- gamAssocsShadow (rsRlGam rsInfo)
                       , rlSelIsSel nmAny nmAny rlNm
                       , let vws
                               = [ v
                                 | v@(vwNm,_) <- gamAssocsShadow (rlVwGam rlInfo)
                                 , rlSelIsSel vwNm nmAny nmAny
                                 ]
                       , not (null vws)
                       ]
             , not (null rls)
             ]

rsSelectGroup :: RlSelIsSel -> RsGam Expr -> [(Nm,RsInfo Expr)]
rsSelectGroup rlSelIsSel rsGam
  = rs
  where rs = [ (rsNm,rsInfo {rsRlNms = rlNms})
             | (rsNm,rsInfo) <- gamAssocsShadow rsGam
             , rsInfoIsGroup rsInfo
             , rlSelIsSel nmAny rsNm nmAny
             , let rlNms
                     = [ r | r@(nRs,nRl) <- rsRlNms rsInfo, rlSelIsSel nmAny nRs nRl ]
             , not (null rlNms)
             ]

scSelect :: RlSelIsSel -> ScGam Expr -> [(Nm,ScInfo Expr)]
scSelect rlSelIsSel scGam
  = sc
  where sc = [ (scNm,scInfo {scVwGam = gamFromAssocs vws})
             | (scNm,scInfo) <- gamAssocsShadow scGam
             -- , rlSelIsSel nmAny rsNm nmAny
             , let vws
                     = [ v
                       | v@(vwNm,_) <- gamAssocsShadow (scVwGam scInfo)
                       , rlSelIsSel vwNm nmAny nmAny
                       ]
             ]

jdChangeInfo :: Opts -> (Nm -> Maybe Bool) -> FmGam Expr -> (FmGam Expr,Expr->Expr)
jdChangeInfo opts isChgd jaFmGam
  = if null chs    then (jaFmGam,id)
    else if or chs then (fmGamMap mkChng jaFmGam,id)
                   else (jaFmGam,Expr_Wrap WrIsSame)
  where chs = catMaybes . map isChgd . gamKeys $ jaFmGam
        mkChng
          = case optMbMarkChange opts of
              Just _  -> \nAt -> case isChgd nAt of
                                   Just isCh -> Expr_Wrap (if isCh then WrIsChanged else WrIsSame)
                                   Nothing   -> id
              Nothing -> \_   -> id

atIsChanged :: VwRlInfo Expr -> Nm -> Nm -> Maybe Bool
atIsChanged vwRlInfo
  = case vwrlMbChGam vwRlInfo of
      Just g  -> \j a -> maybe (Just False) (const (Just True)) $ dblGamLookup id j a g
      Nothing -> \_ _ -> Nothing

-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
      synthesized attributes:
         exprIsRw             : ExprIsRw
         nmS                  : Set.Set Nm
         pp                   : PP_Doc
         self                 : Expr 
   alternatives:
      alternative AGItf:
         child expr           : Expr 
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = (FmGam Expr) ->
                    ( ExprIsRw,(Set.Set Nm),PP_Doc,Expr )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOexprIsRw :: ExprIsRw
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- copy rule (up)
              _lhsOexprIsRw =
                  _exprIexprIsRw
              -- copy rule (up)
              _lhsOself =
                  _exprIself
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
      chained attribute:
         fmGam                : FmGam Expr
      synthesized attributes:
         as2                  : AS2.Decls
         dtInvGam             : DtInvGam
         errL                 : [Err]
         pp                   : PP_Doc
         rwGam                : RwExprGam
         scGam                : ScGam Expr
   alternatives:
      alternative AGItf:
         child decls          : Decls 
         visit 0:
            local _tup1       : _
            local dtGam       : _
            local errBldDt    : _
            local dtInvGam    : _
            local extNmS      : _
            local drvGam      : _
            local _tup2       : _
            local scGam       : _
            local errBldSc    : _
            local _tup3       : _
            local rsGam       : _
            local errBldRs    : _
            local fmGam       : _
            local rwGam       : _
            local errDupRs    : _
            local errDupSc    : _
            local vwDpdGr     : _
            local scDpdGr     : _
            local errMutSc    : _
            local rlSelIsSel  : _
            local rsSelected  : _
            local scSelected  : _
            local rsGrpSelected : _
            local fmAS2       : _
            local rsMetaSel   : _
            local mkWrapChunk : _
            local as2ScmATTR  : _
            local as2ScmDATA  : _
            local as2ScmMeta  : _
            local as2RuleSets : _
            local as2Explain  : _
            local as2Preamble : _
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _decls )  =
    (sem_AGItf_AGItf (sem_Decls _decls ) )
-- semantic domain
type T_AGItf  = (FmGam Expr) ->
                Opts ->
                ( (AS2.Decls),DtInvGam,([Err]),(FmGam Expr),PP_Doc,RwExprGam,(ScGam Expr))
data Inh_AGItf  = Inh_AGItf {fmGam_Inh_AGItf :: (FmGam Expr),opts_Inh_AGItf :: Opts}
data Syn_AGItf  = Syn_AGItf {as2_Syn_AGItf :: (AS2.Decls),dtInvGam_Syn_AGItf :: DtInvGam,errL_Syn_AGItf :: ([Err]),fmGam_Syn_AGItf :: (FmGam Expr),pp_Syn_AGItf :: PP_Doc,rwGam_Syn_AGItf :: RwExprGam,scGam_Syn_AGItf :: (ScGam Expr)}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIfmGam _lhsIopts )  =
    (let ( _lhsOas2,_lhsOdtInvGam,_lhsOerrL,_lhsOfmGam,_lhsOpp,_lhsOrwGam,_lhsOscGam) = sem _lhsIfmGam _lhsIopts 
     in  (Syn_AGItf _lhsOas2 _lhsOdtInvGam _lhsOerrL _lhsOfmGam _lhsOpp _lhsOrwGam _lhsOscGam ))
sem_AGItf_AGItf :: T_Decls  ->
                   T_AGItf 
sem_AGItf_AGItf decls_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOerrL :: ([Err])
              _declsOruleNm :: Nm
              _declsOscmNm :: Nm
              _declsOviewNm :: Nm
              _declsOuniq :: Int
              _declsOrlSeqNr :: Int
              _lhsOpp :: PP_Doc
              _lhsOas2 :: (AS2.Decls)
              _lhsOdtInvGam :: DtInvGam
              _lhsOfmGam :: (FmGam Expr)
              _lhsOrwGam :: RwExprGam
              _lhsOscGam :: (ScGam Expr)
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrsGam :: (RsGam Expr)
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 73, column 21)
              _lhsOerrL =
                  _errDupSc ++ _errMutSc ++ _errBldDt ++ _errBldSc ++ _errDupRs ++ _errBldRs ++ _declsIerrL
              -- "build/ruler2/Main1AG.ag"(line 118, column 33)
              __tup1 =
                  gamFoldWithKey
                    (\n i (g,e)
                      -> let (i',e') = bldDtInfo _vwDpdGr i
                         in  (gamInsertShadow n i' g,e'++e)
                    )
                    (_declsIgathDtGam,[])
                    _declsIgathDtGam
              -- "build/ruler2/Main1AG.ag"(line 118, column 33)
              (_dtGam,_) =
                  __tup1
              -- "build/ruler2/Main1AG.ag"(line 118, column 33)
              (_,_errBldDt) =
                  __tup1
              -- "build/ruler2/Main1AG.ag"(line 118, column 21)
              _dtInvGam =
                  dtGamInv _dtGam
              -- "build/ruler2/Main1AG.ag"(line 165, column 21)
              _extNmS =
                  _declsIgathExtNmS
              -- "build/ruler2/Main1AG.ag"(line 249, column 21)
              _drvGam =
                  _declsIgathDrvGam
              -- "build/ruler2/Main1AG.ag"(line 260, column 33)
              __tup2 =
                  let r = foldr
                            (\sNm (scGam,scErrs)
                              -> let scInfo = gamLookupJust sNm scGam
                                     drvScInfoL = maybe [] (\l -> map (($ scInfo) . fst) l) $ gamLookup sNm _drvGam
                                     (ascL,errLL) = unzip [ ((scNm i',i'),e) | i <- (scInfo : drvScInfoL), let (i',e) = bldScInfo _vwDpdGr scGam i ]
                                 in  (gamFromAssocs ascL `gamUnionShadow` scGam,concat errLL ++ scErrs)
                            )
                            (_declsIgathScGam,[])
                            (dgTopSort _scDpdGr)
                  in  r
              -- "build/ruler2/Main1AG.ag"(line 260, column 33)
              (_scGam,_) =
                  __tup2
              -- "build/ruler2/Main1AG.ag"(line 260, column 33)
              (_,_errBldSc) =
                  __tup2
              -- "build/ruler2/Main1AG.ag"(line 337, column 21)
              _declsOruleNm =
                  nmNone
              -- "build/ruler2/Main1AG.ag"(line 337, column 21)
              _declsOscmNm =
                  nmNone
              -- "build/ruler2/Main1AG.ag"(line 337, column 21)
              _declsOviewNm =
                  nmNone
              -- "build/ruler2/Main1AG.ag"(line 372, column 33)
              __tup3 =
                  let r = foldr
                            (\sNm (rsGam,rsErrs)
                              -> let (rsGamBldL,errLL)
                                       = unzip
                                         $ map
                                             (\rsInfo
                                               -> let drvRsInfoL = maybe [] (\l -> map (($ _scGam) . snd) l) $ gamLookup sNm _drvGam
                                                      (blds,errs)
                                                        = unzip [ ((rsNm i',i'),e) | i <- (rsInfo : drvRsInfoL)
                                                                , let (i',e) = bldRsInfo _vwDpdGr _extNmS _lhsIopts _dtInvGam _scGam rsGam i
                                                                ]
                                                  in  (gamFromAssocs blds,concat errs)
                                             )
                                         $ [ i | i <- gamElems rsGam, rsInfoIsPlain i, rsScNm i == sNm ]
                                 in  (gamUnionsShadow rsGamBldL `gamUnionShadow` rsGam,concat errLL ++ rsErrs)
                            )
                            (_declsIgathRsGam,[])
                            (dgTopSort _scDpdGr)
                  in  r
              -- "build/ruler2/Main1AG.ag"(line 372, column 33)
              (_rsGam,_) =
                  __tup3
              -- "build/ruler2/Main1AG.ag"(line 372, column 33)
              (_,_errBldRs) =
                  __tup3
              -- "build/ruler2/Main1AG.ag"(line 409, column 21)
              _fmGam =
                  _declsIgathFmGam
                  `gamUnionShadow`
                  _lhsIfmGam
                  `gamUnionShadow`
                  fmGamFromList' FmFmtCmd [ (Nm f,Expr_Var (Nm t)) | (f,t) <- cfgFmFmtCmdAsc ]
              -- "build/ruler2/Main1AG.ag"(line 409, column 21)
              _rwGam =
                  _declsIgathRwGam
              -- "build/ruler2/Main1AG.ag"(line 462, column 21)
              _errDupRs =
                  gamCheckDups emptySPos "toplevel" "ruleset/rulegroup" _declsIgathRsGam
              -- "build/ruler2/Main1AG.ag"(line 462, column 21)
              _errDupSc =
                  gamCheckDups emptySPos "toplevel" "scheme" _declsIgathScGam
              -- "build/ruler2/AS1/Misc.ag"(line 15, column 21)
              _declsOuniq =
                  0
              -- "build/ruler2/AS1/Misc.ag"(line 31, column 21)
              _declsOrlSeqNr =
                  1
              -- "build/ruler2/AS1/ViewDpd.ag"(line 24, column 21)
              _vwDpdGr =
                  let vwOrderL = concat _declsIgathVwOrder
                      missing = _declsIallVwNmS `Set.difference` Set.fromList (concat vwOrderL)
                  in  mkDpdGrFromOrderWithMissing (Set.toList missing) vwOrderL
              -- "build/ruler2/AS1/SchemeDpd.ag"(line 19, column 21)
              _scDpdGr =
                  let missing = gamKeys _declsIgathScGam \\ map fst _declsIgathScDpds
                  in  mkDpdGrFromAssocWithMissing missing _declsIgathScDpds
              -- "build/ruler2/AS1/SchemeDpd.ag"(line 27, column 21)
              _errMutSc =
                  dgCheckSCCMutuals (Err_MutDpds emptySPos "toplevel" "scheme") _scDpdGr
              -- "build/ruler2/AS1/Pretty.ag"(line 5, column 21)
              _lhsOpp =
                         "---------- dt inv gam" >-< ppGam _dtInvGam
                  >-<
                         "---------- scheme gam" >-< ppGam _scGam
                  >-< "---------- rule set gam" >-< ppGam _rsGam
                  >-< "---------- sc dpd graph" >-< pp _scDpdGr
                  >-< "---------- vw dpd graph" >-< pp _vwDpdGr
              -- "build/ruler2/AS1/RlSel.ag"(line 5, column 21)
              _rlSelIsSel =
                  case optMbRlSel _lhsIopts of
                      Just rs -> rlSelIsSel _vwDpdGr rs
                      _       -> \_ _ _ -> True
              -- "build/ruler2/AS1/GenAS2.ag"(line 74, column 21)
              _rsSelected =
                  rsSelect _rlSelIsSel _rsGam
              -- "build/ruler2/AS1/GenAS2.ag"(line 74, column 21)
              _scSelected =
                  scSelect _rlSelIsSel _scGam
              -- "build/ruler2/AS1/GenAS2.ag"(line 74, column 21)
              _rsGrpSelected =
                  rsSelectGroup _rlSelIsSel _rsGam
              -- "build/ruler2/AS1/GenAS2.ag"(line 86, column 21)
              _fmAS2 =
                  fmAS2Fm (optGenFM _lhsIopts)
              -- "build/ruler2/AS1/GenAS2.ag"(line 87, column 21)
              _lhsOas2 =
                  _as2Preamble ++ _as2ScmMeta ++ _as2RuleSets ++ _as2Explain
              -- "build/ruler2/AS1/GenAS2.ag"(line 94, column 21)
              _rsMetaSel =
                  let nVwS = dgVertices _vwDpdGr
                  in  gamFromAssocsWith Set.union
                        [ (rsScNm i,vs) | i <- gamElemsShadow _rsGam
                                        , let vs = Set.filter (\v -> _rlSelIsSel v (rsNm i) nmAny) nVwS, not (Set.null vs)
                        ]
              -- "build/ruler2/AS1/GenAS2.ag"(line 94, column 21)
              _mkWrapChunk =
                  \ag s v -> AS2.wrapInChunk AS2.Decl_Chunk _lhsIopts (rsSelMapVwNm (optMbRlSel _lhsIopts) v `nmApd` s `nmApd` Nm ag)
              -- "build/ruler2/AS1/GenAS2.ag"(line 94, column 21)
              _as2ScmATTR =
                  let mkChunk = _mkWrapChunk "ATTR"
                      mk s v
                        = case scVwGamLookup s v _scGam of
                            Just (si,vi) | not (gamIsEmpty agi && gamIsEmpty agsi && gamIsEmpty ags)
                              -> [AS2.Decl_AttrAG (AS2.AttrAGDecl_Attr (sc2DATA si _dtInvGam) (mkg agi) (mkg agsi) (mkg ags))]
                              where ag1 = gamFilter (\ai -> AtNode `notElem` atProps ai) (vwscFullAtGam vi)
                                    (agsi,ag2) = gamPartition (\ai -> isJust (atMbSynInh ai)) ag1
                                    (ags,agi) = gamPartition (\ai -> AtSyn `atHasDir` ai) ag2
                                    mkg g = gamAssocsShadow $ gamFromAssocs
                                            $ [ (n,atTy ai) | ai <- gamElemsShadow g, let n = nmSubst _lhsIopts _fmGam $ maybe (atNm ai) id $ atMbSynInh ai ]
                            _ -> []
                  in  [ mkChunk s v d | (s,vs) <- gamAssocsShadow _rsMetaSel, v <- Set.toList vs, d <- mk s v ]
              -- "build/ruler2/AS1/GenAS2.ag"(line 94, column 21)
              _as2ScmDATA =
                  let mkChunk = _mkWrapChunk "DATA"
                      mk s v
                        = case scVwGamLookup s v _scGam of
                            Just (si,vi) | not (null alts)
                              -> [AS2.Decl_DataAG (AS2.DataAGDecl_Data dtNm alts)]
                              where dtNm = sc2DATA si _dtInvGam
                                    alts
                                      = case dtVwGamLookup dtNm v _dtGam of
                                          Just (dtInfo,dtVwInfo)
                                            -> [ AS2.DataAGAlt_Alt a
                                                    [ AS2.DataAGFld_Fld (nmSubst _lhsIopts _fmGam f) (dfTy fi) (tyIsDef (tyTopNm $ dfTy fi))
                                                    | (f,fi) <- sortOn (dfSeqNr . snd) $ gamAssocs $ daFldGam ai
                                                    ]
                                               | (a,ai) <- gamAssocs (vdFullAltGam dtVwInfo)
                                               ]
                                            where tyIsDef t = maybe False (const True) $ dtVwGamLookup t v _dtGam
                                          _ -> []
                            _ -> []
                  in  [ mkChunk s v d | (s,vs) <- gamAssocsShadow _rsMetaSel, v <- Set.toList vs, d <- mk s v ]
              -- "build/ruler2/AS1/GenAS2.ag"(line 94, column 21)
              _as2ScmMeta =
                  if _fmAS2 == FmAG || _fmAS2 == FmHS
                  then (if optGenAGData _lhsIopts then _as2ScmDATA else [])
                       ++ (if optGenAGAttr _lhsIopts then _as2ScmATTR else [])
                  else []
              -- "build/ruler2/AS1/GenAS2.ag"(line 141, column 21)
              _as2RuleSets =
                  let topWrap d = [AS2.Decl_RsVw d]
                      mkRs rsInfo
                        = case rsInfo of
                            RsInfo nRs _ nSc vwSel d rlGam
                              -> topWrap
                                 $ AS2.RsVwDecl_Rs nRs nSc d
                                     [ AS2.VwDecl_Vw v (mkFullVwNm nRs v) rls'
                                     | (v,rls) <- gamAssocsShadow rlGamT
                                     , let rls' = catMaybes [ gamLookup n rls | n <- rlOrder ]
                                     ]
                              where rlGamT    = gamTranspose (rlVwGam,mkRl) rlGam
                                    rlOrder   = rsRlOrder rsInfo
                                    jdsOf o g = [ maybe (panic "as2RuleSets") id (gamLookup jNm g) | jNm <- o ]
                                    mkRl nRl nVw rlInfo vwRlInfo
                                      = mkChunk nVw rlInfo
                                        $ AS2.RlDecl_Rl nRl fullNm (rlPos rlInfo) (rl2SEM rlInfo _dtInvGam nSc nRl nVw) pre post
                                      where preg  = vwrlFullPreGam vwRlInfo
                                            postg = vwrlFullPostGam vwRlInfo
                                            preOrder  = concat (vwrlPreScc vwRlInfo)
                                            postOrder = gamKeys postg
                                            (pre,post)
                                              = case _fmAS2 of
                                                  FmTeX -> (mkExprJds vwRlInfo preOrder preg,mkExprJds vwRlInfo postOrder postg)
                                                  FmAG  -> (mkAtsJds preOrder preg,mkAtsJds postOrder postg)
                                                  _     -> ([],[])
                                            fullNm = mkFullRlNm nRs nVw nRl
                                    mkChunk nVw rlInfo
                                      = AS2.wrapInChunk AS2.RlDecl_Chunk _lhsIopts (nVwRnm `nmApd` nSc `nmApd` rlNm rlInfo)
                                      where nVwRnm = rsSelMapVwNm (optMbRlSel _lhsIopts) nVw
                                    mkAtsJds order reGam
                                      = [ AS2.Jd_Ats (reNm jInfo) (reScNm jInfo) (mkAts jInfo) | jInfo <- jdsOf order reGam ]
                                      where mkAts jInfo = [ AS2.JdAt_At aNm (jaExpr a) | (aNm,a) <- gamAssocsShadow (reJAGam jInfo) ]
                                    mkExprJds vwRlInfo order reGam
                                      = [ AS2.Jd_Expr (reNm jInfo) (reScNm jInfo) (mkExpr vwRlInfo jInfo) (reIsSmall jInfo) | jInfo <- jdsOf order reGam ]
                                      where mkExpr vwRlInfo jInfo
                                              = wrapFullJd $ exprSubst (_lhsIopts {optSubstOnce=True}) jg' $ e
                                              where (scInfo,vwScInfo) = fromJust (scVwGamLookup (reScNm jInfo) nVw _scGam)
                                                    jg = jaGamToFmGam id . reJAGam $ jInfo
                                                    (jg',wrapFullJd) = jdChangeInfo _lhsIopts (atIsChanged vwRlInfo (reNm jInfo)) jg
                                                    e  = jdGamFmExpr _fmAS2 . vwscJdShpGam $ vwScInfo
                                                    nVw = vwrlNm vwRlInfo
                            RsInfoGroup nRs _ nSc vwSel d rlNms | _fmAS2 == FmTeX
                              -> topWrap
                                 $ AS2.RsVwDecl_Rs nRs nSc d
                                     [ AS2.VwDecl_Vw nVw (mkFullVwNm nRs nVw) (mkRls nVw rlNms) | nVw <- vwOrder ]
                              where vwOrder = [ v | v <- dgTopSort _vwDpdGr, _rlSelIsSel v nmAny nmAny ]
                                    mkRls nVw rlNms
                                      = [ AS2.RlDecl_LTXAlias (mkFullRlNm nRs nVw nRl) (mkFullRlNm nRs' nVw nRl)
                                        | (nRs',nRl) <- rlNms, rlVwIsDef nRs' nVw nRl
                                        ]
                                    rlVwIsDef nRs nVw nRl
                                      = isJust (do rsInfo <- gamLookup nRs _rsGam
                                                   rlGam <- rsInfoMbRlGam rsInfo
                                                   rlVwGamLookup nRl nVw rlGam
                                               )
                            _ -> []
                      mkFullVwNm nRs nVw     = nmApd (Nm (optBaseNm _lhsIopts)) $ (if nVw == nmNone then id else nmApd nVw) $ nRs
                      mkFullRlNm nRs nVw nRl = mkFullVwNm nRs nVw `nmApd`              nRl
                      mkRlNm                 = if optDot2Dash _lhsIopts then nmDashed else nmFlatten
                  in  [ d | (nRs,rsInfo) <- _rsSelected ++ _rsGrpSelected, d <- mkRs rsInfo ]
              -- "build/ruler2/AS1/GenAS2.ag"(line 207, column 21)
              _as2Explain =
                  if optGenExpl _lhsIopts
                  then let explGen
                             = [ ex | (nSc,scInfo) <- _scSelected, exSc <- mkSc scInfo, ex <- exSc ]
                             where mkSc scInfo
                                     = [ (maybe [] (\e -> [mkChunk nVw "explain.scheme" $ AS2.Decl_ScVwExplain (fmtex e)]) . gamLookup nmNone . vwscExplGam $ vwInfo)
                                         ++ [ mkChunk nVw "explain.holes" $ AS2.Decl_ScVwAtExplain
                                              $ [ (fmte $ Expr_Expr $ Expr_Var $ n, fmtex ei) | (n,ei) <- gamAssocsShadow (vwscExplGam vwInfo `Map.intersection` vwscFullAtGam vwInfo) ]
                                            ]
                                       | (nVw,vwInfo) <- gamAssocsShadow (scVwGam scInfo)
                                       ]
                                     where mkChunk nVw n = AS2.wrapInChunk AS2.Decl_Chunk _lhsIopts (scNm scInfo `nmApd` nVw `nmApd` Nm n)
                                           fmte = exprSubst (_lhsIopts {optSubstFullNm=False, optGenFM = FmTeX}) _fmGam
                                           fmtex = fmte . explExpr
                       in  explGen
                  else []
              -- "build/ruler2/AS1/GenAS2.ag"(line 228, column 21)
              _as2Preamble =
                  if optPreamble _lhsIopts
                  then fkGamLookup [] (\p -> [AS2.Decl_Preamble p]) [_fmAS2] _declsIpaGam
                  else []
              -- copy rule (from local)
              _lhsOdtInvGam =
                  _dtInvGam
              -- copy rule (from local)
              _lhsOfmGam =
                  _fmGam
              -- copy rule (from local)
              _lhsOrwGam =
                  _rwGam
              -- copy rule (from local)
              _lhsOscGam =
                  _scGam
              -- copy rule (from local)
              _declsOdrvGam =
                  _drvGam
              -- copy rule (from local)
              _declsOfmGam =
                  _fmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _declsOrlSelIsSel =
                  _rlSelIsSel
              -- copy rule (from local)
              _declsOrsGam =
                  _rsGam
              -- copy rule (from local)
              _declsOrwGam =
                  _rwGam
              -- copy rule (from local)
              _declsOscDpdGr =
                  _scDpdGr
              -- copy rule (from local)
              _declsOscGam =
                  _scGam
              -- copy rule (from local)
              _declsOvwDpdGr =
                  _vwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOas2,_lhsOdtInvGam,_lhsOerrL,_lhsOfmGam,_lhsOpp,_lhsOrwGam,_lhsOscGam)))
-- AGTyItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : Ty 
   alternatives:
      alternative AGItf:
         child ty             : Ty 
-}
-- cata
sem_AGTyItf :: AGTyItf  ->
               T_AGTyItf 
sem_AGTyItf (AGTyItf_AGItf _ty )  =
    (sem_AGTyItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_AGTyItf  = ( Ty )
sem_AGTyItf_AGItf :: T_Ty  ->
                     T_AGTyItf 
sem_AGTyItf_AGItf ty_  =
    (let _lhsOself :: Ty 
         _tyIself :: Ty 
         -- copy rule (up)
         _lhsOself =
             _tyIself
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDest               : Bool
         mbDstWd              : Maybe (Int,Int)
         mbPrevNdStr          : Maybe String
      synthesized attributes:
         mxDstAtWd            : Int
         mxDstNdWd            : Int
         ndStr                : String
         nmS                  : Set.Set Nm
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local ndStr       : _
            local self        : _
      alternative Wild:
         visit 0:
            local nm          : _
            local ndStr       : _
            local self        : _
-}
-- cata
sem_ANm :: ANm  ->
           T_ANm 
sem_ANm (ANm_Fld _nm )  =
    (sem_ANm_Fld _nm )
sem_ANm (ANm_Lhs _nm _props )  =
    (sem_ANm_Lhs _nm _props )
sem_ANm (ANm_Loc _nm _props )  =
    (sem_ANm_Loc _nm _props )
sem_ANm (ANm_Node _ndNm _nm )  =
    (sem_ANm_Node _ndNm _nm )
sem_ANm (ANm_Wild )  =
    (sem_ANm_Wild )
-- semantic domain
type T_ANm  = Bool ->
              (Maybe (Int,Int)) ->
              (Maybe String) ->
              ( Int,Int,String,(Set.Set Nm),PP_Doc,ANm )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstAtWd :: Int
              _lhsOmxDstNdWd :: Int
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 51, column 21)
              _lhsOpp =
                  ppDest "F" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr "??"   _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 89, column 21)
              _ndStr =
                  ""
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 67, column 64)
              _lhsOmxDstNdWd =
                  0
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ANm_Fld nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 50, column 21)
              _lhsOpp =
                  ppDest "P" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 70, column 21)
              _lhsOmxDstNdWd =
                  3
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 87, column 21)
              _ndStr =
                  if AtRetain `elem` props_ then strLoc else strLhs
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ANm_Lhs nm_ props_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/NmSAG.ag"(line 9, column 21)
              _lhsOnmS =
                  Set.singleton nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 49, column 21)
              _lhsOpp =
                  ppDest "L" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr ""     nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 70, column 21)
              _lhsOmxDstNdWd =
                  3
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 86, column 21)
              _ndStr =
                  strLoc
              -- self rule
              _self =
                  ANm_Loc nm_ props_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 52, column 21)
              _lhsOpp =
                  ppDest "N" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr _ndStr _ndStr nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 72, column 21)
              _lhsOmxDstNdWd =
                  length _ndStr
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 88, column 21)
              _ndStr =
                  show ndNm_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ANm_Node ndNm_ nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIisDest
       _lhsImbDstWd
       _lhsImbPrevNdStr ->
         (let _lhsOpp :: PP_Doc
              _lhsOmxDstNdWd :: Int
              _lhsOmxDstAtWd :: Int
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ANm 
              _lhsOndStr :: String
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 47, column 21)
              _nm =
                  nmWild
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 48, column 21)
              _lhsOpp =
                  ppDest "W" _lhsIisDest _lhsImbDstWd _lhsImbPrevNdStr ""     ""     _nm
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 71, column 21)
              _lhsOmxDstNdWd =
                  0
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 73, column 21)
              _lhsOmxDstAtWd =
                  length . show $ _nm
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 89, column 21)
              _ndStr =
                  ""
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ANm_Wild
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _lhsOndStr =
                  _ndStr
          in  ( _lhsOmxDstAtWd,_lhsOmxDstNdWd,_lhsOndStr,_lhsOnmS,_lhsOpp,_lhsOself)))
-- AttrEqn -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         jaGam                : JAGam Expr
         pp                   : PP_Doc
   alternatives:
      alternative Del:
         child nm             : {Nm}
      alternative Eqn:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
-}
-- cata
sem_AttrEqn :: AttrEqn  ->
               T_AttrEqn 
sem_AttrEqn (AttrEqn_Del _nm )  =
    (sem_AttrEqn_Del _nm )
sem_AttrEqn (AttrEqn_Eqn _nm _expr )  =
    (sem_AttrEqn_Eqn _nm (sem_Expr _expr ) )
-- semantic domain
type T_AttrEqn  = Opts ->
                  Int ->
                  ( ([Err]),(JAGam Expr),PP_Doc,Int)
sem_AttrEqn_Del :: Nm ->
                   T_AttrEqn 
sem_AttrEqn_Del nm_  =
    (\ _lhsIopts
       _lhsIuniq ->
         (let _lhsOjaGam :: (JAGam Expr)
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 283, column 21)
              _lhsOjaGam =
                  gamSingleton nm_ (JAInfoDel nm_)
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
sem_AttrEqn_Eqn :: Nm ->
                   T_Expr  ->
                   T_AttrEqn 
sem_AttrEqn_Eqn nm_ expr_  =
    (\ _lhsIopts
       _lhsIuniq ->
         (let _lhsOjaGam :: (JAGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOerrL :: ([Err])
              _lhsOuniq :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Main1AG.ag"(line 282, column 21)
              _lhsOjaGam =
                  gamSingleton nm_ (mkJAInfo nm_ _exprIself)
              -- "build/ruler2/Main1AG.ag"(line 429, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 429, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 429, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/AS1/Pretty.ag"(line 41, column 21)
              _lhsOpp =
                  "|" >#< pp nm_ >#< "=" >#< _exprIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
-- AttrEqns ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         jaGam                : JAGam Expr
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : AttrEqn 
         child tl             : AttrEqns 
      alternative Nil:
-}
-- cata
sem_AttrEqns :: AttrEqns  ->
                T_AttrEqns 
sem_AttrEqns list  =
    (Prelude.foldr sem_AttrEqns_Cons sem_AttrEqns_Nil (Prelude.map sem_AttrEqn list) )
-- semantic domain
type T_AttrEqns  = Opts ->
                   Int ->
                   ( ([Err]),(JAGam Expr),PP_Doc,Int)
sem_AttrEqns_Cons :: T_AttrEqn  ->
                     T_AttrEqns  ->
                     T_AttrEqns 
sem_AttrEqns_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIuniq ->
         (let _lhsOerrL :: ([Err])
              _lhsOjaGam :: (JAGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOuniq :: Int
              _tlOopts :: Opts
              _tlOuniq :: Int
              _hdIerrL :: ([Err])
              _hdIjaGam :: (JAGam Expr)
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIerrL :: ([Err])
              _tlIjaGam :: (JAGam Expr)
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 279, column 29)
              _lhsOjaGam =
                  _hdIjaGam `gamUnion` _tlIjaGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIerrL,_hdIjaGam,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOuniq 
              ( _tlIerrL,_tlIjaGam,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOuniq 
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
sem_AttrEqns_Nil :: T_AttrEqns 
sem_AttrEqns_Nil  =
    (\ _lhsIopts
       _lhsIuniq ->
         (let _lhsOerrL :: ([Err])
              _lhsOjaGam :: (JAGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 279, column 29)
              _lhsOjaGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
-- AttrIntro ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative Intro:
         child props          : {[AtProp]}
         child nm             : {Nm}
         child ty             : {Nm}
         visit 0:
            local atGam       : _
-}
-- cata
sem_AttrIntro :: AttrIntro  ->
                 T_AttrIntro 
sem_AttrIntro (AttrIntro_Intro _props _nm _ty )  =
    (sem_AttrIntro_Intro _props _nm _ty )
-- semantic domain
type T_AttrIntro  = Opts ->
                    Nm ->
                    Int ->
                    ( ([ScAtBld]),AtGam,([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrIntro_Intro :: ([AtProp]) ->
                       Nm ->
                       Nm ->
                       T_AttrIntro 
sem_AttrIntro_Intro props_ nm_ ty_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOpp :: PP_Doc
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 142, column 21)
              _atGam =
                  let (pd,pr) = partition (`Set.member` propsDir) props_
                  in  gamSingleton nm_ (AtInfo nm_ pd pr ty_)
              -- "build/ruler2/AS1/Pretty.ag"(line 38, column 21)
              _lhsOpp =
                  pp props_ >#< pp nm_ >#< ":" >#< pp ty_
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _atGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- AttrIntroDecl -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative Attrs:
         child inhs           : AttrIntros 
         child inhsyns        : AttrIntros 
         child syns           : AttrIntros 
         visit 0:
            local atGam       : _
      alternative AttrsProp:
         child intros         : AttrIntros 
         visit 0:
            local atGam       : _
      alternative Scheme:
         child pos            : {SPos}
         child nm             : {Nm}
         child renames        : AttrRenames 
-}
-- cata
sem_AttrIntroDecl :: AttrIntroDecl  ->
                     T_AttrIntroDecl 
sem_AttrIntroDecl (AttrIntroDecl_Attrs _inhs _inhsyns _syns )  =
    (sem_AttrIntroDecl_Attrs (sem_AttrIntros _inhs ) (sem_AttrIntros _inhsyns ) (sem_AttrIntros _syns ) )
sem_AttrIntroDecl (AttrIntroDecl_AttrsProp _intros )  =
    (sem_AttrIntroDecl_AttrsProp (sem_AttrIntros _intros ) )
sem_AttrIntroDecl (AttrIntroDecl_Scheme _pos _nm _renames )  =
    (sem_AttrIntroDecl_Scheme _pos _nm (sem_AttrRenames _renames ) )
-- semantic domain
type T_AttrIntroDecl  = Opts ->
                        Nm ->
                        Int ->
                        ( ([ScAtBld]),AtGam,([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrIntroDecl_Attrs :: T_AttrIntros  ->
                           T_AttrIntros  ->
                           T_AttrIntros  ->
                           T_AttrIntroDecl 
sem_AttrIntroDecl_Attrs inhs_ inhsyns_ syns_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOpp :: PP_Doc
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOuniq :: Int
              _inhsOopts :: Opts
              _inhsOscmNm :: Nm
              _inhsOuniq :: Int
              _inhsynsOopts :: Opts
              _inhsynsOscmNm :: Nm
              _inhsynsOuniq :: Int
              _synsOopts :: Opts
              _synsOscmNm :: Nm
              _synsOuniq :: Int
              _inhsIatBldL :: ([ScAtBld])
              _inhsIatGam :: AtGam
              _inhsIerrL :: ([Err])
              _inhsIgathScDpds :: ([(Nm,Nm)])
              _inhsIpp :: PP_Doc
              _inhsIuniq :: Int
              _inhsynsIatBldL :: ([ScAtBld])
              _inhsynsIatGam :: AtGam
              _inhsynsIerrL :: ([Err])
              _inhsynsIgathScDpds :: ([(Nm,Nm)])
              _inhsynsIpp :: PP_Doc
              _inhsynsIuniq :: Int
              _synsIatBldL :: ([ScAtBld])
              _synsIatGam :: AtGam
              _synsIerrL :: ([Err])
              _synsIgathScDpds :: ([(Nm,Nm)])
              _synsIpp :: PP_Doc
              _synsIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 146, column 21)
              _atGam =
                  let t = const True
                  in  atGamUpdDirs t [AtInh] _inhsIatGam `gamUnion` atGamUpdDirs t [AtInh,AtSyn] _inhsynsIatGam `gamUnion` atGamUpdDirs t [AtSyn] _synsIatGam
              -- "build/ruler2/Main1AG.ag"(line 149, column 21)
              _lhsOatBldL =
                  [ScAtBldDirect _atGam]
              -- "build/ruler2/AS1/Pretty.ag"(line 44, column 21)
              _lhsOpp =
                  "inh" >#< _inhsIpp >-< "i+s" >#< _inhsynsIpp >-< "syn" >#< _synsIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _atGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _inhsIerrL ++ _inhsynsIerrL ++ _synsIerrL
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _inhsIgathScDpds ++ _inhsynsIgathScDpds ++ _synsIgathScDpds
              -- copy rule (up)
              _lhsOuniq =
                  _synsIuniq
              -- copy rule (down)
              _inhsOopts =
                  _lhsIopts
              -- copy rule (down)
              _inhsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _inhsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _inhsynsOopts =
                  _lhsIopts
              -- copy rule (down)
              _inhsynsOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _inhsynsOuniq =
                  _inhsIuniq
              -- copy rule (down)
              _synsOopts =
                  _lhsIopts
              -- copy rule (down)
              _synsOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _synsOuniq =
                  _inhsynsIuniq
              ( _inhsIatBldL,_inhsIatGam,_inhsIerrL,_inhsIgathScDpds,_inhsIpp,_inhsIuniq) =
                  inhs_ _inhsOopts _inhsOscmNm _inhsOuniq 
              ( _inhsynsIatBldL,_inhsynsIatGam,_inhsynsIerrL,_inhsynsIgathScDpds,_inhsynsIpp,_inhsynsIuniq) =
                  inhsyns_ _inhsynsOopts _inhsynsOscmNm _inhsynsOuniq 
              ( _synsIatBldL,_synsIatGam,_synsIerrL,_synsIgathScDpds,_synsIpp,_synsIuniq) =
                  syns_ _synsOopts _synsOscmNm _synsOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrIntroDecl_AttrsProp :: T_AttrIntros  ->
                               T_AttrIntroDecl 
sem_AttrIntroDecl_AttrsProp intros_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _introsOopts :: Opts
              _introsOscmNm :: Nm
              _introsOuniq :: Int
              _introsIatBldL :: ([ScAtBld])
              _introsIatGam :: AtGam
              _introsIerrL :: ([Err])
              _introsIgathScDpds :: ([(Nm,Nm)])
              _introsIpp :: PP_Doc
              _introsIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 148, column 21)
              _atGam =
                  atGamUpdDirs null [AtInh,AtSyn] _introsIatGam
              -- "build/ruler2/Main1AG.ag"(line 149, column 21)
              _lhsOatBldL =
                  [ScAtBldDirect _atGam]
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _atGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _introsIerrL
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _introsIgathScDpds
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _introsIpp
              -- copy rule (up)
              _lhsOuniq =
                  _introsIuniq
              -- copy rule (down)
              _introsOopts =
                  _lhsIopts
              -- copy rule (down)
              _introsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _introsOuniq =
                  _lhsIuniq
              ( _introsIatBldL,_introsIatGam,_introsIerrL,_introsIgathScDpds,_introsIpp,_introsIuniq) =
                  intros_ _introsOopts _introsOscmNm _introsOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrIntroDecl_Scheme :: SPos ->
                            Nm ->
                            T_AttrRenames  ->
                            T_AttrIntroDecl 
sem_AttrIntroDecl_Scheme pos_ nm_ renames_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _renamesOopts :: Opts
              _renamesOscmNm :: Nm
              _renamesOuniq :: Int
              _renamesIatBldL :: ([ScAtBld])
              _renamesIatGam :: AtGam
              _renamesIatRnmL :: ([ScAtBldRename])
              _renamesIerrL :: ([Err])
              _renamesIgathScDpds :: ([(Nm,Nm)])
              _renamesIpp :: PP_Doc
              _renamesIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 150, column 21)
              _lhsOatBldL =
                  [ScAtBldScheme nm_ pos_ _renamesIatRnmL]
              -- "build/ruler2/AS1/SchemeDpd.ag"(line 5, column 21)
              _lhsOgathScDpds =
                  [(_lhsIscmNm,nm_)]
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _renamesIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _renamesIerrL
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _renamesIpp
              -- copy rule (up)
              _lhsOuniq =
                  _renamesIuniq
              -- copy rule (down)
              _renamesOopts =
                  _lhsIopts
              -- copy rule (down)
              _renamesOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _renamesOuniq =
                  _lhsIuniq
              ( _renamesIatBldL,_renamesIatGam,_renamesIatRnmL,_renamesIerrL,_renamesIgathScDpds,_renamesIpp,_renamesIuniq) =
                  renames_ _renamesOopts _renamesOscmNm _renamesOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- AttrIntroDecls ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : AttrIntroDecl 
         child tl             : AttrIntroDecls 
      alternative Nil:
-}
-- cata
sem_AttrIntroDecls :: AttrIntroDecls  ->
                      T_AttrIntroDecls 
sem_AttrIntroDecls list  =
    (Prelude.foldr sem_AttrIntroDecls_Cons sem_AttrIntroDecls_Nil (Prelude.map sem_AttrIntroDecl list) )
-- semantic domain
type T_AttrIntroDecls  = Opts ->
                         Nm ->
                         Int ->
                         ( ([ScAtBld]),AtGam,([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrIntroDecls_Cons :: T_AttrIntroDecl  ->
                           T_AttrIntroDecls  ->
                           T_AttrIntroDecls 
sem_AttrIntroDecls_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOscmNm :: Nm
              _hdOuniq :: Int
              _tlOopts :: Opts
              _tlOscmNm :: Nm
              _tlOuniq :: Int
              _hdIatBldL :: ([ScAtBld])
              _hdIatGam :: AtGam
              _hdIerrL :: ([Err])
              _hdIgathScDpds :: ([(Nm,Nm)])
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIatBldL :: ([ScAtBld])
              _tlIatGam :: AtGam
              _tlIerrL :: ([Err])
              _tlIgathScDpds :: ([(Nm,Nm)])
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _hdIatBldL ++ _tlIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _hdIatGam `gamUnion` _tlIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _hdIgathScDpds ++ _tlIgathScDpds
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIatBldL,_hdIatGam,_hdIerrL,_hdIgathScDpds,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOscmNm _hdOuniq 
              ( _tlIatBldL,_tlIatGam,_tlIerrL,_tlIgathScDpds,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOscmNm _tlOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrIntroDecls_Nil :: T_AttrIntroDecls 
sem_AttrIntroDecls_Nil  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- AttrIntros --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : AttrIntro 
         child tl             : AttrIntros 
      alternative Nil:
-}
-- cata
sem_AttrIntros :: AttrIntros  ->
                  T_AttrIntros 
sem_AttrIntros list  =
    (Prelude.foldr sem_AttrIntros_Cons sem_AttrIntros_Nil (Prelude.map sem_AttrIntro list) )
-- semantic domain
type T_AttrIntros  = Opts ->
                     Nm ->
                     Int ->
                     ( ([ScAtBld]),AtGam,([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrIntros_Cons :: T_AttrIntro  ->
                       T_AttrIntros  ->
                       T_AttrIntros 
sem_AttrIntros_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOscmNm :: Nm
              _hdOuniq :: Int
              _tlOopts :: Opts
              _tlOscmNm :: Nm
              _tlOuniq :: Int
              _hdIatBldL :: ([ScAtBld])
              _hdIatGam :: AtGam
              _hdIerrL :: ([Err])
              _hdIgathScDpds :: ([(Nm,Nm)])
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIatBldL :: ([ScAtBld])
              _tlIatGam :: AtGam
              _tlIerrL :: ([Err])
              _tlIgathScDpds :: ([(Nm,Nm)])
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _hdIatBldL ++ _tlIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _hdIatGam `gamUnion` _tlIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _hdIgathScDpds ++ _tlIgathScDpds
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIatBldL,_hdIatGam,_hdIerrL,_hdIgathScDpds,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOscmNm _hdOuniq 
              ( _tlIatBldL,_tlIatGam,_tlIerrL,_tlIgathScDpds,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOscmNm _tlOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrIntros_Nil :: T_AttrIntros 
sem_AttrIntros_Nil  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- AttrRename --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         atRnmL               : [ScAtBldRename]
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative EqualTo:
         child pos            : {SPos}
         child nmLeft         : {Nm}
         child nmRight        : {Nm}
      alternative Rename:
         child pos            : {SPos}
         child nmNew          : {Nm}
         child nmOld          : {Nm}
-}
-- cata
sem_AttrRename :: AttrRename  ->
                  T_AttrRename 
sem_AttrRename (AttrRename_EqualTo _pos _nmLeft _nmRight )  =
    (sem_AttrRename_EqualTo _pos _nmLeft _nmRight )
sem_AttrRename (AttrRename_Rename _pos _nmNew _nmOld )  =
    (sem_AttrRename_Rename _pos _nmNew _nmOld )
-- semantic domain
type T_AttrRename  = Opts ->
                     Nm ->
                     Int ->
                     ( ([ScAtBld]),AtGam,([ScAtBldRename]),([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrRename_EqualTo :: SPos ->
                          Nm ->
                          Nm ->
                          T_AttrRename 
sem_AttrRename_EqualTo pos_ nmLeft_ nmRight_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatRnmL :: ([ScAtBldRename])
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 156, column 21)
              _lhsOatRnmL =
                  [ScAtBldEqualTo nmLeft_ nmRight_]
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOatRnmL,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrRename_Rename :: SPos ->
                         Nm ->
                         Nm ->
                         T_AttrRename 
sem_AttrRename_Rename pos_ nmNew_ nmOld_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatRnmL :: ([ScAtBldRename])
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 155, column 21)
              _lhsOatRnmL =
                  [ScAtBldRename nmNew_ nmOld_]
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOatRnmL,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- AttrRenames -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         scmNm                : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         atBldL               : [ScAtBld]
         atGam                : AtGam
         atRnmL               : [ScAtBldRename]
         errL                 : [Err]
         gathScDpds           : [(Nm,Nm)]
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : AttrRename 
         child tl             : AttrRenames 
      alternative Nil:
-}
-- cata
sem_AttrRenames :: AttrRenames  ->
                   T_AttrRenames 
sem_AttrRenames list  =
    (Prelude.foldr sem_AttrRenames_Cons sem_AttrRenames_Nil (Prelude.map sem_AttrRename list) )
-- semantic domain
type T_AttrRenames  = Opts ->
                      Nm ->
                      Int ->
                      ( ([ScAtBld]),AtGam,([ScAtBldRename]),([Err]),([(Nm,Nm)]),PP_Doc,Int)
sem_AttrRenames_Cons :: T_AttrRename  ->
                        T_AttrRenames  ->
                        T_AttrRenames 
sem_AttrRenames_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOatRnmL :: ([ScAtBldRename])
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOscmNm :: Nm
              _hdOuniq :: Int
              _tlOopts :: Opts
              _tlOscmNm :: Nm
              _tlOuniq :: Int
              _hdIatBldL :: ([ScAtBld])
              _hdIatGam :: AtGam
              _hdIatRnmL :: ([ScAtBldRename])
              _hdIerrL :: ([Err])
              _hdIgathScDpds :: ([(Nm,Nm)])
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIatBldL :: ([ScAtBld])
              _tlIatGam :: AtGam
              _tlIatRnmL :: ([ScAtBldRename])
              _tlIerrL :: ([Err])
              _tlIgathScDpds :: ([(Nm,Nm)])
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _hdIatBldL ++ _tlIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _hdIatGam `gamUnion` _tlIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 152, column 33)
              _lhsOatRnmL =
                  _hdIatRnmL ++ _tlIatRnmL
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _hdIgathScDpds ++ _tlIgathScDpds
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIatBldL,_hdIatGam,_hdIatRnmL,_hdIerrL,_hdIgathScDpds,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOscmNm _hdOuniq 
              ( _tlIatBldL,_tlIatGam,_tlIatRnmL,_tlIerrL,_tlIgathScDpds,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOscmNm _tlOuniq 
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOatRnmL,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
sem_AttrRenames_Nil :: T_AttrRenames 
sem_AttrRenames_Nil  =
    (\ _lhsIopts
       _lhsIscmNm
       _lhsIuniq ->
         (let _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOatRnmL :: ([ScAtBldRename])
              _lhsOerrL :: ([Err])
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 152, column 33)
              _lhsOatRnmL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOatBldL,_lhsOatGam,_lhsOatRnmL,_lhsOerrL,_lhsOgathScDpds,_lhsOpp,_lhsOuniq)))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         drvGam               : DrvGam
         fmGam                : FmGam Expr
         opts                 : Opts
         rlSelIsSel           : RlSelIsSel
         rsGam                : RsGam Expr
         ruleNm               : Nm
         rwGam                : RwExprGam
         scDpdGr              : DpdGr Nm
         scGam                : ScGam Expr
         scmNm                : Nm
         viewNm               : Nm
         vwDpdGr              : DpdGr Nm
      chained attributes:
         rlSeqNr              : Int
         uniq                 : Int
      synthesized attributes:
         allVwNmS             : Set.Set Nm
         atBldL               : [ScAtBld]
         atGam                : AtGam
         dtAltGam             : DtAltGam
         errL                 : [Err]
         explGam              : ExplGam Expr
         gathDrvGam           : DrvGam
         gathDtGam            : DtGam
         gathExtNmS           : Set.Set Nm
         gathFmGam            : FmGam Expr
         gathRsGam            : RsGam Expr
         gathRwGam            : RwExprGam
         gathScDpds           : [(Nm,Nm)]
         gathScGam            : ScGam Expr
         gathVwOrder          : [[[Nm]]]
         jdBldL               : [RlJdBld Expr]
         paGam                : FmKdGam String
         pp                   : PP_Doc
         rlGam                : RlGam Expr
         vwDtGam              : DtVwGam
         vwJdShpGam           : JdShpGam Expr
         vwRlGam              : VwRlGam Expr
         vwScGam              : VwScGam Expr
   alternatives:
      alternative Attr:
         child intros         : AttrIntroDecls 
      alternative DataAST:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNms      : {[Nm]}
         child decls          : Decls 
      alternative DataASTAlt:
         child pos            : {SPos}
         child nm             : {Nm}
         child ruleNm         : {Nm}
         child mbBasedOnNm    : {Maybe Nm}
         child fldIntros      : FldIntros 
         visit 0:
            local seqNr       : _
      alternative DataASTView:
         child pos            : {SPos}
         child nm             : {Nm}
         child decls          : Decls 
      alternative Explain:
         child mbNm           : {Maybe Nm}
         child expr           : Expr 
         visit 0:
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
      alternative Extern:
         child nms            : {[Nm]}
      alternative Fmt:
         child fmKind         : {FmKind}
         child atIO           : {AtDir}
         child matchExpr      : Expr 
         child expr           : Expr 
         visit 0:
            local _tup4       : {(FmGam Expr,RwExprGam)}
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
      alternative Include:
         child pos            : {SPos}
         child nm             : {Nm}
      alternative Preamble:
         child fmKind         : {FmKind}
         child preamble       : {String}
      alternative RulView:
         child pos            : {SPos}
         child nm             : {Nm}
         child jdIntros       : RuleJudgeIntros 
         child group          : {[[Nm]]}
         visit 0:
            local viewNm      : _
      alternative Rule:
         child pos            : {SPos}
         child nm             : {Nm}
         child mbBasedOnNm    : {Maybe Nm}
         child viewSel        : {Maybe ViewSel}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
         visit 0:
            local ruleNm      : _
            local errDupVw    : _
      alternative Rules:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNm       : {Nm}
         child viewSel        : {ViewSel}
         child info           : {String}
         child decls          : Decls 
      alternative RulesGroup:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNm       : {Nm}
         child viewSel        : {ViewSel}
         child info           : {String}
         child rlNms          : {[(Nm,Nm)]}
      alternative Scheme:
         child pos            : {SPos}
         child scKind         : {ScKind}
         child nm             : {Nm}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
         visit 0:
            local scmNm       : _
            local errDupVw    : _
      alternative SchemeDeriv:
         child pos            : {SPos}
         child scKind         : {ScKind}
         child nm             : {Nm}
         child scDeriv        : {ScDeriv}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
         visit 0:
            local firstVwNm   : _
      alternative ScmView:
         child nm             : {Nm}
         child decls          : Decls 
         visit 0:
            local viewNm      : _
      alternative ShpDel:
         child pos            : {SPos}
         child fmKinds        : {[FmKind]}
      alternative ShpJudge:
         child pos            : {SPos}
         child fmKind         : {FmKind}
         child expr           : Expr 
         visit 0:
            local cxStr       : _
            local _tup5       : _
            local vwScInfo    : _
            local errVwSc     : _
            local errUndefs   : _
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
      alternative ViewHierarchy:
         child nmOrder        : {[[Nm]]}
-}
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl (Decl_Attr _intros )  =
    (sem_Decl_Attr (sem_AttrIntroDecls _intros ) )
sem_Decl (Decl_DataAST _pos _nm _schemeNms _decls )  =
    (sem_Decl_DataAST _pos _nm _schemeNms (sem_Decls _decls ) )
sem_Decl (Decl_DataASTAlt _pos _nm _ruleNm _mbBasedOnNm _fldIntros )  =
    (sem_Decl_DataASTAlt _pos _nm _ruleNm _mbBasedOnNm (sem_FldIntros _fldIntros ) )
sem_Decl (Decl_DataASTView _pos _nm _decls )  =
    (sem_Decl_DataASTView _pos _nm (sem_Decls _decls ) )
sem_Decl (Decl_Explain _mbNm _expr )  =
    (sem_Decl_Explain _mbNm (sem_Expr _expr ) )
sem_Decl (Decl_Extern _nms )  =
    (sem_Decl_Extern _nms )
sem_Decl (Decl_Fmt _fmKind _atIO _matchExpr _expr )  =
    (sem_Decl_Fmt _fmKind _atIO (sem_Expr _matchExpr ) (sem_Expr _expr ) )
sem_Decl (Decl_Include _pos _nm )  =
    (sem_Decl_Include _pos _nm )
sem_Decl (Decl_Preamble _fmKind _preamble )  =
    (sem_Decl_Preamble _fmKind _preamble )
sem_Decl (Decl_RulView _pos _nm _jdIntros _group )  =
    (sem_Decl_RulView _pos _nm (sem_RuleJudgeIntros _jdIntros ) _group )
sem_Decl (Decl_Rule _pos _nm _mbBasedOnNm _viewSel _mbAGNm _decls )  =
    (sem_Decl_Rule _pos _nm _mbBasedOnNm _viewSel _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_Rules _pos _nm _schemeNm _viewSel _info _decls )  =
    (sem_Decl_Rules _pos _nm _schemeNm _viewSel _info (sem_Decls _decls ) )
sem_Decl (Decl_RulesGroup _pos _nm _schemeNm _viewSel _info _rlNms )  =
    (sem_Decl_RulesGroup _pos _nm _schemeNm _viewSel _info _rlNms )
sem_Decl (Decl_Scheme _pos _scKind _nm _mbAGNm _decls )  =
    (sem_Decl_Scheme _pos _scKind _nm _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_SchemeDeriv _pos _scKind _nm _scDeriv _mbAGNm _decls )  =
    (sem_Decl_SchemeDeriv _pos _scKind _nm _scDeriv _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_ScmView _nm _decls )  =
    (sem_Decl_ScmView _nm (sem_Decls _decls ) )
sem_Decl (Decl_ShpDel _pos _fmKinds )  =
    (sem_Decl_ShpDel _pos _fmKinds )
sem_Decl (Decl_ShpJudge _pos _fmKind _expr )  =
    (sem_Decl_ShpJudge _pos _fmKind (sem_Expr _expr ) )
sem_Decl (Decl_ViewHierarchy _nmOrder )  =
    (sem_Decl_ViewHierarchy _nmOrder )
-- semantic domain
type T_Decl  = DrvGam ->
               (FmGam Expr) ->
               Opts ->
               RlSelIsSel ->
               Int ->
               (RsGam Expr) ->
               Nm ->
               RwExprGam ->
               (DpdGr Nm) ->
               (ScGam Expr) ->
               Nm ->
               Int ->
               Nm ->
               (DpdGr Nm) ->
               ( (Set.Set Nm),([ScAtBld]),AtGam,DtAltGam,([Err]),(ExplGam Expr),DrvGam,DtGam,(Set.Set Nm),(FmGam Expr),(RsGam Expr),RwExprGam,([(Nm,Nm)]),(ScGam Expr),([[[Nm]]]),([RlJdBld Expr]),(FmKdGam String),PP_Doc,(RlGam Expr),Int,Int,DtVwGam,(JdShpGam Expr),(VwRlGam Expr),(VwScGam Expr))
sem_Decl_Attr :: T_AttrIntroDecls  ->
                 T_Decl 
sem_Decl_Attr intros_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _introsOopts :: Opts
              _introsOscmNm :: Nm
              _introsOuniq :: Int
              _introsIatBldL :: ([ScAtBld])
              _introsIatGam :: AtGam
              _introsIerrL :: ([Err])
              _introsIgathScDpds :: ([(Nm,Nm)])
              _introsIpp :: PP_Doc
              _introsIuniq :: Int
              -- "build/ruler2/AS1/Pretty.ag"(line 27, column 21)
              _lhsOpp =
                  "holes"         >#< _introsIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _introsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _introsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _introsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _introsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _introsIuniq
              -- copy rule (down)
              _introsOopts =
                  _lhsIopts
              -- copy rule (down)
              _introsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _introsOuniq =
                  _lhsIuniq
              ( _introsIatBldL,_introsIatGam,_introsIerrL,_introsIgathScDpds,_introsIpp,_introsIuniq) =
                  intros_ _introsOopts _introsOscmNm _introsOuniq 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_DataAST :: SPos ->
                    Nm ->
                    ([Nm]) ->
                    T_Decls  ->
                    T_Decl 
sem_Decl_DataAST pos_ nm_ schemeNms_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathDtGam :: DtGam
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 115, column 21)
              _lhsOgathDtGam =
                  gamSingleton nm_ (DtInfo nm_ schemeNms_ _declsIvwDtGam)
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _declsIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _declsIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_DataASTAlt :: SPos ->
                       Nm ->
                       Nm ->
                       (Maybe Nm) ->
                       T_FldIntros  ->
                       T_Decl 
sem_Decl_DataASTAlt pos_ nm_ ruleNm_ mbBasedOnNm_ fldIntros_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOdtAltGam :: DtAltGam
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _fldIntrosOopts :: Opts
              _fldIntrosOseqNr :: Int
              _fldIntrosOuniq :: Int
              _fldIntrosIdtFldGam :: DtFldGam
              _fldIntrosIerrL :: ([Err])
              _fldIntrosIpp :: PP_Doc
              _fldIntrosIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 96, column 21)
              _seqNr =
                  1
              -- "build/ruler2/Main1AG.ag"(line 104, column 21)
              _lhsOdtAltGam =
                  gamSingleton nm_ (DtAltInfo nm_ ruleNm_ mbBasedOnNm_ _fldIntrosIdtFldGam)
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _fldIntrosIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _fldIntrosIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _fldIntrosIuniq
              -- copy rule (down)
              _fldIntrosOopts =
                  _lhsIopts
              -- copy rule (from local)
              _fldIntrosOseqNr =
                  _seqNr
              -- copy rule (down)
              _fldIntrosOuniq =
                  _lhsIuniq
              ( _fldIntrosIdtFldGam,_fldIntrosIerrL,_fldIntrosIpp,_fldIntrosIuniq) =
                  fldIntros_ _fldIntrosOopts _fldIntrosOseqNr _fldIntrosOuniq 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_DataASTView :: SPos ->
                        Nm ->
                        T_Decls  ->
                        T_Decl 
sem_Decl_DataASTView pos_ nm_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOvwDtGam :: DtVwGam
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 109, column 21)
              _lhsOvwDtGam =
                  gamSingleton nm_ (DtVwInfo nm_ _declsIdtAltGam emptyGam)
              -- "build/ruler2/AS1/ViewDpd.ag"(line 6, column 21)
              _lhsOallVwNmS =
                  Set.singleton nm_
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _declsIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Explain :: (Maybe Nm) ->
                    T_Expr  ->
                    T_Decl 
sem_Decl_Explain mbNm_ expr_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOexplGam :: (ExplGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 446, column 21)
              _lhsOexplGam =
                  gamSingleton (maybe nmNone id mbNm_) (ExplInfo _exprIself)
              -- "build/ruler2/AS1/Pretty.ag"(line 28, column 21)
              _lhsOpp =
                  "explain"       >#< mbNm_ >#< "=" >#< ppParens _exprIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Extern :: ([Nm]) ->
                   T_Decl 
sem_Decl_Extern nms_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 168, column 21)
              _lhsOgathExtNmS =
                  Set.fromList nms_
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Fmt :: FmKind ->
                AtDir ->
                T_Expr  ->
                T_Expr  ->
                T_Decl 
sem_Decl_Fmt fmKind_ atIO_ matchExpr_ expr_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let __tup4 :: ((FmGam Expr,RwExprGam))
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _matchExprOfmGam :: (FmGam Expr)
              _exprOfmGam :: (FmGam Expr)
              _matchExprIexprIsRw :: ExprIsRw
              _matchExprInmS :: (Set.Set Nm)
              _matchExprIpp :: PP_Doc
              _matchExprIself :: Expr 
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Main1AG.ag"(line 417, column 33)
              __tup4 =
                  case _matchExprIexprIsRw of
                    ExprIsRw  n  -> (emptyGam,rwSingleton n fmKind_ atIO_ (_matchExprIself,_exprIself))
                    ExprIsVar n  -> (fmSingleton n fmKind_ _exprIself,emptyGam)
                    ExprIsOther  -> (emptyGam,emptyGam)
              -- "build/ruler2/Main1AG.ag"(line 417, column 33)
              (_lhsOgathFmGam,_) =
                  __tup4
              -- "build/ruler2/Main1AG.ag"(line 417, column 33)
              (_,_lhsOgathRwGam) =
                  __tup4
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/AS1/Pretty.ag"(line 18, column 21)
              _lhsOpp =
                  "format"        >#< fmKind_ >#< _matchExprIpp >#< "=" >#< _exprIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _matchExprOfmGam =
                  _fmGam
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              ( _matchExprIexprIsRw,_matchExprInmS,_matchExprIpp,_matchExprIself) =
                  matchExpr_ _matchExprOfmGam 
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Include :: SPos ->
                    Nm ->
                    T_Decl 
sem_Decl_Include pos_ nm_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Preamble :: FmKind ->
                     String ->
                     T_Decl 
sem_Decl_Preamble fmKind_ preamble_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOpaGam :: (FmKdGam String)
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 455, column 21)
              _lhsOpaGam =
                  gamSingleton fmKind_ preamble_
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_RulView :: SPos ->
                    Nm ->
                    T_RuleJudgeIntros  ->
                    ([[Nm]]) ->
                    T_Decl 
sem_Decl_RulView pos_ nm_ jdIntros_ group_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _jdIntrosOopts :: Opts
              _jdIntrosOruleNm :: Nm
              _jdIntrosOscGam :: (ScGam Expr)
              _jdIntrosOscmNm :: Nm
              _jdIntrosOuniq :: Int
              _jdIntrosOviewNm :: Nm
              _jdIntrosIerrL :: ([Err])
              _jdIntrosIjdBldL :: ([RlJdBld Expr])
              _jdIntrosIpp :: PP_Doc
              _jdIntrosIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 334, column 21)
              _viewNm =
                  nm_
              -- "build/ruler2/Main1AG.ag"(line 348, column 21)
              _lhsOvwRlGam =
                  gamSingleton nm_ (mkVwRlInfo nm_ pos_ _jdIntrosIjdBldL group_)
              -- "build/ruler2/AS1/ViewDpd.ag"(line 6, column 21)
              _lhsOallVwNmS =
                  Set.singleton nm_
              -- "build/ruler2/AS1/Pretty.ag"(line 20, column 21)
              _lhsOpp =
                  "view"          >#< nm_ >-< "=" >#< _jdIntrosIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _jdIntrosIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _jdIntrosIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _jdIntrosIuniq
              -- copy rule (down)
              _jdIntrosOopts =
                  _lhsIopts
              -- copy rule (down)
              _jdIntrosOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _jdIntrosOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _jdIntrosOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _jdIntrosOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _jdIntrosOviewNm =
                  _viewNm
              ( _jdIntrosIerrL,_jdIntrosIjdBldL,_jdIntrosIpp,_jdIntrosIuniq) =
                  jdIntros_ _jdIntrosOopts _jdIntrosOruleNm _jdIntrosOscGam _jdIntrosOscmNm _jdIntrosOuniq _jdIntrosOviewNm 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Rule :: SPos ->
                 Nm ->
                 (Maybe Nm) ->
                 (Maybe ViewSel) ->
                 (Maybe String) ->
                 T_Decls  ->
                 T_Decl 
sem_Decl_Rule pos_ nm_ mbBasedOnNm_ viewSel_ mbAGNm_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOerrL :: ([Err])
              _lhsOrlGam :: (RlGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 84, column 21)
              _lhsOerrL =
                  _errDupVw ++ _declsIerrL
              -- "build/ruler2/Main1AG.ag"(line 328, column 21)
              _ruleNm =
                  nm_
              -- "build/ruler2/Main1AG.ag"(line 363, column 21)
              _lhsOrlGam =
                  gamSingleton nm_ (RlInfo nm_ pos_ mbBasedOnNm_ mbAGNm_ _lhsIrlSeqNr (fmap (viewSelNmS _lhsIvwDpdGr) viewSel_) _declsIvwRlGam)
              -- "build/ruler2/Main1AG.ag"(line 466, column 21)
              _errDupVw =
                  gamCheckDups pos_ "rule" "view" _declsIvwRlGam
              -- "build/ruler2/AS1/Misc.ag"(line 28, column 21)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr + 1
              -- "build/ruler2/AS1/Pretty.ag"(line 23, column 21)
              _lhsOpp =
                  "rule"          >#< nm_ >#< "viewsel" >#< pp viewSel_ >-< "=" >#< _declsIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _declsIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (from local)
              _declsOruleNm =
                  _ruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Rules :: SPos ->
                  Nm ->
                  Nm ->
                  ViewSel ->
                  String ->
                  T_Decls  ->
                  T_Decl 
sem_Decl_Rules pos_ nm_ schemeNm_ viewSel_ info_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathRsGam :: (RsGam Expr)
              _declsOrlSeqNr :: Int
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 394, column 21)
              _lhsOgathRsGam =
                  gamSingleton nm_ (RsInfo nm_ pos_ schemeNm_ (viewSelNmS _lhsIvwDpdGr viewSel_) info_ _declsIrlGam)
              -- "build/ruler2/AS1/Misc.ag"(line 27, column 21)
              _declsOrlSeqNr =
                  1
              -- "build/ruler2/AS1/Pretty.ag"(line 21, column 21)
              _lhsOpp =
                  "rules"         >#< nm_ >#< "scheme" >#< schemeNm_ >-< "=" >#< _declsIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _declsIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_RulesGroup :: SPos ->
                       Nm ->
                       Nm ->
                       ViewSel ->
                       String ->
                       ([(Nm,Nm)]) ->
                       T_Decl 
sem_Decl_RulesGroup pos_ nm_ schemeNm_ viewSel_ info_ rlNms_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathRsGam :: (RsGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 395, column 21)
              _lhsOgathRsGam =
                  gamSingleton nm_ (RsInfoGroup nm_ pos_ schemeNm_ (viewSelNmS _lhsIvwDpdGr viewSel_) info_ rlNms_)
              -- "build/ruler2/AS1/Pretty.ag"(line 22, column 21)
              _lhsOpp =
                  "rulesgroup"    >#< nm_ >#< "scheme" >#< schemeNm_ >-< "=" >#< (vlist . map (\(rs,r) -> "rule" >#< rs >#< r) $ rlNms_)
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_Scheme :: SPos ->
                   ScKind ->
                   Nm ->
                   (Maybe String) ->
                   T_Decls  ->
                   T_Decl 
sem_Decl_Scheme pos_ scKind_ nm_ mbAGNm_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOerrL :: ([Err])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 83, column 21)
              _lhsOerrL =
                  _errDupVw ++ _declsIerrL
              -- "build/ruler2/Main1AG.ag"(line 273, column 21)
              _lhsOgathScGam =
                  gamSingleton nm_ (ScInfo pos_ nm_ mbAGNm_ scKind_ _declsIvwScGam)
              -- "build/ruler2/Main1AG.ag"(line 331, column 21)
              _scmNm =
                  nm_
              -- "build/ruler2/Main1AG.ag"(line 467, column 21)
              _errDupVw =
                  gamCheckDups pos_ "scheme" "view" _declsIvwScGam
              -- "build/ruler2/AS1/Pretty.ag"(line 16, column 21)
              _lhsOpp =
                  "scheme"        >#< scKind_ >#< nm_ >-< "=" >#< _declsIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _declsIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (from local)
              _declsOscmNm =
                  _scmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_SchemeDeriv :: SPos ->
                        ScKind ->
                        Nm ->
                        ScDeriv ->
                        (Maybe String) ->
                        T_Decls  ->
                        T_Decl 
sem_Decl_SchemeDeriv pos_ scKind_ nm_ scDeriv_ mbAGNm_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathDrvGam :: DrvGam
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 202, column 21)
              _lhsOgathDrvGam =
                  let mkListSc si
                        = siNw
                        where siNw = ScInfo pos_ nm_ mbAGNm_ scKind_ vGm
                              vGm
                                = gamMapWithKey
                                    (\nVw vi
                                      -> let mbNdNm = atGamNode (vwscAtGam vi)
                                             (aGm,ndRenmG)
                                               = case mbNdNm of
                                                   Just n -> (n `Map.delete` vwscAtGam vi,fmGamFromList [(n,Expr_Var nm_)])
                                                   _      -> (vwscAtGam vi,emptyGam)
                                             aNdGamNw
                                               = if _firstVwNm == nVw
                                                 then gamSingleton nm_ (AtInfo nm_ [AtInh] [AtNode] (nmCapitalize nm_))
                                                 else emptyGam
                                             jdG = gamMap (\i -> i {jdshExpr = exprSubst _lhsIopts ndRenmG (jdshExpr i)}) (vwscJdShpGam vi)
                                         in  vi {vwscAtBldL = [ScAtBldDirect (aNdGamNw `gamUnion` aGm)], vwscJdShpGam = jdG}
                                    )
                                    (scVwGam si)
                              mkNm n = n `nmStrApd` Nm "s"
                      vwSels = dgVertices _lhsIvwDpdGr
                      mkg :: Ord k => (v -> k) -> [v] -> Gam k v
                      mkg mkn l = gamFromAssocs [ (mkn i,i) | i <- l ]
                      mkRsInfo nl scGam
                        = RsInfo (nm_ `nmApd` Nm "base") pos_ nm_ vwSels ("Rules for " ++ show nm_) (mkg rlNm [rl1])
                        where rl1
                                = RlInfo (nm_ `nmApd` Nm "cons") emptySPos Nothing (Just "Cons") 0 (Just vwSels) (mkg vwrlNm [vw])
                                where nHd = Nm "hd"
                                      ndHd = nm_ `nmApd` nHd
                                      nTl = Nm "tl"
                                      ndTl = nm_ `nmApd` nTl
                                      vw = mkVwRlInfo _firstVwNm emptySPos [RlJdBldDirect Set.empty (mkg reNm [j1,j2]) (mkg reNm [j3])] []
                                      j3 = REInfoJudge nm_ nm_ Set.empty Set.empty (mkg jaNm [a3]) False
                                      a3 = mkJAInfo nm_ e3
                                      e3 = mkExprApp (Expr_ChildOrder 0 (Expr_Var ndHd))
                                                     [Expr_ChildOrder 1 (Expr_Var ndTl)]
                                      n2 = maybe nl id $ scVwGamNodeAt nl _firstVwNm scGam
                                      j2 = REInfoJudge nHd nl Set.empty Set.empty (mkg jaNm [a2]) False
                                      a2 = mkJAInfo n2 e2
                                      e2 = Expr_Var ndHd
                                      j1 = REInfoJudge nTl nm_ Set.empty Set.empty (mkg jaNm [a1]) False
                                      a1 = mkJAInfo nm_ e1
                                      e1 = Expr_Var ndTl
                  in  case scDeriv_ of
                        ScList nl -> (gamSingleton nl [(mkListSc,mkRsInfo nl)])
              -- "build/ruler2/AS1/ViewDpd.ag"(line 29, column 21)
              _firstVwNm =
                  last (dgTopSort _lhsIvwDpdGr)
              -- "build/ruler2/AS1/Pretty.ag"(line 17, column 21)
              _lhsOpp =
                  "scheme"        >#< scKind_ >#< nm_ >#< ":" >#< scDeriv_ >-< "=" >#< _declsIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _declsIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _declsIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _declsOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_ScmView :: Nm ->
                    T_Decls  ->
                    T_Decl 
sem_Decl_ScmView nm_ decls_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOvwScGam :: (VwScGam Expr)
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _declsOdrvGam :: DrvGam
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrlSelIsSel :: RlSelIsSel
              _declsOrlSeqNr :: Int
              _declsOrsGam :: (RsGam Expr)
              _declsOruleNm :: Nm
              _declsOrwGam :: RwExprGam
              _declsOscDpdGr :: (DpdGr Nm)
              _declsOscGam :: (ScGam Expr)
              _declsOscmNm :: Nm
              _declsOuniq :: Int
              _declsOviewNm :: Nm
              _declsOvwDpdGr :: (DpdGr Nm)
              _declsIallVwNmS :: (Set.Set Nm)
              _declsIatBldL :: ([ScAtBld])
              _declsIatGam :: AtGam
              _declsIdtAltGam :: DtAltGam
              _declsIerrL :: ([Err])
              _declsIexplGam :: (ExplGam Expr)
              _declsIgathDrvGam :: DrvGam
              _declsIgathDtGam :: DtGam
              _declsIgathExtNmS :: (Set.Set Nm)
              _declsIgathFmGam :: (FmGam Expr)
              _declsIgathRsGam :: (RsGam Expr)
              _declsIgathRwGam :: RwExprGam
              _declsIgathScDpds :: ([(Nm,Nm)])
              _declsIgathScGam :: (ScGam Expr)
              _declsIgathVwOrder :: ([[[Nm]]])
              _declsIjdBldL :: ([RlJdBld Expr])
              _declsIpaGam :: (FmKdGam String)
              _declsIpp :: PP_Doc
              _declsIrlGam :: (RlGam Expr)
              _declsIrlSeqNr :: Int
              _declsIuniq :: Int
              _declsIvwDtGam :: DtVwGam
              _declsIvwJdShpGam :: (JdShpGam Expr)
              _declsIvwRlGam :: (VwRlGam Expr)
              _declsIvwScGam :: (VwScGam Expr)
              -- "build/ruler2/Main1AG.ag"(line 178, column 21)
              _lhsOvwScGam =
                  gamSingleton nm_ (VwScInfo nm_ _declsIvwJdShpGam _declsIatBldL [] emptyGam emptyGam _declsIexplGam)
              -- "build/ruler2/Main1AG.ag"(line 334, column 21)
              _viewNm =
                  nm_
              -- "build/ruler2/AS1/ViewDpd.ag"(line 6, column 21)
              _lhsOallVwNmS =
                  Set.singleton nm_
              -- "build/ruler2/AS1/Pretty.ag"(line 19, column 21)
              _lhsOpp =
                  "view"          >#< nm_ >-< "=" >#< _declsIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _declsIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _declsIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _declsIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _declsIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _declsIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _declsIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _declsIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _declsIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _declsIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _declsIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _declsIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _declsIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _declsIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _declsIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _declsIpaGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _declsIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _declsIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _declsIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _declsIvwRlGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _declsIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _declsIuniq
              -- copy rule (down)
              _declsOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declsOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _declsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _declsOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _declsOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _declsOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _declsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _declsOviewNm =
                  _viewNm
              -- copy rule (down)
              _declsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _declsIallVwNmS,_declsIatBldL,_declsIatGam,_declsIdtAltGam,_declsIerrL,_declsIexplGam,_declsIgathDrvGam,_declsIgathDtGam,_declsIgathExtNmS,_declsIgathFmGam,_declsIgathRsGam,_declsIgathRwGam,_declsIgathScDpds,_declsIgathScGam,_declsIgathVwOrder,_declsIjdBldL,_declsIpaGam,_declsIpp,_declsIrlGam,_declsIrlSeqNr,_declsIuniq,_declsIvwDtGam,_declsIvwJdShpGam,_declsIvwRlGam,_declsIvwScGam) =
                  decls_ _declsOdrvGam _declsOfmGam _declsOopts _declsOrlSelIsSel _declsOrlSeqNr _declsOrsGam _declsOruleNm _declsOrwGam _declsOscDpdGr _declsOscGam _declsOscmNm _declsOuniq _declsOviewNm _declsOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_ShpDel :: SPos ->
                   ([FmKind]) ->
                   T_Decl 
sem_Decl_ShpDel pos_ fmKinds_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 187, column 21)
              _lhsOvwJdShpGam =
                  gamFromAssocs [ (k,JdShpDel) | k <- fmKinds_ ]
              -- "build/ruler2/AS1/Pretty.ag"(line 26, column 21)
              _lhsOpp =
                  "judgeshape"    >#< "-" >#< ppCommas' fmKinds_
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_ShpJudge :: SPos ->
                     FmKind ->
                     T_Expr  ->
                     T_Decl 
sem_Decl_ShpJudge pos_ fmKind_ expr_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOerrL :: ([Err])
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Main1AG.ag"(line 82, column 21)
              _lhsOerrL =
                  errFirst [_errVwSc,_errUndefs]
              -- "build/ruler2/Main1AG.ag"(line 179, column 21)
              _lhsOvwJdShpGam =
                  gamSingleton fmKind_ (JdShpInfo _exprIself)
              -- "build/ruler2/Main1AG.ag"(line 180, column 21)
              _cxStr =
                  "judgement for view '" ++ show _lhsIviewNm ++ "' for scheme '" ++ show _lhsIscmNm ++ "'"
              -- "build/ruler2/Main1AG.ag"(line 181, column 33)
              __tup5 =
                  case scVwGamLookup _lhsIscmNm _lhsIviewNm _lhsIscGam of
                      Just (_,i) -> (i,[])
                      Nothing    -> (emptyVwScInfo,[Err_UndefNm pos_ _cxStr "view" [_lhsIviewNm]])
              -- "build/ruler2/Main1AG.ag"(line 181, column 33)
              (_vwScInfo,_) =
                  __tup5
              -- "build/ruler2/Main1AG.ag"(line 181, column 33)
              (_,_errVwSc) =
                  __tup5
              -- "build/ruler2/Main1AG.ag"(line 180, column 21)
              _errUndefs =
                  let nms = gamKeys (vwscFullAtGam _vwScInfo) \\ Set.toList _exprInmS
                  in  []
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 435, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/AS1/Pretty.ag"(line 25, column 21)
              _lhsOpp =
                  "judgeshape"    >#< _exprIpp
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decl_ViewHierarchy :: ([[Nm]]) ->
                          T_Decl 
sem_Decl_ViewHierarchy nmOrder_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOpp :: PP_Doc
              _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- "build/ruler2/AS1/ViewDpd.ag"(line 15, column 21)
              _lhsOgathVwOrder =
                  [nmOrder_]
              -- "build/ruler2/AS1/Pretty.ag"(line 24, column 21)
              _lhsOpp =
                  "viewhierarchy" >#< text (show nmOrder_)
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         drvGam               : DrvGam
         fmGam                : FmGam Expr
         opts                 : Opts
         rlSelIsSel           : RlSelIsSel
         rsGam                : RsGam Expr
         ruleNm               : Nm
         rwGam                : RwExprGam
         scDpdGr              : DpdGr Nm
         scGam                : ScGam Expr
         scmNm                : Nm
         viewNm               : Nm
         vwDpdGr              : DpdGr Nm
      chained attributes:
         rlSeqNr              : Int
         uniq                 : Int
      synthesized attributes:
         allVwNmS             : Set.Set Nm
         atBldL               : [ScAtBld]
         atGam                : AtGam
         dtAltGam             : DtAltGam
         errL                 : [Err]
         explGam              : ExplGam Expr
         gathDrvGam           : DrvGam
         gathDtGam            : DtGam
         gathExtNmS           : Set.Set Nm
         gathFmGam            : FmGam Expr
         gathRsGam            : RsGam Expr
         gathRwGam            : RwExprGam
         gathScDpds           : [(Nm,Nm)]
         gathScGam            : ScGam Expr
         gathVwOrder          : [[[Nm]]]
         jdBldL               : [RlJdBld Expr]
         paGam                : FmKdGam String
         pp                   : PP_Doc
         rlGam                : RlGam Expr
         vwDtGam              : DtVwGam
         vwJdShpGam           : JdShpGam Expr
         vwRlGam              : VwRlGam Expr
         vwScGam              : VwScGam Expr
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls list  =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )
-- semantic domain
type T_Decls  = DrvGam ->
                (FmGam Expr) ->
                Opts ->
                RlSelIsSel ->
                Int ->
                (RsGam Expr) ->
                Nm ->
                RwExprGam ->
                (DpdGr Nm) ->
                (ScGam Expr) ->
                Nm ->
                Int ->
                Nm ->
                (DpdGr Nm) ->
                ( (Set.Set Nm),([ScAtBld]),AtGam,DtAltGam,([Err]),(ExplGam Expr),DrvGam,DtGam,(Set.Set Nm),(FmGam Expr),(RsGam Expr),RwExprGam,([(Nm,Nm)]),(ScGam Expr),([[[Nm]]]),([RlJdBld Expr]),(FmKdGam String),PP_Doc,(RlGam Expr),Int,Int,DtVwGam,(JdShpGam Expr),(VwRlGam Expr),(VwScGam Expr))
sem_Decls_Cons :: T_Decl  ->
                  T_Decls  ->
                  T_Decls 
sem_Decls_Cons hd_ tl_  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              _hdOdrvGam :: DrvGam
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrlSelIsSel :: RlSelIsSel
              _hdOrlSeqNr :: Int
              _hdOrsGam :: (RsGam Expr)
              _hdOruleNm :: Nm
              _hdOrwGam :: RwExprGam
              _hdOscDpdGr :: (DpdGr Nm)
              _hdOscGam :: (ScGam Expr)
              _hdOscmNm :: Nm
              _hdOuniq :: Int
              _hdOviewNm :: Nm
              _hdOvwDpdGr :: (DpdGr Nm)
              _tlOdrvGam :: DrvGam
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrlSelIsSel :: RlSelIsSel
              _tlOrlSeqNr :: Int
              _tlOrsGam :: (RsGam Expr)
              _tlOruleNm :: Nm
              _tlOrwGam :: RwExprGam
              _tlOscDpdGr :: (DpdGr Nm)
              _tlOscGam :: (ScGam Expr)
              _tlOscmNm :: Nm
              _tlOuniq :: Int
              _tlOviewNm :: Nm
              _tlOvwDpdGr :: (DpdGr Nm)
              _hdIallVwNmS :: (Set.Set Nm)
              _hdIatBldL :: ([ScAtBld])
              _hdIatGam :: AtGam
              _hdIdtAltGam :: DtAltGam
              _hdIerrL :: ([Err])
              _hdIexplGam :: (ExplGam Expr)
              _hdIgathDrvGam :: DrvGam
              _hdIgathDtGam :: DtGam
              _hdIgathExtNmS :: (Set.Set Nm)
              _hdIgathFmGam :: (FmGam Expr)
              _hdIgathRsGam :: (RsGam Expr)
              _hdIgathRwGam :: RwExprGam
              _hdIgathScDpds :: ([(Nm,Nm)])
              _hdIgathScGam :: (ScGam Expr)
              _hdIgathVwOrder :: ([[[Nm]]])
              _hdIjdBldL :: ([RlJdBld Expr])
              _hdIpaGam :: (FmKdGam String)
              _hdIpp :: PP_Doc
              _hdIrlGam :: (RlGam Expr)
              _hdIrlSeqNr :: Int
              _hdIuniq :: Int
              _hdIvwDtGam :: DtVwGam
              _hdIvwJdShpGam :: (JdShpGam Expr)
              _hdIvwRlGam :: (VwRlGam Expr)
              _hdIvwScGam :: (VwScGam Expr)
              _tlIallVwNmS :: (Set.Set Nm)
              _tlIatBldL :: ([ScAtBld])
              _tlIatGam :: AtGam
              _tlIdtAltGam :: DtAltGam
              _tlIerrL :: ([Err])
              _tlIexplGam :: (ExplGam Expr)
              _tlIgathDrvGam :: DrvGam
              _tlIgathDtGam :: DtGam
              _tlIgathExtNmS :: (Set.Set Nm)
              _tlIgathFmGam :: (FmGam Expr)
              _tlIgathRsGam :: (RsGam Expr)
              _tlIgathRwGam :: RwExprGam
              _tlIgathScDpds :: ([(Nm,Nm)])
              _tlIgathScGam :: (ScGam Expr)
              _tlIgathVwOrder :: ([[[Nm]]])
              _tlIjdBldL :: ([RlJdBld Expr])
              _tlIpaGam :: (FmKdGam String)
              _tlIpp :: PP_Doc
              _tlIrlGam :: (RlGam Expr)
              _tlIrlSeqNr :: Int
              _tlIuniq :: Int
              _tlIvwDtGam :: DtVwGam
              _tlIvwJdShpGam :: (JdShpGam Expr)
              _tlIvwRlGam :: (VwRlGam Expr)
              _tlIvwScGam :: (VwScGam Expr)
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  _hdIallVwNmS `Set.union` _tlIallVwNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  _hdIatBldL ++ _tlIatBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  _hdIatGam `gamUnion` _tlIatGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  _hdIdtAltGam `gamUnion` _tlIdtAltGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  _hdIexplGam `gamUnion` _tlIexplGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  _hdIgathDrvGam `drvGamUnion` _tlIgathDrvGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  _hdIgathDtGam `gamUnion` _tlIgathDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  _hdIgathExtNmS `Set.union` _tlIgathExtNmS
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  _hdIgathFmGam `fmGamUnion` _tlIgathFmGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  _hdIgathRsGam `gamUnion` _tlIgathRsGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  _hdIgathRwGam `rwGamUnion` _tlIgathRwGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  _hdIgathScDpds ++ _tlIgathScDpds
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  _hdIgathScGam `gamUnion` _tlIgathScGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  _hdIgathVwOrder ++ _tlIgathVwOrder
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _hdIjdBldL ++ _tlIjdBldL
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  _hdIpaGam `gamUnion` _tlIpaGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  _hdIrlGam `gamUnion` _tlIrlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  _hdIvwDtGam `gamUnion` _tlIvwDtGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  _hdIvwJdShpGam `gamUnion` _tlIvwJdShpGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  _hdIvwRlGam `gamUnion` _tlIvwRlGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  _hdIvwScGam `gamUnion` _tlIvwScGam
              -- copy rule (up)
              _lhsOrlSeqNr =
                  _tlIrlSeqNr
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (down)
              _hdOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (down)
              _hdOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _hdOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _hdOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _hdOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _hdOvwDpdGr =
                  _lhsIvwDpdGr
              -- copy rule (down)
              _tlOdrvGam =
                  _lhsIdrvGam
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrlSelIsSel =
                  _lhsIrlSelIsSel
              -- copy rule (chain)
              _tlOrlSeqNr =
                  _hdIrlSeqNr
              -- copy rule (down)
              _tlOrsGam =
                  _lhsIrsGam
              -- copy rule (down)
              _tlOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscDpdGr =
                  _lhsIscDpdGr
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              -- copy rule (down)
              _tlOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _tlOvwDpdGr =
                  _lhsIvwDpdGr
              ( _hdIallVwNmS,_hdIatBldL,_hdIatGam,_hdIdtAltGam,_hdIerrL,_hdIexplGam,_hdIgathDrvGam,_hdIgathDtGam,_hdIgathExtNmS,_hdIgathFmGam,_hdIgathRsGam,_hdIgathRwGam,_hdIgathScDpds,_hdIgathScGam,_hdIgathVwOrder,_hdIjdBldL,_hdIpaGam,_hdIpp,_hdIrlGam,_hdIrlSeqNr,_hdIuniq,_hdIvwDtGam,_hdIvwJdShpGam,_hdIvwRlGam,_hdIvwScGam) =
                  hd_ _hdOdrvGam _hdOfmGam _hdOopts _hdOrlSelIsSel _hdOrlSeqNr _hdOrsGam _hdOruleNm _hdOrwGam _hdOscDpdGr _hdOscGam _hdOscmNm _hdOuniq _hdOviewNm _hdOvwDpdGr 
              ( _tlIallVwNmS,_tlIatBldL,_tlIatGam,_tlIdtAltGam,_tlIerrL,_tlIexplGam,_tlIgathDrvGam,_tlIgathDtGam,_tlIgathExtNmS,_tlIgathFmGam,_tlIgathRsGam,_tlIgathRwGam,_tlIgathScDpds,_tlIgathScGam,_tlIgathVwOrder,_tlIjdBldL,_tlIpaGam,_tlIpp,_tlIrlGam,_tlIrlSeqNr,_tlIuniq,_tlIvwDtGam,_tlIvwJdShpGam,_tlIvwRlGam,_tlIvwScGam) =
                  tl_ _tlOdrvGam _tlOfmGam _tlOopts _tlOrlSelIsSel _tlOrlSeqNr _tlOrsGam _tlOruleNm _tlOrwGam _tlOscDpdGr _tlOscGam _tlOscmNm _tlOuniq _tlOviewNm _tlOvwDpdGr 
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
sem_Decls_Nil :: T_Decls 
sem_Decls_Nil  =
    (\ _lhsIdrvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrlSelIsSel
       _lhsIrlSeqNr
       _lhsIrsGam
       _lhsIruleNm
       _lhsIrwGam
       _lhsIscDpdGr
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm
       _lhsIvwDpdGr ->
         (let _lhsOallVwNmS :: (Set.Set Nm)
              _lhsOatBldL :: ([ScAtBld])
              _lhsOatGam :: AtGam
              _lhsOdtAltGam :: DtAltGam
              _lhsOerrL :: ([Err])
              _lhsOexplGam :: (ExplGam Expr)
              _lhsOgathDrvGam :: DrvGam
              _lhsOgathDtGam :: DtGam
              _lhsOgathExtNmS :: (Set.Set Nm)
              _lhsOgathFmGam :: (FmGam Expr)
              _lhsOgathRsGam :: (RsGam Expr)
              _lhsOgathRwGam :: RwExprGam
              _lhsOgathScDpds :: ([(Nm,Nm)])
              _lhsOgathScGam :: (ScGam Expr)
              _lhsOgathVwOrder :: ([[[Nm]]])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpaGam :: (FmKdGam String)
              _lhsOpp :: PP_Doc
              _lhsOrlGam :: (RlGam Expr)
              _lhsOvwDtGam :: DtVwGam
              _lhsOvwJdShpGam :: (JdShpGam Expr)
              _lhsOvwRlGam :: (VwRlGam Expr)
              _lhsOvwScGam :: (VwScGam Expr)
              _lhsOrlSeqNr :: Int
              _lhsOuniq :: Int
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 2, column 29)
              _lhsOallVwNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 138, column 44)
              _lhsOatBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 137, column 43)
              _lhsOatGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 101, column 29)
              _lhsOdtAltGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 443, column 28)
              _lhsOexplGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 199, column 46)
              _lhsOgathDrvGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 111, column 30)
              _lhsOgathDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 162, column 31)
              _lhsOgathExtNmS =
                  Set.empty
              -- use rule "build/ruler2/Main1AG.ag"(line 401, column 30)
              _lhsOgathFmGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 369, column 50)
              _lhsOgathRsGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 404, column 30)
              _lhsOgathRwGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/SchemeDpd.ag"(line 2, column 48)
              _lhsOgathScDpds =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 255, column 30)
              _lhsOgathScGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/ViewDpd.ag"(line 12, column 32)
              _lhsOgathVwOrder =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 452, column 26)
              _lhsOpaGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/Main1AG.ag"(line 360, column 26)
              _lhsOrlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 106, column 28)
              _lhsOvwDtGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 175, column 31)
              _lhsOvwJdShpGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 345, column 28)
              _lhsOvwRlGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 174, column 28)
              _lhsOvwScGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOrlSeqNr =
                  _lhsIrlSeqNr
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOallVwNmS,_lhsOatBldL,_lhsOatGam,_lhsOdtAltGam,_lhsOerrL,_lhsOexplGam,_lhsOgathDrvGam,_lhsOgathDtGam,_lhsOgathExtNmS,_lhsOgathFmGam,_lhsOgathRsGam,_lhsOgathRwGam,_lhsOgathScDpds,_lhsOgathScGam,_lhsOgathVwOrder,_lhsOjdBldL,_lhsOpaGam,_lhsOpp,_lhsOrlGam,_lhsOrlSeqNr,_lhsOuniq,_lhsOvwDtGam,_lhsOvwJdShpGam,_lhsOvwRlGam,_lhsOvwScGam)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
      synthesized attributes:
         nmS                  : Set.Set Nm
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local self        : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
-}
-- cata
sem_ECnstr :: ECnstr  ->
              T_ECnstr 
sem_ECnstr (ECnstr_Empty )  =
    (sem_ECnstr_Empty )
sem_ECnstr (ECnstr_Ty _nms )  =
    (sem_ECnstr_Ty _nms )
sem_ECnstr (ECnstr_Var _nm )  =
    (sem_ECnstr_Var _nm )
-- semantic domain
type T_ECnstr  = (FmGam Expr) ->
                 ( (Set.Set Nm),PP_Doc,ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIfmGam ->
         (let _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: ECnstr 
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  empty
              -- self rule
              _self =
                  ECnstr_Empty
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOnmS,_lhsOpp,_lhsOself)))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ECnstr 
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 43, column 21)
              _lhsOpp =
                  ppCommas' nms_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ECnstr_Ty nms_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOnmS,_lhsOpp,_lhsOself)))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: ECnstr 
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 44, column 21)
              _lhsOpp =
                  pp nm_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  ECnstr_Var nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOnmS,_lhsOpp,_lhsOself)))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
      synthesized attributes:
         exprIsRw             : ExprIsRw
         nmS                  : Set.Set Nm
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local mbDstWd     : _
            local mbPrevNdStr : _
            local self        : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
      alternative Empty:
         visit 0:
            local self        : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Int:
         child int            : {String}
         visit 0:
            local self        : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local self        : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local self        : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local self        : _
      alternative Undefined:
         visit 0:
            local self        : _
      alternative Uniq:
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_AVar _anm )  =
    (sem_Expr_AVar (sem_ANm _anm ) )
sem_Expr (Expr_App _lExpr _rExpr )  =
    (sem_Expr_App (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_AppTop _expr )  =
    (sem_Expr_AppTop (sem_Expr _expr ) )
sem_Expr (Expr_ChildOrder _seqNr _expr )  =
    (sem_Expr_ChildOrder _seqNr (sem_Expr _expr ) )
sem_Expr (Expr_Cnstr _expr _cnstr )  =
    (sem_Expr_Cnstr (sem_Expr _expr ) (sem_ECnstr _cnstr ) )
sem_Expr (Expr_Empty )  =
    (sem_Expr_Empty )
sem_Expr (Expr_Expr _expr )  =
    (sem_Expr_Expr (sem_Expr _expr ) )
sem_Expr (Expr_Int _int )  =
    (sem_Expr_Int _int )
sem_Expr (Expr_LF _lExpr _rExpr )  =
    (sem_Expr_LF (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Named _nm _expr )  =
    (sem_Expr_Named _nm (sem_Expr _expr ) )
sem_Expr (Expr_Op _nm _nmExpr _lExpr _rExpr )  =
    (sem_Expr_Op _nm (sem_Expr _nmExpr ) (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Paren _expr )  =
    (sem_Expr_Paren (sem_Expr _expr ) )
sem_Expr (Expr_Retain _expr )  =
    (sem_Expr_Retain (sem_Expr _expr ) )
sem_Expr (Expr_SP _lExpr _rExpr )  =
    (sem_Expr_SP (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Sel _expr _selMbExpr )  =
    (sem_Expr_Sel (sem_Expr _expr ) (sem_MbExpr _selMbExpr ) )
sem_Expr (Expr_SelTop _expr )  =
    (sem_Expr_SelTop (sem_Expr _expr ) )
sem_Expr (Expr_StrAsIs _str )  =
    (sem_Expr_StrAsIs _str )
sem_Expr (Expr_StrText _str )  =
    (sem_Expr_StrText _str )
sem_Expr (Expr_Undefined )  =
    (sem_Expr_Undefined )
sem_Expr (Expr_Uniq )  =
    (sem_Expr_Uniq )
sem_Expr (Expr_Var _nm )  =
    (sem_Expr_Var _nm )
sem_Expr (Expr_Wrap _wrKind _expr )  =
    (sem_Expr_Wrap _wrKind (sem_Expr _expr ) )
sem_Expr (Expr_WrapCnstr _cnstr )  =
    (sem_Expr_WrapCnstr (sem_ECnstr _cnstr ) )
-- semantic domain
type T_Expr  = (FmGam Expr) ->
               ( ExprIsRw,(Set.Set Nm),PP_Doc,Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _anmOisDest :: Bool
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _anmOmbDstWd :: (Maybe (Int,Int))
              _anmOmbPrevNdStr :: (Maybe String)
              _anmImxDstAtWd :: Int
              _anmImxDstNdWd :: Int
              _anmIndStr :: String
              _anmInmS :: (Set.Set Nm)
              _anmIpp :: PP_Doc
              _anmIself :: ANm 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 61, column 21)
              _anmOisDest =
                  False
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 76, column 21)
              _mbDstWd =
                  Nothing
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 92, column 21)
              _mbPrevNdStr =
                  Nothing
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _anmInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _anmIpp
              -- self rule
              _self =
                  Expr_AVar _anmIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _anmOmbDstWd =
                  _mbDstWd
              -- copy rule (from local)
              _anmOmbPrevNdStr =
                  _mbPrevNdStr
              ( _anmImxDstAtWd,_anmImxDstNdWd,_anmIndStr,_anmInmS,_anmIpp,_anmIself) =
                  anm_ _anmOisDest _anmOmbDstWd _anmOmbPrevNdStr 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
              _lExprIexprIsRw :: ExprIsRw
              _lExprInmS :: (Set.Set Nm)
              _lExprIpp :: PP_Doc
              _lExprIself :: Expr 
              _rExprIexprIsRw :: ExprIsRw
              _rExprInmS :: (Set.Set Nm)
              _rExprIpp :: PP_Doc
              _rExprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 7, column 21)
              _lhsOexprIsRw =
                  ExprIsRw nmApp
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 9, column 21)
              _lhsOpp =
                  exprNeedPar ParCtxtAppL nmUnk _lExprIself _lExprIpp
                  >#< exprNeedPar ParCtxtAppR nmUnk _rExprIself _rExprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _lExprInmS `Set.union` _rExprInmS
              -- self rule
              _self =
                  Expr_App _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIexprIsRw,_lExprInmS,_lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIexprIsRw,_rExprInmS,_rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _lhsOexprIsRw :: ExprIsRw
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 30, column 21)
              _lhsOpp =
                  _exprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- self rule
              _self =
                  Expr_AppTop _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOexprIsRw =
                  _exprIexprIsRw
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- self rule
              _self =
                  Expr_ChildOrder seqNr_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _cnstrOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              _cnstrInmS :: (Set.Set Nm)
              _cnstrIpp :: PP_Doc
              _cnstrIself :: ECnstr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 23, column 21)
              _lhsOpp =
                  ppCurlys (_exprIpp >|< "|" >|< _cnstrIpp)
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS `Set.union` _cnstrInmS
              -- self rule
              _self =
                  Expr_Cnstr _exprIself _cnstrIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
              ( _cnstrInmS,_cnstrIpp,_cnstrIself) =
                  cnstr_ _cnstrOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  empty
              -- self rule
              _self =
                  Expr_Empty
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- self rule
              _self =
                  Expr_Expr _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 20, column 21)
              _lhsOpp =
                  pp int_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  Expr_Int int_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
              _lExprIexprIsRw :: ExprIsRw
              _lExprInmS :: (Set.Set Nm)
              _lExprIpp :: PP_Doc
              _lExprIself :: Expr 
              _rExprIexprIsRw :: ExprIsRw
              _rExprInmS :: (Set.Set Nm)
              _rExprIpp :: PP_Doc
              _rExprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 17, column 21)
              _lhsOpp =
                  exprNeedPar ParCtxtAppR nmUnk _lExprIself _lExprIpp
                  >-< exprNeedPar ParCtxtAppR nmUnk _rExprIself _rExprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _lExprInmS `Set.union` _rExprInmS
              -- self rule
              _self =
                  Expr_LF _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIexprIsRw,_lExprInmS,_lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIexprIsRw,_rExprInmS,_rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 22, column 21)
              _lhsOpp =
                  _exprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- self rule
              _self =
                  Expr_Named nm_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _nmExprOfmGam :: (FmGam Expr)
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
              _nmExprIexprIsRw :: ExprIsRw
              _nmExprInmS :: (Set.Set Nm)
              _nmExprIpp :: PP_Doc
              _nmExprIself :: Expr 
              _lExprIexprIsRw :: ExprIsRw
              _lExprInmS :: (Set.Set Nm)
              _lExprIpp :: PP_Doc
              _lExprIself :: Expr 
              _rExprIexprIsRw :: ExprIsRw
              _rExprInmS :: (Set.Set Nm)
              _rExprIpp :: PP_Doc
              _rExprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 5, column 21)
              _lhsOexprIsRw =
                  let nm e = case e of {ExprIsRw n | n /= nmApp -> (`nmApd` n) ; _ -> id}
                  in  ExprIsRw .                            nm _rExprIexprIsRw $ nm_
              -- "build/ruler2/Expr/NmSAG.ag"(line 6, column 21)
              _lhsOnmS =
                  (nm_ `Set.delete` _nmExprInmS) `Set.union` _lExprInmS `Set.union` _rExprInmS
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 11, column 21)
              _lhsOpp =
                  let op = if nm_ == nmSp1 then empty else _nmExprIpp
                  in  ppExprMbEmpty _lExprIself (>|< " ")
                          (exprNeedPar ParCtxtOpL nm_ _lExprIself _lExprIpp)
                      >|< op
                      >|< ppExprMbEmpty _rExprIself (" " >|<)
                              (exprNeedPar ParCtxtOpR nm_ _rExprIself _rExprIpp)
              -- self rule
              _self =
                  Expr_Op nm_ _nmExprIself _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _nmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _nmExprIexprIsRw,_nmExprInmS,_nmExprIpp,_nmExprIself) =
                  nmExpr_ _nmExprOfmGam 
              ( _lExprIexprIsRw,_lExprInmS,_lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIexprIsRw,_rExprInmS,_rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 29, column 21)
              _lhsOpp =
                  _exprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- self rule
              _self =
                  Expr_Paren _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- self rule
              _self =
                  Expr_Retain _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _lExprOfmGam :: (FmGam Expr)
              _rExprOfmGam :: (FmGam Expr)
              _lExprIexprIsRw :: ExprIsRw
              _lExprInmS :: (Set.Set Nm)
              _lExprIpp :: PP_Doc
              _lExprIself :: Expr 
              _rExprIexprIsRw :: ExprIsRw
              _rExprInmS :: (Set.Set Nm)
              _rExprIpp :: PP_Doc
              _rExprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 19, column 21)
              _lhsOpp =
                  _lExprIpp >|< _rExprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _lExprInmS `Set.union` _rExprInmS
              -- self rule
              _self =
                  Expr_SP _lExprIself _rExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              ( _lExprIexprIsRw,_lExprInmS,_lExprIpp,_lExprIself) =
                  lExpr_ _lExprOfmGam 
              ( _rExprIexprIsRw,_rExprInmS,_rExprIpp,_rExprIself) =
                  rExpr_ _rExprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _selMbExprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              _selMbExprInmS :: (Set.Set Nm)
              _selMbExprIpp :: PP_Doc
              _selMbExprIself :: MbExpr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 27, column 21)
              _lhsOpp =
                  exprNeedPar ParCtxtOther nmUnk _exprIself _exprIpp >|< cfgStrSel >|< _selMbExprIpp
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS `Set.union` _selMbExprInmS
              -- self rule
              _self =
                  Expr_Sel _exprIself _selMbExprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _selMbExprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
              ( _selMbExprInmS,_selMbExprIpp,_selMbExprIself) =
                  selMbExpr_ _selMbExprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- self rule
              _self =
                  Expr_SelTop _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 21, column 21)
              _lhsOpp =
                  pp str_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  Expr_StrAsIs str_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 21, column 21)
              _lhsOpp =
                  pp str_
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  Expr_StrText str_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 25, column 21)
              _lhsOpp =
                  pp "_"
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  Expr_Undefined
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOpp :: PP_Doc
              _lhsOnmS :: (Set.Set Nm)
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 24, column 21)
              _lhsOpp =
                  pp "?uniq?"
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- self rule
              _self =
                  Expr_Uniq
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 8, column 21)
              _lhsOexprIsRw =
                  ExprIsVar nm_
              -- "build/ruler2/Expr/NmSAG.ag"(line 5, column 21)
              _lhsOnmS =
                  Set.singleton nm_
              -- "build/ruler2/Expr/PrettyPrintAG.ag"(line 28, column 21)
              _lhsOpp =
                  pp nm_
              -- self rule
              _self =
                  Expr_Var nm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _exprInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _exprIpp
              -- self rule
              _self =
                  Expr_Wrap wrKind_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIfmGam ->
         (let _lhsOexprIsRw :: ExprIsRw
              _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: Expr 
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrInmS :: (Set.Set Nm)
              _cnstrIpp :: PP_Doc
              _cnstrIself :: ECnstr 
              -- "build/ruler2/Expr/IsRwAG.ag"(line 10, column 21)
              _lhsOexprIsRw =
                  ExprIsOther
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _cnstrInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _cnstrIpp
              -- self rule
              _self =
                  Expr_WrapCnstr _cnstrIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              ( _cnstrInmS,_cnstrIpp,_cnstrIself) =
                  cnstr_ _cnstrOfmGam 
          in  ( _lhsOexprIsRw,_lhsOnmS,_lhsOpp,_lhsOself)))
-- FldIntro ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         seqNr                : Int
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         dtFldGam             : DtFldGam
         errL                 : [Err]
         pp                   : PP_Doc
   alternatives:
      alternative Intro:
         child nm             : {Nm}
         child ty             : Ty 
-}
-- cata
sem_FldIntro :: FldIntro  ->
                T_FldIntro 
sem_FldIntro (FldIntro_Intro _nm _ty )  =
    (sem_FldIntro_Intro _nm (sem_Ty _ty ) )
-- semantic domain
type T_FldIntro  = Opts ->
                   Int ->
                   Int ->
                   ( DtFldGam,([Err]),PP_Doc,Int)
sem_FldIntro_Intro :: Nm ->
                      T_Ty  ->
                      T_FldIntro 
sem_FldIntro_Intro nm_ ty_  =
    (\ _lhsIopts
       _lhsIseqNr
       _lhsIuniq ->
         (let _lhsOdtFldGam :: DtFldGam
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _tyIself :: Ty 
              -- "build/ruler2/Main1AG.ag"(line 99, column 21)
              _lhsOdtFldGam =
                  gamSingleton nm_ (DtFldInfo nm_ _tyIself _lhsIseqNr)
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              ( _tyIself) =
                  ty_ 
          in  ( _lhsOdtFldGam,_lhsOerrL,_lhsOpp,_lhsOuniq)))
-- FldIntros ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         seqNr                : Int
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         dtFldGam             : DtFldGam
         errL                 : [Err]
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : FldIntro 
         child tl             : FldIntros 
      alternative Nil:
-}
-- cata
sem_FldIntros :: FldIntros  ->
                 T_FldIntros 
sem_FldIntros list  =
    (Prelude.foldr sem_FldIntros_Cons sem_FldIntros_Nil (Prelude.map sem_FldIntro list) )
-- semantic domain
type T_FldIntros  = Opts ->
                    Int ->
                    Int ->
                    ( DtFldGam,([Err]),PP_Doc,Int)
sem_FldIntros_Cons :: T_FldIntro  ->
                      T_FldIntros  ->
                      T_FldIntros 
sem_FldIntros_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIseqNr
       _lhsIuniq ->
         (let _tlOseqNr :: Int
              _lhsOdtFldGam :: DtFldGam
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOseqNr :: Int
              _hdOuniq :: Int
              _tlOopts :: Opts
              _tlOuniq :: Int
              _hdIdtFldGam :: DtFldGam
              _hdIerrL :: ([Err])
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIdtFldGam :: DtFldGam
              _tlIerrL :: ([Err])
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 93, column 21)
              _tlOseqNr =
                  _lhsIseqNr + 1
              -- use rule "build/ruler2/Main1AG.ag"(line 90, column 44)
              _lhsOdtFldGam =
                  _hdIdtFldGam `gamUnion` _tlIdtFldGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIdtFldGam,_hdIerrL,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOseqNr _hdOuniq 
              ( _tlIdtFldGam,_tlIerrL,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOseqNr _tlOuniq 
          in  ( _lhsOdtFldGam,_lhsOerrL,_lhsOpp,_lhsOuniq)))
sem_FldIntros_Nil :: T_FldIntros 
sem_FldIntros_Nil  =
    (\ _lhsIopts
       _lhsIseqNr
       _lhsIuniq ->
         (let _lhsOdtFldGam :: DtFldGam
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 90, column 44)
              _lhsOdtFldGam =
                  emptyGam
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOdtFldGam,_lhsOerrL,_lhsOpp,_lhsOuniq)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         fmGam                : FmGam Expr
      synthesized attributes:
         nmS                  : Set.Set Nm
         pp                   : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local self        : _
      alternative Nothing:
         visit 0:
            local self        : _
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = (FmGam Expr) ->
                 ( (Set.Set Nm),PP_Doc,MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIfmGam ->
         (let _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: MbExpr 
              _justOfmGam :: (FmGam Expr)
              _justIexprIsRw :: ExprIsRw
              _justInmS :: (Set.Set Nm)
              _justIpp :: PP_Doc
              _justIself :: Expr 
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  _justInmS
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  _justIpp
              -- self rule
              _self =
                  Just _justIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _justOfmGam =
                  _lhsIfmGam
              ( _justIexprIsRw,_justInmS,_justIpp,_justIself) =
                  just_ _justOfmGam 
          in  ( _lhsOnmS,_lhsOpp,_lhsOself)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIfmGam ->
         (let _lhsOnmS :: (Set.Set Nm)
              _lhsOpp :: PP_Doc
              _lhsOself :: MbExpr 
              -- use rule "build/ruler2/Expr/NmSAG.ag"(line 2, column 34)
              _lhsOnmS =
                  Set.empty
              -- use rule "build/ruler2/Expr/PrettyPrintAG.ag"(line 6, column 33)
              _lhsOpp =
                  empty
              -- self rule
              _self =
                  Nothing
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOnmS,_lhsOpp,_lhsOself)))
-- RExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         ruleNm               : Nm
         scGam                : ScGam Expr
         viewNm               : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         pp                   : PP_Doc
         reGam                : REGam Expr
   alternatives:
      alternative Del:
         child pos            : {SPos}
         child nms            : {[Nm]}
      alternative Judge:
         child pos            : {SPos}
         child mbRNm          : {Maybe Nm}
         child schemeNm       : {Nm}
         child eqns           : RExprEqn 
         child isSmallExpr    : {Bool}
         visit 0:
            local nm          : _
            local cxStr       : _
            local cxStr2      : _
            local _tup6       : _
            local scInfo      : _
            local errSc       : _
            local _tup7       : _
            local vwScInfo    : _
            local errVwSc     : _
            local reInfo      : _
            local errJd       : _
            local _tup8       : _
            local lUniq       : _
-}
-- cata
sem_RExpr :: RExpr  ->
             T_RExpr 
sem_RExpr (RExpr_Del _pos _nms )  =
    (sem_RExpr_Del _pos _nms )
sem_RExpr (RExpr_Judge _pos _mbRNm _schemeNm _eqns _isSmallExpr )  =
    (sem_RExpr_Judge _pos _mbRNm _schemeNm (sem_RExprEqn _eqns ) _isSmallExpr )
-- semantic domain
type T_RExpr  = Opts ->
                Nm ->
                (ScGam Expr) ->
                Int ->
                Nm ->
                ( ([Err]),PP_Doc,(REGam Expr),Int)
sem_RExpr_Del :: SPos ->
                 ([Nm]) ->
                 T_RExpr 
sem_RExpr_Del pos_ nms_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOreGam :: (REGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOerrL :: ([Err])
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 317, column 21)
              _lhsOreGam =
                  gamSingleton (head nms_) (REInfoDel nms_)
              -- "build/ruler2/AS1/Pretty.ag"(line 35, column 21)
              _lhsOpp =
                  "judge" >#< "-" >#< ppCommas nms_
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOpp,_lhsOreGam,_lhsOuniq)))
sem_RExpr_Judge :: SPos ->
                   (Maybe Nm) ->
                   Nm ->
                   T_RExprEqn  ->
                   Bool ->
                   T_RExpr 
sem_RExpr_Judge pos_ mbRNm_ schemeNm_ eqns_ isSmallExpr_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOerrL :: ([Err])
              _eqnsOpos :: SPos
              _eqnsOschemeNm :: Nm
              _lhsOreGam :: (REGam Expr)
              _eqnsOuniq :: Int
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _eqnsOcxStr :: String
              _eqnsOopts :: Opts
              _eqnsOvwScInfo :: (VwScInfo Expr)
              _eqnsIerrL :: ([Err])
              _eqnsIjaGam :: (JAGam Expr)
              _eqnsIpp :: PP_Doc
              _eqnsIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 76, column 21)
              _lhsOerrL =
                  errFirst [_errSc,_errVwSc,_errJd,_eqnsIerrL]
              -- "build/ruler2/Main1AG.ag"(line 299, column 21)
              _eqnsOpos =
                  pos_
              -- "build/ruler2/Main1AG.ag"(line 299, column 21)
              _eqnsOschemeNm =
                  schemeNm_
              -- "build/ruler2/Main1AG.ag"(line 301, column 21)
              _nm =
                  maybe (Nm (strUnd ++ show _lUniq)) id mbRNm_
              -- "build/ruler2/Main1AG.ag"(line 301, column 21)
              _cxStr =
                  "judgement for view '" ++ show _lhsIviewNm ++ "' for rule '" ++ show _lhsIruleNm ++ "'"
              -- "build/ruler2/Main1AG.ag"(line 301, column 21)
              _cxStr2 =
                  _cxStr ++ " for scheme '" ++ show schemeNm_ ++ "'"
              -- "build/ruler2/Main1AG.ag"(line 304, column 33)
              __tup6 =
                  case gamLookup schemeNm_ _lhsIscGam of
                      Just i -> (i,[])
                      Nothing -> (emptyScInfo,[Err_UndefNm pos_ _cxStr "scheme" [schemeNm_]])
              -- "build/ruler2/Main1AG.ag"(line 304, column 33)
              (_scInfo,_) =
                  __tup6
              -- "build/ruler2/Main1AG.ag"(line 304, column 33)
              (_,_errSc) =
                  __tup6
              -- "build/ruler2/Main1AG.ag"(line 308, column 33)
              __tup7 =
                  case gamLookup _lhsIviewNm (scVwGam _scInfo) of
                      Just i -> (i,[])
                      Nothing -> (emptyVwScInfo,[Err_UndefNm pos_ _cxStr "view" [_lhsIviewNm]])
              -- "build/ruler2/Main1AG.ag"(line 308, column 33)
              (_vwScInfo,_) =
                  __tup7
              -- "build/ruler2/Main1AG.ag"(line 308, column 33)
              (_,_errVwSc) =
                  __tup7
              -- "build/ruler2/Main1AG.ag"(line 301, column 21)
              _reInfo =
                  REInfoJudge _nm schemeNm_ Set.empty Set.empty _eqnsIjaGam isSmallExpr_
              -- "build/ruler2/Main1AG.ag"(line 301, column 21)
              _errJd =
                  case gamKeys (_eqnsIjaGam `gamDifference` vwscFullAtGam _vwScInfo) of
                    [] -> []
                    ks -> [Err_NoXXFor pos_ _cxStr2 "scheme hole definition" ks]
              -- "build/ruler2/Main1AG.ag"(line 316, column 21)
              _lhsOreGam =
                  gamSingleton _nm _reInfo
              -- "build/ruler2/AS1/Misc.ag"(line 18, column 21)
              __tup8 =
                  (_lhsIuniq+1,_lhsIuniq)
              -- "build/ruler2/AS1/Misc.ag"(line 18, column 21)
              (_eqnsOuniq,_) =
                  __tup8
              -- "build/ruler2/AS1/Misc.ag"(line 18, column 21)
              (_,_lUniq) =
                  __tup8
              -- "build/ruler2/AS1/Pretty.ag"(line 34, column 21)
              _lhsOpp =
                  "judge" >#< maybe empty (\n -> pp n >#< "=") mbRNm_ >#< pp schemeNm_ >#< _eqnsIpp
              -- copy rule (up)
              _lhsOuniq =
                  _eqnsIuniq
              -- copy rule (from local)
              _eqnsOcxStr =
                  _cxStr
              -- copy rule (down)
              _eqnsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _eqnsOvwScInfo =
                  _vwScInfo
              ( _eqnsIerrL,_eqnsIjaGam,_eqnsIpp,_eqnsIuniq) =
                  eqns_ _eqnsOcxStr _eqnsOopts _eqnsOpos _eqnsOschemeNm _eqnsOuniq _eqnsOvwScInfo 
          in  ( _lhsOerrL,_lhsOpp,_lhsOreGam,_lhsOuniq)))
-- RExprEqn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cxStr                : String
         opts                 : Opts
         pos                  : SPos
         schemeNm             : Nm
         vwScInfo             : VwScInfo Expr
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         jaGam                : JAGam Expr
         pp                   : PP_Doc
   alternatives:
      alternative Attrs:
         child eqns           : AttrEqns 
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local _tup9       : _
            local jdshExpr    : _
            local errJd       : _
            local mt          : _
            local errMt       : _
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
-}
-- cata
sem_RExprEqn :: RExprEqn  ->
                T_RExprEqn 
sem_RExprEqn (RExprEqn_Attrs _eqns )  =
    (sem_RExprEqn_Attrs (sem_AttrEqns _eqns ) )
sem_RExprEqn (RExprEqn_Expr _expr )  =
    (sem_RExprEqn_Expr (sem_Expr _expr ) )
-- semantic domain
type T_RExprEqn  = String ->
                   Opts ->
                   SPos ->
                   Nm ->
                   Int ->
                   (VwScInfo Expr) ->
                   ( ([Err]),(JAGam Expr),PP_Doc,Int)
sem_RExprEqn_Attrs :: T_AttrEqns  ->
                      T_RExprEqn 
sem_RExprEqn_Attrs eqns_  =
    (\ _lhsIcxStr
       _lhsIopts
       _lhsIpos
       _lhsIschemeNm
       _lhsIuniq
       _lhsIvwScInfo ->
         (let _lhsOerrL :: ([Err])
              _lhsOjaGam :: (JAGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _eqnsOopts :: Opts
              _eqnsOuniq :: Int
              _eqnsIerrL :: ([Err])
              _eqnsIjaGam :: (JAGam Expr)
              _eqnsIpp :: PP_Doc
              _eqnsIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _eqnsIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 279, column 29)
              _lhsOjaGam =
                  _eqnsIjaGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _eqnsIpp
              -- copy rule (up)
              _lhsOuniq =
                  _eqnsIuniq
              -- copy rule (down)
              _eqnsOopts =
                  _lhsIopts
              -- copy rule (down)
              _eqnsOuniq =
                  _lhsIuniq
              ( _eqnsIerrL,_eqnsIjaGam,_eqnsIpp,_eqnsIuniq) =
                  eqns_ _eqnsOopts _eqnsOuniq 
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
sem_RExprEqn_Expr :: T_Expr  ->
                     T_RExprEqn 
sem_RExprEqn_Expr expr_  =
    (\ _lhsIcxStr
       _lhsIopts
       _lhsIpos
       _lhsIschemeNm
       _lhsIuniq
       _lhsIvwScInfo ->
         (let _lhsOerrL :: ([Err])
              _lhsOjaGam :: (JAGam Expr)
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _exprOfmGam :: (FmGam Expr)
              _exprIexprIsRw :: ExprIsRw
              _exprInmS :: (Set.Set Nm)
              _exprIpp :: PP_Doc
              _exprIself :: Expr 
              -- "build/ruler2/Main1AG.ag"(line 79, column 21)
              _lhsOerrL =
                  errFirst [_errMt,_errJd]
              -- "build/ruler2/Main1AG.ag"(line 288, column 33)
              __tup9 =
                  gamTryLookups (Expr_Empty,[Err_NoJdSpec _lhsIpos _lhsIcxStr [_lhsIschemeNm]])
                                (\i -> (jdshExpr i,[]))
                                [FmSpec,FmTeX,FmAll] (vwscJdShpGam _lhsIvwScInfo)
              -- "build/ruler2/Main1AG.ag"(line 288, column 33)
              (_jdshExpr,_) =
                  __tup9
              -- "build/ruler2/Main1AG.ag"(line 288, column 33)
              (_,_errJd) =
                  __tup9
              -- "build/ruler2/Main1AG.ag"(line 288, column 21)
              _mt =
                  exprMatch (_lhsIopts {optMatchROpOpnd = False}) emptyGam _exprIself _jdshExpr
              -- "build/ruler2/Main1AG.ag"(line 288, column 21)
              _errMt =
                  if mtMatches _mt then [] else [Err_Match _lhsIpos _lhsIcxStr (pp _exprIself) (pp _jdshExpr)]
              -- "build/ruler2/Main1AG.ag"(line 294, column 21)
              _lhsOjaGam =
                  fmGamToJaGam FmAll (mtFmGam _mt) `gamIntersection` vwscFullAtGam _lhsIvwScInfo
              -- "build/ruler2/Main1AG.ag"(line 424, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 424, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/Main1AG.ag"(line 424, column 21)
              _ecGam =
                  emptyGam
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _exprIpp
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              ( _exprIexprIsRw,_exprInmS,_exprIpp,_exprIself) =
                  expr_ _exprOfmGam 
          in  ( _lhsOerrL,_lhsOjaGam,_lhsOpp,_lhsOuniq)))
-- RExprs ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         ruleNm               : Nm
         scGam                : ScGam Expr
         viewNm               : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         pp                   : PP_Doc
         reGam                : REGam Expr
   alternatives:
      alternative Cons:
         child hd             : RExpr 
         child tl             : RExprs 
      alternative Nil:
-}
-- cata
sem_RExprs :: RExprs  ->
              T_RExprs 
sem_RExprs list  =
    (Prelude.foldr sem_RExprs_Cons sem_RExprs_Nil (Prelude.map sem_RExpr list) )
-- semantic domain
type T_RExprs  = Opts ->
                 Nm ->
                 (ScGam Expr) ->
                 Int ->
                 Nm ->
                 ( ([Err]),PP_Doc,(REGam Expr),Int)
sem_RExprs_Cons :: T_RExpr  ->
                   T_RExprs  ->
                   T_RExprs 
sem_RExprs_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOreGam :: (REGam Expr)
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOruleNm :: Nm
              _hdOscGam :: (ScGam Expr)
              _hdOuniq :: Int
              _hdOviewNm :: Nm
              _tlOopts :: Opts
              _tlOruleNm :: Nm
              _tlOscGam :: (ScGam Expr)
              _tlOuniq :: Int
              _tlOviewNm :: Nm
              _hdIerrL :: ([Err])
              _hdIpp :: PP_Doc
              _hdIreGam :: (REGam Expr)
              _hdIuniq :: Int
              _tlIerrL :: ([Err])
              _tlIpp :: PP_Doc
              _tlIreGam :: (REGam Expr)
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 296, column 27)
              _lhsOreGam =
                  _hdIreGam `gamUnion` _tlIreGam
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _hdOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              -- copy rule (down)
              _tlOviewNm =
                  _lhsIviewNm
              ( _hdIerrL,_hdIpp,_hdIreGam,_hdIuniq) =
                  hd_ _hdOopts _hdOruleNm _hdOscGam _hdOuniq _hdOviewNm 
              ( _tlIerrL,_tlIpp,_tlIreGam,_tlIuniq) =
                  tl_ _tlOopts _tlOruleNm _tlOscGam _tlOuniq _tlOviewNm 
          in  ( _lhsOerrL,_lhsOpp,_lhsOreGam,_lhsOuniq)))
sem_RExprs_Nil :: T_RExprs 
sem_RExprs_Nil  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOreGam :: (REGam Expr)
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- use rule "build/ruler2/Main1AG.ag"(line 296, column 27)
              _lhsOreGam =
                  emptyGam
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOpp,_lhsOreGam,_lhsOuniq)))
-- RuleJudgeIntro ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         ruleNm               : Nm
         scGam                : ScGam Expr
         scmNm                : Nm
         viewNm               : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         jdBldL               : [RlJdBld Expr]
         pp                   : PP_Doc
   alternatives:
      alternative PrePost:
         child extNms         : {[Nm]}
         child pre            : RExprs 
         child post           : RExprs 
      alternative RulesetRule:
         child pos            : {SPos}
         child rsNm           : {Nm}
         child rlNm           : {Nm}
         child schemeRnmL     : {[BldRename]}
-}
-- cata
sem_RuleJudgeIntro :: RuleJudgeIntro  ->
                      T_RuleJudgeIntro 
sem_RuleJudgeIntro (RuleJudgeIntro_PrePost _extNms _pre _post )  =
    (sem_RuleJudgeIntro_PrePost _extNms (sem_RExprs _pre ) (sem_RExprs _post ) )
sem_RuleJudgeIntro (RuleJudgeIntro_RulesetRule _pos _rsNm _rlNm _schemeRnmL )  =
    (sem_RuleJudgeIntro_RulesetRule _pos _rsNm _rlNm _schemeRnmL )
-- semantic domain
type T_RuleJudgeIntro  = Opts ->
                         Nm ->
                         (ScGam Expr) ->
                         Nm ->
                         Int ->
                         Nm ->
                         ( ([Err]),([RlJdBld Expr]),PP_Doc,Int)
sem_RuleJudgeIntro_PrePost :: ([Nm]) ->
                              T_RExprs  ->
                              T_RExprs  ->
                              T_RuleJudgeIntro 
sem_RuleJudgeIntro_PrePost extNms_ pre_ post_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpp :: PP_Doc
              _lhsOerrL :: ([Err])
              _lhsOuniq :: Int
              _preOopts :: Opts
              _preOruleNm :: Nm
              _preOscGam :: (ScGam Expr)
              _preOuniq :: Int
              _preOviewNm :: Nm
              _postOopts :: Opts
              _postOruleNm :: Nm
              _postOscGam :: (ScGam Expr)
              _postOuniq :: Int
              _postOviewNm :: Nm
              _preIerrL :: ([Err])
              _preIpp :: PP_Doc
              _preIreGam :: (REGam Expr)
              _preIuniq :: Int
              _postIerrL :: ([Err])
              _postIpp :: PP_Doc
              _postIreGam :: (REGam Expr)
              _postIuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 353, column 21)
              _lhsOjdBldL =
                  [RlJdBldDirect (Set.fromList extNms_) _preIreGam _postIreGam]
              -- "build/ruler2/AS1/Pretty.ag"(line 31, column 33)
              _lhsOpp =
                  _preIpp >-< "---" >-< _postIpp
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _preIerrL ++ _postIerrL
              -- copy rule (up)
              _lhsOuniq =
                  _postIuniq
              -- copy rule (down)
              _preOopts =
                  _lhsIopts
              -- copy rule (down)
              _preOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _preOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _preOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _preOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _postOopts =
                  _lhsIopts
              -- copy rule (down)
              _postOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _postOscGam =
                  _lhsIscGam
              -- copy rule (chain)
              _postOuniq =
                  _preIuniq
              -- copy rule (down)
              _postOviewNm =
                  _lhsIviewNm
              ( _preIerrL,_preIpp,_preIreGam,_preIuniq) =
                  pre_ _preOopts _preOruleNm _preOscGam _preOuniq _preOviewNm 
              ( _postIerrL,_postIpp,_postIreGam,_postIuniq) =
                  post_ _postOopts _postOruleNm _postOscGam _postOuniq _postOviewNm 
          in  ( _lhsOerrL,_lhsOjdBldL,_lhsOpp,_lhsOuniq)))
sem_RuleJudgeIntro_RulesetRule :: SPos ->
                                  Nm ->
                                  Nm ->
                                  ([BldRename]) ->
                                  T_RuleJudgeIntro 
sem_RuleJudgeIntro_RulesetRule pos_ rsNm_ rlNm_ schemeRnmL_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOerrL :: ([Err])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- "build/ruler2/Main1AG.ag"(line 354, column 21)
              _lhsOjdBldL =
                  [RlJdBldFromRuleset pos_ rsNm_ rlNm_ schemeRnmL_]
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOjdBldL,_lhsOpp,_lhsOuniq)))
-- RuleJudgeIntros ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         ruleNm               : Nm
         scGam                : ScGam Expr
         scmNm                : Nm
         viewNm               : Nm
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errL                 : [Err]
         jdBldL               : [RlJdBld Expr]
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : RuleJudgeIntro 
         child tl             : RuleJudgeIntros 
      alternative Nil:
-}
-- cata
sem_RuleJudgeIntros :: RuleJudgeIntros  ->
                       T_RuleJudgeIntros 
sem_RuleJudgeIntros list  =
    (Prelude.foldr sem_RuleJudgeIntros_Cons sem_RuleJudgeIntros_Nil (Prelude.map sem_RuleJudgeIntro list) )
-- semantic domain
type T_RuleJudgeIntros  = Opts ->
                          Nm ->
                          (ScGam Expr) ->
                          Nm ->
                          Int ->
                          Nm ->
                          ( ([Err]),([RlJdBld Expr]),PP_Doc,Int)
sem_RuleJudgeIntros_Cons :: T_RuleJudgeIntro  ->
                            T_RuleJudgeIntros  ->
                            T_RuleJudgeIntros 
sem_RuleJudgeIntros_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              _hdOopts :: Opts
              _hdOruleNm :: Nm
              _hdOscGam :: (ScGam Expr)
              _hdOscmNm :: Nm
              _hdOuniq :: Int
              _hdOviewNm :: Nm
              _tlOopts :: Opts
              _tlOruleNm :: Nm
              _tlOscGam :: (ScGam Expr)
              _tlOscmNm :: Nm
              _tlOuniq :: Int
              _tlOviewNm :: Nm
              _hdIerrL :: ([Err])
              _hdIjdBldL :: ([RlJdBld Expr])
              _hdIpp :: PP_Doc
              _hdIuniq :: Int
              _tlIerrL :: ([Err])
              _tlIjdBldL :: ([RlJdBld Expr])
              _tlIpp :: PP_Doc
              _tlIuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  _hdIjdBldL ++ _tlIjdBldL
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _hdOscmNm =
                  _lhsIscmNm
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _hdOviewNm =
                  _lhsIviewNm
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOruleNm =
                  _lhsIruleNm
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOscmNm =
                  _lhsIscmNm
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              -- copy rule (down)
              _tlOviewNm =
                  _lhsIviewNm
              ( _hdIerrL,_hdIjdBldL,_hdIpp,_hdIuniq) =
                  hd_ _hdOopts _hdOruleNm _hdOscGam _hdOscmNm _hdOuniq _hdOviewNm 
              ( _tlIerrL,_tlIjdBldL,_tlIpp,_tlIuniq) =
                  tl_ _tlOopts _tlOruleNm _tlOscGam _tlOscmNm _tlOuniq _tlOviewNm 
          in  ( _lhsOerrL,_lhsOjdBldL,_lhsOpp,_lhsOuniq)))
sem_RuleJudgeIntros_Nil :: T_RuleJudgeIntros 
sem_RuleJudgeIntros_Nil  =
    (\ _lhsIopts
       _lhsIruleNm
       _lhsIscGam
       _lhsIscmNm
       _lhsIuniq
       _lhsIviewNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOjdBldL :: ([RlJdBld Expr])
              _lhsOpp :: PP_Doc
              _lhsOuniq :: Int
              -- use rule "build/ruler2/Main1AG.ag"(line 70, column 30)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/Main1AG.ag"(line 350, column 49)
              _lhsOjdBldL =
                  []
              -- use rule "build/ruler2/AS1/Pretty.ag"(line 2, column 28)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOerrL,_lhsOjdBldL,_lhsOpp,_lhsOuniq)))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative App:
         child lTy            : Ty 
         child rTy            : Ty 
         visit 0:
            local self        : _
      alternative Con:
         child nm             : {Nm}
         visit 0:
            local self        : _
-}
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Ty_App _lTy _rTy )  =
    (sem_Ty_App (sem_Ty _lTy ) (sem_Ty _rTy ) )
sem_Ty (Ty_Con _nm )  =
    (sem_Ty_Con _nm )
-- semantic domain
type T_Ty  = ( Ty )
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App lTy_ rTy_  =
    (let _lhsOself :: Ty 
         _lTyIself :: Ty 
         _rTyIself :: Ty 
         -- self rule
         _self =
             Ty_App _lTyIself _rTyIself
         -- self rule
         _lhsOself =
             _self
         ( _lTyIself) =
             lTy_ 
         ( _rTyIself) =
             rTy_ 
     in  ( _lhsOself))
sem_Ty_Con :: Nm ->
              T_Ty 
sem_Ty_Con nm_  =
    (let _lhsOself :: Ty 
         -- self rule
         _self =
             Ty_Con nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
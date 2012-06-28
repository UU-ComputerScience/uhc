


module EH101.EH.MainAG_AGItf where

import Data.Char
import Data.List as List
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts
import EH101.Gam.Full
import EH101.Error
import EH101.Error.Pretty
import EH101.EH
import EH101.Ty.Pretty
import EH101.Ty.FitsInCommon
import EH101.Ty.FitsIn
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import EH101.VarMp
import EH101.Substitutable
import Data.Maybe
import EH101.Ty.Utils1
import EH101.Ty.Trf.Quantify
import EH101.Ty.Trf.Instantiate
import EH101.Ty
import EH101.Base.Debug as Debug
import Debug.Trace
import EH101.Ty.FitsInCommon2
import EH101.Ty.FIEnv2
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import EH101.Ty.Trf.FreshVar
import EH101.Ty.Ftv
import EH.Util.Utils (groupSortOn)
import Control.Applicative ((<|>))
import EH101.AbstractCore
import EH101.AbstractCore.Utils
import EH101.Core
import EH101.Core.FFI
import EH101.Core.Utils
import EH101.Core.Pretty
import EH101.Foreign.Extract
import EH101.LamInfo
import Control.Monad.State
import EH101.Ty.Utils2
import EH101.Base.Target
import EH101.Core.Subst
import EH101.Core.Coercion
import EH101.Ty.Trf.MergePreds
import EH101.Ty.Trf.Canonic
import EH101.Pred
import EH101.Pred.RedGraph (redPruneReductionsUntil)
import EH101.CHR
import EH101.CHR.Constraint
import EH101.Pred.CHR
import EH101.Pred.ToCHR
import EH101.Pred.Heuristics
import EH101.CHR.Solve
import EH101.Pred.EvidenceToCore
import EH101.Gam.ClassDefaultGam
import EH101.Ty.Trf.BetaReduce (tyBetaRedFull)
import EH101.Module
import EH101.Ty.UsedNames
import EH101.BuiltinPrims
import EH101.Foreign
import EH101.Foreign
import EH101.Foreign.Pretty
import EH101.Deriving
import EH101.Generics
import EH101.VarMp.Utils

import EH101.EH.MainAG_common

-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         clGam                : ClGam
         dataGam              : DataGam
         idQualGam            : IdQualGam
         isMainMod            : Bool
         kiGam                : KiGam
         moduleNm             : HsName
         opts                 : EHCOpts
         polGam               : PolGam
         tyGam                : TyGam
         tyKiGam              : TyKiGam
         valGam               : ValGam
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         allErrSq             : ErrSq
         cmodule              : CModule
         gathChrStore         : ScopedPredStore
         gathClDfGam          : ClassDefaultGam
         gathClGam            : ClGam
         gathDataGam          : DataGam
         gathHiddenExports    : Seq.Seq (HsName,IdOccKind)
         gathKiGam            : KiGam
         gathLamMp            : LamMp
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathPolGam           : PolGam
         gathTyGam            : TyGam
         gathTyKiGam          : TyKiGam
         gathValGam           : ValGam
         mbOrphan             : Maybe (Set.Set HsName)
         pp                   : PP_Doc
         topTyPP              : PP_Doc
   alternatives:
      alternative AGItf:
         child expr           : Expr 
         visit 0:
            local valGam      : _
            local tyKiGam     : _
            local tyGam       : _
            local predScope   : {PredScope}
            local predSameScopeCounter : {Int}
            local polGam      : _
            local lexLev      : {Int}
            local _tup8       : {(UID,UID,UID,UID,UID)}
            local kiGam       : _
            local kiVarMp     : _
            local clGam       : _
            local clDfGam     : _
            local lUniq       : {UID}
            local chrFIIn     : {FIIn}
            local _tup2       : _
            local instsOuter  : _
            local initChrStore : _
            local _tup3       : _
            local chrStoreOuterScope : _
            local instsInner  : _
            local _tup4       : _
            local chrStore    : _
            local rangeMp     : _
            local finValGam   : {ValGam}
            local lUniq_9_simplify : {UID}
            local toProveCnstrMp : _
            local toProveHereCnstrMp : _
            local chrProveFIIn : {FIIn}
            local _tup5       : _
            local chrSolveSimpTyVarMp : _
            local finTyVarMp  : _
            local lUniq_98_fitsIn_ty : {UID}
            local lUniq_98_IO_tvar : {UID}
            local chrSolveMainRemCnstrMp : _
            local cannotProveCnstrMp : _
            local chrSolveMainErrs : _
            local predNotPrfErrs : _
            local chrSolveMainScopeBindMp : _
            local _tup7       : _
            local chrScopeBindMp : _
            local chrSolveMainEvidBindMp : _
            local chrEvidBindMp : _
            local cSubst      : _
            local fe          : {FIEnv}
            local _tup1       : _
            local nmErrs      : {ErrL}
            local foMain      : _
            local lErrSq      : _
            local chrScopeMainBindL : _
            local extraCBindL : _
            local mainDefExists : _
            local insertMainDef : _
            local mainUseExists : _
            local insertMainUse : _
            local gathChrStore : _
            local chrSimplifyResult : _
            local gathMentrelFilterMp : _
            local extraPP     : _
            local errTopPP    : _
-}
sem_AGItf_AGItf :: T_Expr  ->
                   T_AGItf 

sem_AGItf_AGItf expr_  | expr_ `seq` (True) =
    (\ _lhsIchrStore
       _lhsIclDfGam
       _lhsIclGam
       _lhsIdataGam
       _lhsIgUniq
       _lhsIidQualGam
       _lhsIisMainMod
       _lhsIkiGam
       _lhsImoduleNm
       _lhsIopts
       _lhsIpolGam
       _lhsItyGam
       _lhsItyKiGam
       _lhsIvalGam ->
         _lhsIchrStore `seq`
         (_lhsIclDfGam `seq`
          (_lhsIclGam `seq`
           (_lhsIdataGam `seq`
            (_lhsIgUniq `seq`
             (_lhsIidQualGam `seq`
              (_lhsIisMainMod `seq`
               (_lhsIkiGam `seq`
                (_lhsImoduleNm `seq`
                 (_lhsIopts `seq`
                  (_lhsIpolGam `seq`
                   (_lhsItyGam `seq`
                    (_lhsItyKiGam `seq`
                     (_lhsIvalGam `seq`
                      ((case (_lhsIvalGam) of
                        { _valGam | _valGam `seq` (True) ->
                        (case (_valGam) of
                         { _exprOvalGam | _exprOvalGam `seq` (True) ->
                         (case (_lhsItyKiGam) of
                          { _tyKiGam | _tyKiGam `seq` (True) ->
                          (case (_tyKiGam) of
                           { _exprOtyKiGam | _exprOtyKiGam `seq` (True) ->
                           (case (_lhsItyGam) of
                            { _tyGam | _tyGam `seq` (True) ->
                            (case (_tyGam) of
                             { _exprOtyGam | _exprOtyGam `seq` (True) ->
                             (case (initPredScope) of
                              { _predScope | _predScope `seq` (True) ->
                              (case (_predScope) of
                               { _exprOpredScope | _exprOpredScope `seq` (True) ->
                               (case (0) of
                                { _predSameScopeCounter | _predSameScopeCounter `seq` (True) ->
                                (case (_predSameScopeCounter) of
                                 { _exprOpredSameScopeCounter | _exprOpredSameScopeCounter `seq` (True) ->
                                 (case (_lhsIpolGam) of
                                  { _polGam | _polGam `seq` (True) ->
                                  (case (_polGam) of
                                   { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                   (case (-1) of
                                    { _lexLev | _lexLev `seq` (True) ->
                                    (case (_lexLev) of
                                     { _exprOlexLev | _exprOlexLev `seq` (True) ->
                                     (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_98_IO_tvar) -> case nextUnique __cont of { (__cont, lUniq_98_fitsIn_ty) -> case nextUnique __cont of { (__cont, lUniq_9_simplify) -> (__cont, lUniq,lUniq_98_IO_tvar,lUniq_98_fitsIn_ty,lUniq_9_simplify)}}}} )) of
                                      { __tup8 | __tup8 `seq` (True) ->
                                      (case (expr_ ) of
                                       { ( _exprIrange,expr_1) | True ->
                                           (case (__tup8) of
                                            { (_exprOgUniq,_,_,_,_) | _exprOgUniq `seq` (True) ->
                                            (case (_lhsIkiGam) of
                                             { _kiGam | _kiGam `seq` (True) ->
                                             (case (expr_1 _exprOgUniq ) of
                                              { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                                  (case (_kiGam) of
                                                   { _exprOkiGam | _exprOkiGam `seq` (True) ->
                                                   (case (True) of
                                                    { _exprOisFirstLet | _exprOisFirstLet `seq` (True) ->
                                                    (case (expr_2 _exprOisFirstLet _exprOkiGam _exprOlexLev _exprOpredSameScopeCounter ) of
                                                     { ( _exprIpredSameScopeCounter,expr_3) | True ->
                                                         (case (_lhsIopts) of
                                                          { _exprOopts | _exprOopts `seq` (True) ->
                                                          (case (emptyVarMp) of
                                                           { _kiVarMp | _kiVarMp `seq` (True) ->
                                                           (case (_kiVarMp) of
                                                            { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                            (case (_lhsIclGam) of
                                                             { _clGam | _clGam `seq` (True) ->
                                                             (case (_clGam) of
                                                              { _exprOclGam | _exprOclGam `seq` (True) ->
                                                              (case (emptyVarMp) of
                                                               { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                               (case (Set.empty) of
                                                                { _exprOtyKiGlobFreeTvarS | _exprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                     (case (emptyGam) of
                                                                      { _exprOgathDataGam | _exprOgathDataGam `seq` (True) ->
                                                                      (case (Set.empty) of
                                                                       { _exprOtyTyTySigFreeTvarS | _exprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                       (case (emptyGam) of
                                                                        { _exprOfinTyKiGam | _exprOfinTyKiGam `seq` (True) ->
                                                                        (case (_exprIkiVarMp) of
                                                                         { _exprOfinKiVarMp | _exprOfinKiVarMp `seq` (True) ->
                                                                         (case (expr_4 _exprOclGam _exprOfinKiVarMp _exprOfinTyKiGam _exprOgathDataGam _exprOtyTyTySigFreeTvarS ) of
                                                                          { ( _exprIgathDataGam,expr_5) | True ->
                                                                              (case (_exprIgathDataGam `gamUnion` _lhsIdataGam) of
                                                                               { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                               (case (expr_5 _exprOdataGam ) of
                                                                                { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                    (case (_exprIgathClDfGam `gamUnion` _lhsIclDfGam) of
                                                                                     { _clDfGam | _clDfGam `seq` (True) ->
                                                                                     (case (_clDfGam) of
                                                                                      { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                      (case (__tup8) of
                                                                                       { (_,_lUniq,_,_,_) | _lUniq `seq` (True) ->
                                                                                       (case (_exprIchrFIIn {fiUniq = _lUniq}) of
                                                                                        { _chrFIIn | _chrFIIn `seq` (True) ->
                                                                                        (case (partition (\(_,_,_,sc) -> sc == initPredScope) $ Seq.toList _exprIchrInstDeclSq) of
                                                                                         { __tup2 | __tup2 `seq` (True) ->
                                                                                         (case (__tup2) of
                                                                                          { (_instsOuter,_) | _instsOuter `seq` (True) ->
                                                                                          (case (_lhsIchrStore) of
                                                                                           { _initChrStore | _initChrStore `seq` (True) ->
                                                                                           (case (mkScopedCHR2 _chrFIIn (Seq.toList _exprIchrClassDeclSq) _instsOuter _initChrStore) of
                                                                                            { __tup3 | __tup3 `seq` (True) ->
                                                                                            (case (__tup3) of
                                                                                             { (_chrStoreOuterScope,_) | _chrStoreOuterScope `seq` (True) ->
                                                                                             (case (__tup2) of
                                                                                              { (_,_instsInner) | _instsInner `seq` (True) ->
                                                                                              (case (mkScopedCHR2 _chrFIIn [] _instsInner _chrStoreOuterScope) of
                                                                                               { __tup4 | __tup4 `seq` (True) ->
                                                                                               (case (__tup4) of
                                                                                                { (_chrStore,_) | _chrStore `seq` (True) ->
                                                                                                (case (_chrStore) of
                                                                                                 { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                 (case (Set.empty) of
                                                                                                  { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                  (case (Set.empty) of
                                                                                                   { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                   (case (_exprIkiVarMp) of
                                                                                                    { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                    (case (strongFIOpts) of
                                                                                                     { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                     (case (Ty_Any) of
                                                                                                      { _exprOknTy | _exprOknTy `seq` (True) ->
                                                                                                      (case (emptyVarMp) of
                                                                                                       { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                       (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                        { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                            (case (Map.unionsWith (++)
                                                                                                                   $ map (\(c,r) -> Map.singleton (_exprItyVarMp `varUpd` c) r)
                                                                                                                   $ Map.toList
                                                                                                                   $ _exprIgathRangeMp
                                                                                                                     `Map.union` cnstrMpToRangeMp _exprIgathCnstrMp) of
                                                                                                             { _rangeMp | _rangeMp `seq` (True) ->
                                                                                                             (case (_rangeMp) of
                                                                                                              { _exprOrangeMp | _exprOrangeMp `seq` (True) ->
                                                                                                              (case (_exprIgathValGam `gamUnion` _lhsIvalGam) of
                                                                                                               { _finValGam | _finValGam `seq` (True) ->
                                                                                                               (case (_finValGam) of
                                                                                                                { _exprOfinValGam | _exprOfinValGam `seq` (True) ->
                                                                                                                (case (__tup8) of
                                                                                                                 { (_,_,_,_,_lUniq_9_simplify) | _lUniq_9_simplify `seq` (True) ->
                                                                                                                 (case (_exprItyVarMp `varUpd` _exprIgathCnstrMp) of
                                                                                                                  { _toProveCnstrMp | _toProveCnstrMp `seq` (True) ->
                                                                                                                  (case (_toProveCnstrMp) of
                                                                                                                   { _toProveHereCnstrMp | _toProveHereCnstrMp `seq` (True) ->
                                                                                                                   (case (_chrFIIn {fiVarMp = _exprItyVarMp}) of
                                                                                                                    { _chrProveFIIn | _chrProveFIIn `seq` (True) ->
                                                                                                                    (case (ehcOptTrace _lhsIopts "AGItf.simplify" $
                                                                                                                           simplify [SimplifyHow_Canonicalize] (_chrProveFIIn {fiUniq = _lUniq_9_simplify}) _chrStore _clDfGam (heurScopedEHC _chrProveFIIn) (\x -> (x,[])) Map.empty _toProveHereCnstrMp emptySimplifyResult) of
                                                                                                                     { __tup5 | __tup5 `seq` (True) ->
                                                                                                                     (case (__tup5) of
                                                                                                                      { (_,_,_,_chrSolveSimpTyVarMp,_,_,_,_,_,_) | _chrSolveSimpTyVarMp `seq` (True) ->
                                                                                                                      (case (_chrSolveSimpTyVarMp `varUpd`
                                                                                                                             _exprItyVarMp) of
                                                                                                                       { _finTyVarMp | _finTyVarMp `seq` (True) ->
                                                                                                                       (case (_finTyVarMp) of
                                                                                                                        { _exprOfinTyVarMp | _exprOfinTyVarMp `seq` (True) ->
                                                                                                                        (case (__tup8) of
                                                                                                                         { (_,_,_,_lUniq_98_fitsIn_ty,_) | _lUniq_98_fitsIn_ty `seq` (True) ->
                                                                                                                         (case (__tup8) of
                                                                                                                          { (_,_,_lUniq_98_IO_tvar,_,_) | _lUniq_98_IO_tvar `seq` (True) ->
                                                                                                                          (case (__tup5) of
                                                                                                                           { (_,_chrSolveMainRemCnstrMp,_,_,_,_,_,_,_,_) | _chrSolveMainRemCnstrMp `seq` (True) ->
                                                                                                                           (case (_chrSolveMainRemCnstrMp) of
                                                                                                                            { _cannotProveCnstrMp | _cannotProveCnstrMp `seq` (True) ->
                                                                                                                            (case (__tup5) of
                                                                                                                             { (_,_,_,_,_chrSolveMainErrs,_,_,_,_,_) | _chrSolveMainErrs `seq` (True) ->
                                                                                                                             (case (mkPrvErr _rangeMp emptyRange _cannotProveCnstrMp) of
                                                                                                                              { _predNotPrfErrs | _predNotPrfErrs `seq` (True) ->
                                                                                                                              (case (_lhsImoduleNm) of
                                                                                                                               { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                               (case (__tup5) of
                                                                                                                                { (_,_,_,_,_,_,_,_,_chrSolveMainScopeBindMp,_) | _chrSolveMainScopeBindMp `seq` (True) ->
                                                                                                                                (case (mkScopeBindings True _predScope _chrSolveMainScopeBindMp) of
                                                                                                                                 { __tup7 | __tup7 `seq` (True) ->
                                                                                                                                 (case (__tup7) of
                                                                                                                                  { (_,_chrScopeBindMp) | _chrScopeBindMp `seq` (True) ->
                                                                                                                                  (case (_chrScopeBindMp) of
                                                                                                                                   { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                   (case (__tup5) of
                                                                                                                                    { (_,_,_,_,_,_,_,_chrSolveMainEvidBindMp,_,_) | _chrSolveMainEvidBindMp `seq` (True) ->
                                                                                                                                    (case (_chrSolveMainEvidBindMp) of
                                                                                                                                     { _chrEvidBindMp | _chrEvidBindMp `seq` (True) ->
                                                                                                                                     (case (_chrEvidBindMp) of
                                                                                                                                      { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                      (case (emptyCSubst) of
                                                                                                                                       { _cSubst | _cSubst `seq` (True) ->
                                                                                                                                       (case (_cSubst) of
                                                                                                                                        { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                        (case (True) of
                                                                                                                                         { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                         (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                          { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                              (case (defaultFIEnv
                                                                                                                                                         { feEHCOpts = _lhsIopts
                                                                                                                                                         , fePredScope = _predScope
                                                                                                                                                         , feTyGam = _exprIgathTyGam
                                                                                                                                                         , fePolGam = _exprIgathPolGam
                                                                                                                                                         , feRange = emptyRange
                                                                                                                                                         }) of
                                                                                                                                               { _fe | _fe `seq` (True) ->
                                                                                                                                               (case (if _lhsIisMainMod
                                                                                                                                                      then let (t,e) = valGamLookupTy hsnMain _exprIgathValGam
                                                                                                                                                           in  (fitsIn strongFIOpts _fe _lUniq_98_fitsIn_ty _exprItyVarMp t (tyTopLevelMain _lhsIopts _lUniq_98_IO_tvar), e)
                                                                                                                                                      else (emptyFO,[])) of
                                                                                                                                                { __tup1 | __tup1 `seq` (True) ->
                                                                                                                                                (case (__tup1) of
                                                                                                                                                 { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                 (case (__tup1) of
                                                                                                                                                  { (_foMain,_) | _foMain `seq` (True) ->
                                                                                                                                                  (case (Seq.unions [ Seq.fromList _predNotPrfErrs
                                                                                                                                                                    , Seq.fromList _chrSolveMainErrs
                                                                                                                                                                    , Seq.fromList _nmErrs
                                                                                                                                                                    , foErrSq _foMain
                                                                                                                                                                    ]) of
                                                                                                                                                   { _lErrSq | _lErrSq `seq` (True) ->
                                                                                                                                                   (case (Seq.unions [_lErrSq, _exprIallErrSq]) of
                                                                                                                                                    { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                    (case (__tup7) of
                                                                                                                                                     { (_chrScopeMainBindL,_) | _chrScopeMainBindL `seq` (True) ->
                                                                                                                                                     (case ([(CBindCateg_Rec,_chrScopeMainBindL)]) of
                                                                                                                                                      { _extraCBindL | _extraCBindL `seq` (True) ->
                                                                                                                                                      (case (let ds = [ b | (_,g) <- _exprIletCBindL, b <- g, cbindNm b == hsnMain ]
                                                                                                                                                             in  not (null ds)) of
                                                                                                                                                       { _mainDefExists | _mainDefExists `seq` (True) ->
                                                                                                                                                       (case (not _mainDefExists && _lhsIisMainMod) of
                                                                                                                                                        { _insertMainDef | _insertMainDef `seq` (True) ->
                                                                                                                                                        (case (case acoreExprMbVar _exprIletCBody of
                                                                                                                                                                 Just n | n == hsnMain -> True
                                                                                                                                                                 _                     -> False) of
                                                                                                                                                         { _mainUseExists | _mainUseExists `seq` (True) ->
                                                                                                                                                         (case (not _mainUseExists && _lhsIisMainMod) of
                                                                                                                                                          { _insertMainUse | _insertMainUse `seq` (True) ->
                                                                                                                                                          (case (let b = if _insertMainUse then acoreVar hsnMain else _exprIletCBody
                                                                                                                                                                     d = if _insertMainDef then acoreLet CBindCateg_Plain [acoreBind1Ty hsnMain (acoreTyErr $ "EH.ToCore.AGItf.main") _exprIletCBody] else id
                                                                                                                                                                 in  CModule_Mod _lhsImoduleNm
                                                                                                                                                                         (
                                                                                                                                                                            cSubstAppExpr True _exprIcSubst
                                                                                                                                                                            (foldr (\(c,b) e -> acoreLet c b e)
                                                                                                                                                                              (d b)
                                                                                                                                                                              (_extraCBindL ++ _exprIfrontCBindL ++ _exprIletCBindL
                                                                                                                                                                              ++ _exprIbackCBindL
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                            Nothing
                                                                                                                                                                         )
                                                                                                                                                                         [ (tn,Map.toList m) | (tn,dgi) <- gamToAssocL _exprIgathDataGam, not (dgiIsNewtype dgi), let m = Map.map dtiCTag $ dgiConstrTagMp dgi ]) of
                                                                                                                                                           { _lhsOcmodule | _lhsOcmodule `seq` (True) ->
                                                                                                                                                           (case (_exprIgUniq) of
                                                                                                                                                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                                                                                                                            (case (__tup3) of
                                                                                                                                                             { (_,_gathChrStore) | _gathChrStore `seq` (True) ->
                                                                                                                                                             (case (_gathChrStore) of
                                                                                                                                                              { _lhsOgathChrStore | _lhsOgathChrStore `seq` (True) ->
                                                                                                                                                              (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathClDfGam" _exprIgathClDfGam) of
                                                                                                                                                               { _lhsOgathClDfGam | _lhsOgathClDfGam `seq` (True) ->
                                                                                                                                                               (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathClGam" _exprIgathClGam) of
                                                                                                                                                                { _lhsOgathClGam | _lhsOgathClGam `seq` (True) ->
                                                                                                                                                                (case (_exprIgathDataGam) of
                                                                                                                                                                 { _lhsOgathDataGam | _lhsOgathDataGam `seq` (True) ->
                                                                                                                                                                 (case (_exprIgathHiddenExports) of
                                                                                                                                                                  { _lhsOgathHiddenExports | _lhsOgathHiddenExports `seq` (True) ->
                                                                                                                                                                  (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathKiGam" _exprIgathKiGam) of
                                                                                                                                                                   { _lhsOgathKiGam | _lhsOgathKiGam `seq` (True) ->
                                                                                                                                                                   (case (_exprIgathLamMp) of
                                                                                                                                                                    { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                                                                                                                                                                    (case (__tup5) of
                                                                                                                                                                     { (_,_,_,_,_,_chrSimplifyResult,_,_,_,_) | _chrSimplifyResult `seq` (True) ->
                                                                                                                                                                     (case (mentrelFilterMpUnions
                                                                                                                                                                              [ _exprIgathMentrelFilterMp
                                                                                                                                                                              , gathMentrelFilterMpFromSimplifyResult _lhsImoduleNm _chrSimplifyResult
                                                                                                                                                                              ]) of
                                                                                                                                                                      { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                      (case (_gathMentrelFilterMp) of
                                                                                                                                                                       { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                       (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathPolGam" _exprIgathPolGam) of
                                                                                                                                                                        { _lhsOgathPolGam | _lhsOgathPolGam `seq` (True) ->
                                                                                                                                                                        (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathTyGam" _exprIgathTyGam) of
                                                                                                                                                                         { _lhsOgathTyGam | _lhsOgathTyGam `seq` (True) ->
                                                                                                                                                                         (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathTyKiGam" _exprIgathTyKiGam) of
                                                                                                                                                                          { _lhsOgathTyKiGam | _lhsOgathTyKiGam `seq` (True) ->
                                                                                                                                                                          (case (ehcOptTrace _lhsIopts "AGItf.lhs.gathValGam" $ _finTyVarMp `varUpd` _exprIgathValGam) of
                                                                                                                                                                           { _lhsOgathValGam | _lhsOgathValGam `seq` (True) ->
                                                                                                                                                                           (case (if Set.null _exprIorphanS then Nothing else Just _exprIorphanS) of
                                                                                                                                                                            { _lhsOmbOrphan | _lhsOmbOrphan `seq` (True) ->
                                                                                                                                                                            (case (empty) of
                                                                                                                                                                             { _extraPP | _extraPP `seq` (True) ->
                                                                                                                                                                             (case (ppErrsSq _lErrSq) of
                                                                                                                                                                              { _errTopPP | _errTopPP `seq` (True) ->
                                                                                                                                                                              (case (_extraPP >-< _exprIpp
                                                                                                                                                                                     >-< _errTopPP) of
                                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                               (case (ppTy (tyQuantifyClosed _exprIty)) of
                                                                                                                                                                                { _lhsOtopTyPP | _lhsOtopTyPP `seq` (True) ->
                                                                                                                                                                                ( _lhsOallErrSq,_lhsOcmodule,_lhsOgUniq,_lhsOgathChrStore,_lhsOgathClDfGam,_lhsOgathClGam,_lhsOgathDataGam,_lhsOgathHiddenExports,_lhsOgathKiGam,_lhsOgathLamMp,_lhsOgathMentrelFilterMp,_lhsOgathPolGam,_lhsOgathTyGam,_lhsOgathTyKiGam,_lhsOgathValGam,_lhsOmbOrphan,_lhsOpp,_lhsOtopTyPP) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))))


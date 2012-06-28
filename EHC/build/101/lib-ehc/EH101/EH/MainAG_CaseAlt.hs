


module EH101.EH.MainAG_CaseAlt where

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

-- CaseAlt -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         kiGam                : KiGam
         lexLev               : Int
      chained attribute:
         predSameScopeCounter : Int
   visit 3:
      inherited attributes:
         opts                 : EHCOpts
         polGam               : PolGam
         predScope            : PredScope
         tyGam                : TyGam
         tyKiGam              : TyKiGam
         tyKiGlobFreeTvarS    : TyVarIdS
      chained attributes:
         kiVarMp              : VarMp
         polVarMp             : VarMp
   visit 4:
      inherited attributes:
         clGam                : ClGam
         dataGam              : DataGam
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         tyTyTySigFreeTvarS   : TyVarIdS
      synthesized attribute:
         chrInstDeclSq        : Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)
   visit 5:
      inherited attributes:
         knPatTy              : Ty
         valGam               : ValGam
      chained attribute:
         patTyVarMp           : VarMp
   visit 6:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         fiOpts               : FIOpts
         knTy                 : Ty
         tvKiVarMp            : VarMp
         tyTyGlobFreeTvarS    : TyVarIdS
         valTyGlobFreeTvarS   : TyVarIdS
      chained attribute:
         tyVarMp              : VarMp
      synthesized attributes:
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
   visit 7:
      inherited attributes:
         chrEvidBindMp        : EvidKeyToCBindMap
         chrScopeBindMp       : PredScopeToCBindMap
         finTyVarMp           : VarMp
         finValGam            : ValGam
         moduleNm             : HsName
         rangeMp              : RangeMp
      chained attribute:
         cSubst               : CSubst
      synthesized attributes:
         allErrSq             : ErrSq
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         patTy                : Ty
         pp                   : PP_Doc
         ralt                 : RAlt
         ralt'                : RAlt
         ty                   : Ty
   alternatives:
      alternative Pat:
         child hsrange        : {Range}
         child patExpr        : PatExpr 
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 2:
            local lexLev      : {Int}
         visit 5:
            local tySigGam    : {ValGam}
         visit 7:
            local extraPP     : _
            local pp          : _
-}
sem_CaseAlt_Pat :: Range ->
                   T_PatExpr  ->
                   T_Expr  ->
                   T_CaseAlt 

sem_CaseAlt_Pat hsrange_ patExpr_ expr_  | hsrange_ `seq` (patExpr_ `seq` (expr_ `seq` (True))) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (patExpr_ ) of
          { ( _patExprIrange,patExpr_1) | True ->
              (case (rangeUnions [hsrange_, _patExprIrange, _exprIrange  ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_CaseAlt_Pat_1 :: T_CaseAlt_1 
                            sem_CaseAlt_Pat_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (_lhsIgUniq) of
                                       { _patExprOgUniq | _patExprOgUniq `seq` (True) ->
                                       (case (patExpr_1 _patExprOgUniq ) of
                                        { ( _patExprIgUniq,patExpr_2) | True ->
                                            (case (_patExprIgUniq) of
                                             { _exprOgUniq | _exprOgUniq `seq` (True) ->
                                             (case (expr_1 _exprOgUniq ) of
                                              { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                                  (case (_exprIgUniq) of
                                                   { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                   (case ((let sem_CaseAlt_Pat_2 :: T_CaseAlt_2 
                                                               sem_CaseAlt_Pat_2  =
                                                                   (\ _lhsIkiGam
                                                                      _lhsIlexLev
                                                                      _lhsIpredSameScopeCounter ->
                                                                        _lhsIkiGam `seq`
                                                                        (_lhsIlexLev `seq`
                                                                         (_lhsIpredSameScopeCounter `seq`
                                                                          ((case (_lhsIpredSameScopeCounter) of
                                                                            { _patExprOpredSameScopeCounter | _patExprOpredSameScopeCounter `seq` (True) ->
                                                                            (case (_lhsIlexLev + 1) of
                                                                             { _lexLev | _lexLev `seq` (True) ->
                                                                             (case (_lexLev) of
                                                                              { _patExprOlexLev | _patExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _patExprOkiGam | _patExprOkiGam `seq` (True) ->
                                                                               (case (patExpr_2 _patExprOkiGam _patExprOlexLev _patExprOpredSameScopeCounter ) of
                                                                                { ( _patExprIpredSameScopeCounter,patExpr_3) | True ->
                                                                                    (case (_patExprIpredSameScopeCounter) of
                                                                                     { _exprOpredSameScopeCounter | _exprOpredSameScopeCounter `seq` (True) ->
                                                                                     (case (_lexLev) of
                                                                                      { _exprOlexLev | _exprOlexLev `seq` (True) ->
                                                                                      (case (_lhsIkiGam) of
                                                                                       { _exprOkiGam | _exprOkiGam `seq` (True) ->
                                                                                       (case (True) of
                                                                                        { _exprOisFirstLet | _exprOisFirstLet `seq` (True) ->
                                                                                        (case (expr_2 _exprOisFirstLet _exprOkiGam _exprOlexLev _exprOpredSameScopeCounter ) of
                                                                                         { ( _exprIpredSameScopeCounter,expr_3) | True ->
                                                                                             (case (_exprIpredSameScopeCounter) of
                                                                                              { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                                              (case ((let sem_CaseAlt_Pat_3 :: T_CaseAlt_3 
                                                                                                          sem_CaseAlt_Pat_3  =
                                                                                                              (\ _lhsIkiVarMp
                                                                                                                 _lhsIopts
                                                                                                                 _lhsIpolGam
                                                                                                                 _lhsIpolVarMp
                                                                                                                 _lhsIpredScope
                                                                                                                 _lhsItyGam
                                                                                                                 _lhsItyKiGam
                                                                                                                 _lhsItyKiGlobFreeTvarS ->
                                                                                                                   _lhsIkiVarMp `seq`
                                                                                                                   (_lhsIopts `seq`
                                                                                                                    (_lhsIpolGam `seq`
                                                                                                                     (_lhsIpolVarMp `seq`
                                                                                                                      (_lhsIpredScope `seq`
                                                                                                                       (_lhsItyGam `seq`
                                                                                                                        (_lhsItyKiGam `seq`
                                                                                                                         (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                          ((case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                            { _patExprOtyKiGlobFreeTvarS | _patExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                            (case (_lhsItyKiGam) of
                                                                                                                             { _patExprOtyKiGam | _patExprOtyKiGam `seq` (True) ->
                                                                                                                             (case (_lhsIpredScope) of
                                                                                                                              { _patExprOpredScope | _patExprOpredScope `seq` (True) ->
                                                                                                                              (case (_lhsIpolVarMp) of
                                                                                                                               { _patExprOpolVarMp | _patExprOpolVarMp `seq` (True) ->
                                                                                                                               (case (_lhsIpolGam) of
                                                                                                                                { _patExprOpolGam | _patExprOpolGam `seq` (True) ->
                                                                                                                                (case (_lhsIopts) of
                                                                                                                                 { _patExprOopts | _patExprOopts `seq` (True) ->
                                                                                                                                 (case (_lhsIkiVarMp) of
                                                                                                                                  { _patExprOkiVarMp | _patExprOkiVarMp `seq` (True) ->
                                                                                                                                  (case (gamPushNew _lhsItyGam) of
                                                                                                                                   { _patExprOtyGam | _patExprOtyGam `seq` (True) ->
                                                                                                                                   (case (patExpr_3 _patExprOkiVarMp _patExprOopts _patExprOpolGam _patExprOpolVarMp _patExprOpredScope _patExprOtyGam _patExprOtyKiGam _patExprOtyKiGlobFreeTvarS ) of
                                                                                                                                    { ( _patExprIkiVarMp,_patExprIpolVarMp,_patExprItyGam,_patExprItyKiGam,patExpr_4) | True ->
                                                                                                                                        (case (_patExprItyKiGam) of
                                                                                                                                         { _exprOtyKiGam | _exprOtyKiGam `seq` (True) ->
                                                                                                                                         (case (_patExprItyGam) of
                                                                                                                                          { _exprOtyGam | _exprOtyGam `seq` (True) ->
                                                                                                                                          (case (_lhsIpredScope) of
                                                                                                                                           { _exprOpredScope | _exprOpredScope `seq` (True) ->
                                                                                                                                           (case (_patExprIpolVarMp) of
                                                                                                                                            { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                                                                                                            (case (_lhsIpolGam) of
                                                                                                                                             { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                                                                                                                             (case (_lhsIopts) of
                                                                                                                                              { _exprOopts | _exprOopts `seq` (True) ->
                                                                                                                                              (case (_patExprIkiVarMp) of
                                                                                                                                               { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                                                                                                               (case (varFreeSet (_patExprIkiVarMp `varUpd` gamTop _patExprItyKiGam) `Set.union` _lhsItyKiGlobFreeTvarS) of
                                                                                                                                                { _exprOtyKiGlobFreeTvarS | _exprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                                                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                                                                                                     (case (_exprIkiVarMp) of
                                                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                      (case (_exprIpolVarMp) of
                                                                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                       (case ((let sem_CaseAlt_Pat_4 :: T_CaseAlt_4 
                                                                                                                                                                   sem_CaseAlt_Pat_4  =
                                                                                                                                                                       (\ _lhsIclGam
                                                                                                                                                                          _lhsIdataGam
                                                                                                                                                                          _lhsIfinKiVarMp
                                                                                                                                                                          _lhsIfinTyKiGam
                                                                                                                                                                          _lhsItyTyTySigFreeTvarS ->
                                                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                                                            (_lhsIdataGam `seq`
                                                                                                                                                                             (_lhsIfinKiVarMp `seq`
                                                                                                                                                                              (_lhsIfinTyKiGam `seq`
                                                                                                                                                                               (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                ((case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                  { _exprOtyTyTySigFreeTvarS | _exprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                  (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                   { _exprOfinTyKiGam | _exprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                   (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                    { _exprOfinKiVarMp | _exprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIdataGam) of
                                                                                                                                                                                     { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIclGam) of
                                                                                                                                                                                      { _exprOclGam | _exprOclGam `seq` (True) ->
                                                                                                                                                                                      (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                       { _patExprOtyTyTySigFreeTvarS | _patExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                       (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                        { _patExprOfinTyKiGam | _patExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                        (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                         { _patExprOfinKiVarMp | _patExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                         (case (_lhsIdataGam) of
                                                                                                                                                                                          { _patExprOdataGam | _patExprOdataGam `seq` (True) ->
                                                                                                                                                                                          (case (_lhsIclGam) of
                                                                                                                                                                                           { _patExprOclGam | _patExprOclGam `seq` (True) ->
                                                                                                                                                                                           (case (emptyGam) of
                                                                                                                                                                                            { _exprOgathDataGam | _exprOgathDataGam `seq` (True) ->
                                                                                                                                                                                            (case (expr_4 _exprOclGam _exprOfinKiVarMp _exprOfinTyKiGam _exprOgathDataGam _exprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                             { ( _exprIgathDataGam,expr_5) | True ->
                                                                                                                                                                                                 (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                                  { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                                      (case (patExpr_4 _patExprOclGam _patExprOdataGam _patExprOfinKiVarMp _patExprOfinTyKiGam _patExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                                       { ( _patExprIchrInstDeclSq,_patExprItopNm,patExpr_5) | True ->
                                                                                                                                                                                                           (case (_patExprIchrInstDeclSq `Seq.union` _exprIchrInstDeclSq) of
                                                                                                                                                                                                            { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                            (case ((let sem_CaseAlt_Pat_5 :: T_CaseAlt_5 
                                                                                                                                                                                                                        sem_CaseAlt_Pat_5  =
                                                                                                                                                                                                                            (\ _lhsIknPatTy
                                                                                                                                                                                                                               _lhsIpatTyVarMp
                                                                                                                                                                                                                               _lhsIvalGam ->
                                                                                                                                                                                                                                 _lhsIknPatTy `seq`
                                                                                                                                                                                                                                 (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                                  (_lhsIvalGam `seq`
                                                                                                                                                                                                                                   ((case (emptyGam) of
                                                                                                                                                                                                                                     { _tySigGam | _tySigGam `seq` (True) ->
                                                                                                                                                                                                                                     (case (_tySigGam) of
                                                                                                                                                                                                                                      { _patExprOtySigGam | _patExprOtySigGam `seq` (True) ->
                                                                                                                                                                                                                                      (case (gamPushNew _lhsIvalGam) of
                                                                                                                                                                                                                                       { _patExprOvalGam | _patExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                       (case (_lhsIknPatTy) of
                                                                                                                                                                                                                                        { _patExprOknTy | _patExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                        (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                                         { _patExprOpatTyVarMp | _patExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                         (case (strongFIOpts) of
                                                                                                                                                                                                                                          { _patExprOfiOpts | _patExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                          (case ([]) of
                                                                                                                                                                                                                                           { _patExprOknTyL | _patExprOknTyL `seq` (True) ->
                                                                                                                                                                                                                                           (case (True) of
                                                                                                                                                                                                                                            { _patExprOinclVarBind | _patExprOinclVarBind `seq` (True) ->
                                                                                                                                                                                                                                            (case (patExpr_5 _patExprOvalGam ) of
                                                                                                                                                                                                                                             { ( _patExprIarity,_patExprImbTopNm,_patExprIpatFunTy,patExpr_6) | True ->
                                                                                                                                                                                                                                                 (case (patExpr_6 _patExprOfiOpts _patExprOinclVarBind _patExprOknTy _patExprOknTyL _patExprOpatTyVarMp _patExprOtySigGam ) of
                                                                                                                                                                                                                                                  { ( _patExprIpatTyVarMp,_patExprIvalGam,patExpr_7) | True ->
                                                                                                                                                                                                                                                      (case (_patExprIpatTyVarMp) of
                                                                                                                                                                                                                                                       { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                       (case ((let sem_CaseAlt_Pat_6 :: T_CaseAlt_6 
                                                                                                                                                                                                                                                                   sem_CaseAlt_Pat_6  =
                                                                                                                                                                                                                                                                       (\ _lhsIchrStore
                                                                                                                                                                                                                                                                          _lhsIclDfGam
                                                                                                                                                                                                                                                                          _lhsIfiOpts
                                                                                                                                                                                                                                                                          _lhsIknTy
                                                                                                                                                                                                                                                                          _lhsItvKiVarMp
                                                                                                                                                                                                                                                                          _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                          _lhsItyVarMp
                                                                                                                                                                                                                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                            _lhsIchrStore `seq`
                                                                                                                                                                                                                                                                            (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                             (_lhsIfiOpts `seq`
                                                                                                                                                                                                                                                                              (_lhsIknTy `seq`
                                                                                                                                                                                                                                                                               (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                                (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                 (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                   ((case (_patExprIvalGam) of
                                                                                                                                                                                                                                                                                     { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                      { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_lhsIknTy) of
                                                                                                                                                                                                                                                                                       { _exprOknTy | _exprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                        { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                         { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                          { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                           { _patExprOvalTyGlobFreeTvarS | _patExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                            { _patExprOtyTyGlobFreeTvarS | _patExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                             { _patExprOtvKiVarMp | _patExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                              { _patExprOclDfGam | _patExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                               { _patExprOchrStore | _patExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (_lhsItyVarMp) of
                                                                                                                                                                                                                                                                                                { _patExprOtyVarMp | _patExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (patExpr_7 _patExprOchrStore _patExprOclDfGam _patExprOtvKiVarMp _patExprOtyTyGlobFreeTvarS _patExprOtyVarMp _patExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                 { ( _patExprIcpNm,_patExprIgathCnstrMp,_patExprIgathRangeMp,_patExprIscopeGam,_patExprIty,_patExprItyVarMp,patExpr_8) | True ->
                                                                                                                                                                                                                                                                                                     (case (_patExprItyVarMp) of
                                                                                                                                                                                                                                                                                                      { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (varFreeSet (gamTop _patExprItyGam) `Set.union` _lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                       { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (varFreeSet (gamTop _patExprIvalGam) `Set.union` _lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                        { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                         { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                                                                                             (case (_patExprIgathCnstrMp `cnstrMpUnion` _exprIgathCnstrMp) of
                                                                                                                                                                                                                                                                                                              { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (_patExprIgathRangeMp `Map.union` _exprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                               { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (_exprItyVarMp) of
                                                                                                                                                                                                                                                                                                                { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case ((let sem_CaseAlt_Pat_7 :: T_CaseAlt_7 
                                                                                                                                                                                                                                                                                                                            sem_CaseAlt_Pat_7  =
                                                                                                                                                                                                                                                                                                                                (\ _lhsIcSubst
                                                                                                                                                                                                                                                                                                                                   _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                                                                                                   _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                                                                                                   _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                                                                   _lhsIfinValGam
                                                                                                                                                                                                                                                                                                                                   _lhsImoduleNm
                                                                                                                                                                                                                                                                                                                                   _lhsIrangeMp ->
                                                                                                                                                                                                                                                                                                                                     _lhsIcSubst `seq`
                                                                                                                                                                                                                                                                                                                                     (_lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                                                                                                      (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                                                                                                       (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                                                                                        (_lhsIfinValGam `seq`
                                                                                                                                                                                                                                                                                                                                         (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                                                                                          (_lhsIrangeMp `seq`
                                                                                                                                                                                                                                                                                                                                           ((case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                                                             { _exprOrangeMp | _exprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                             (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                              { _exprOfinValGam | _exprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                              (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                               { _exprOfinTyVarMp | _exprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                               (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                                                                { _patExprOrangeMp | _patExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                 { _patExprOfinValGam | _patExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                  { _patExprOfinTyVarMp | _patExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                  (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                   { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                   (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                    { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                    (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                     { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                     (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                      { _patExprOchrScopeBindMp | _patExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                      (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                       { _patExprOchrEvidBindMp | _patExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                       (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                        { _patExprOcSubst | _patExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                        (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                         { _patExprOmoduleNm | _patExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                         (case (_patExprItopNm) of
                                                                                                                                                                                                                                                                                                                                                          { _patExprOceParentNm | _patExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                          (case (patExpr_8 _patExprOcSubst _patExprOceParentNm _patExprOchrEvidBindMp _patExprOchrScopeBindMp _patExprOfinTyVarMp _patExprOfinValGam _patExprOmoduleNm _patExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                           { ( _patExprIallErrSq,_patExprIappArgPPL,_patExprIappFunNm,_patExprIappFunPP,_patExprIcSubst,_patExprIcbindL,_patExprIerrSq,_patExprIfsRPatL,_patExprIgathMentrelFilterMp,_patExprIgathTvKiVarMp,_patExprIisBang,_patExprIpatCRest,_patExprIpp,_patExprIrpat) | True ->
                                                                                                                                                                                                                                                                                                                                                               (case (_patExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (True) of
                                                                                                                                                                                                                                                                                                                                                                 { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                  { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                                                                                                                                                                                                                                                      (case (_patExprIallErrSq `Seq.union` _exprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                       { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                       (case (_exprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                        { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                        (case (_patExprIerrSq `Seq.union` _exprIerrSq) of
                                                                                                                                                                                                                                                                                                                                                                         { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                         (case (_patExprIgathMentrelFilterMp `mentrelFilterMpUnion` _exprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                          { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                          (case (_patExprIgathTvKiVarMp `varmpUnion` _exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                           { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                           (case (_patExprIty) of
                                                                                                                                                                                                                                                                                                                                                                            { _lhsOpatTy | _lhsOpatTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                            (case (empty) of
                                                                                                                                                                                                                                                                                                                                                                             { _extraPP | _extraPP `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                             (case (_patExprIpp >|< _extraPP >###< "->" >#< _exprIpp) of
                                                                                                                                                                                                                                                                                                                                                                              { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                              (case (_pp) of
                                                                                                                                                                                                                                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                               (case (RAlt_Alt [_patExprIrpat] _exprIcexpr _exprIcaseFailS) of
                                                                                                                                                                                                                                                                                                                                                                                { _lhsOralt | _lhsOralt `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                (case (RAlt_Alt (fsL2PatL $ reverse _patExprIfsRPatL) _exprIcexpr _exprIcaseFailS) of
                                                                                                                                                                                                                                                                                                                                                                                 { _lhsOralt' | _lhsOralt' `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                 (case (_exprIty) of
                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                  ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTy,_lhsOpp,_lhsOralt,_lhsOralt',_lhsOty) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                                                                                        in  sem_CaseAlt_Pat_7)) of
                                                                                                                                                                                                                                                                                                                 { ( sem_CaseAlt_7) | True ->
                                                                                                                                                                                                                                                                                                                 ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOtyVarMp,sem_CaseAlt_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                               in  sem_CaseAlt_Pat_6)) of
                                                                                                                                                                                                                                                        { ( sem_CaseAlt_6) | True ->
                                                                                                                                                                                                                                                        ( _lhsOpatTyVarMp,sem_CaseAlt_6) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                                                                                                                                    in  sem_CaseAlt_Pat_5)) of
                                                                                                                                                                                                             { ( sem_CaseAlt_5) | True ->
                                                                                                                                                                                                             ( _lhsOchrInstDeclSq,sem_CaseAlt_5) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                               in  sem_CaseAlt_Pat_4)) of
                                                                                                                                                        { ( sem_CaseAlt_4) | True ->
                                                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,sem_CaseAlt_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                      in  sem_CaseAlt_Pat_3)) of
                                                                                               { ( sem_CaseAlt_3) | True ->
                                                                                               ( _lhsOpredSameScopeCounter,sem_CaseAlt_3) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                           in  sem_CaseAlt_Pat_2)) of
                                                    { ( sem_CaseAlt_2) | True ->
                                                    ( _lhsOgUniq,sem_CaseAlt_2) }) }) }) }) }) })))
                        in  sem_CaseAlt_Pat_1)) of
                 { ( sem_CaseAlt_1) | True ->
                 ( _lhsOrange,sem_CaseAlt_1) }) }) }) }) })


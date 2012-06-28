


module EH101.EH.MainAG_CaseAlts where

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

-- CaseAlts ----------------------------------------------------
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
         knTy                 : Ty
         valGam               : ValGam
      chained attribute:
         patTyVarMp           : VarMp
      synthesized attribute:
         ty                   : Ty
   visit 6:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         fiOpts               : FIOpts
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
         ppL                  : [PP_Doc]
         raltL                : [RAlt]
         raltL'               : [RAlt]
   alternatives:
      alternative Cons:
         child hd             : CaseAlt 
         child tl             : CaseAlts 
         visit 7:
            local extraPP     : _
      alternative Nil:
-}
sem_CaseAlts_Cons :: T_CaseAlt  ->
                     T_CaseAlts  ->
                     T_CaseAlts 

sem_CaseAlts_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (case (tl_ ) of
     { ( _tlIrange,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdIrange,hd_1) | True ->
              (case (_hdIrange `rangeUnion` _tlIrange) of
               { _lhsOrange | _lhsOrange `seq` (True) ->
               (case ((let sem_CaseAlts_Cons_1 :: T_CaseAlts_1 
                           sem_CaseAlts_Cons_1  =
                               (\ _lhsIgUniq ->
                                    _lhsIgUniq `seq`
                                    ((case (_lhsIgUniq) of
                                      { _hdOgUniq | _hdOgUniq `seq` (True) ->
                                      (case (hd_1 _hdOgUniq ) of
                                       { ( _hdIgUniq,hd_2) | True ->
                                           (case (_hdIgUniq) of
                                            { _tlOgUniq | _tlOgUniq `seq` (True) ->
                                            (case (tl_1 _tlOgUniq ) of
                                             { ( _tlIgUniq,tl_2) | True ->
                                                 (case (_tlIgUniq) of
                                                  { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                  (case ((let sem_CaseAlts_Cons_2 :: T_CaseAlts_2 
                                                              sem_CaseAlts_Cons_2  =
                                                                  (\ _lhsIkiGam
                                                                     _lhsIlexLev
                                                                     _lhsIpredSameScopeCounter ->
                                                                       _lhsIkiGam `seq`
                                                                       (_lhsIlexLev `seq`
                                                                        (_lhsIpredSameScopeCounter `seq`
                                                                         ((case (_lhsIpredSameScopeCounter) of
                                                                           { _hdOpredSameScopeCounter | _hdOpredSameScopeCounter `seq` (True) ->
                                                                           (case (_lhsIlexLev) of
                                                                            { _hdOlexLev | _hdOlexLev `seq` (True) ->
                                                                            (case (_lhsIkiGam) of
                                                                             { _hdOkiGam | _hdOkiGam `seq` (True) ->
                                                                             (case (hd_2 _hdOkiGam _hdOlexLev _hdOpredSameScopeCounter ) of
                                                                              { ( _hdIpredSameScopeCounter,hd_3) | True ->
                                                                                  (case (_hdIpredSameScopeCounter) of
                                                                                   { _tlOpredSameScopeCounter | _tlOpredSameScopeCounter `seq` (True) ->
                                                                                   (case (_lhsIlexLev) of
                                                                                    { _tlOlexLev | _tlOlexLev `seq` (True) ->
                                                                                    (case (_lhsIkiGam) of
                                                                                     { _tlOkiGam | _tlOkiGam `seq` (True) ->
                                                                                     (case (tl_2 _tlOkiGam _tlOlexLev _tlOpredSameScopeCounter ) of
                                                                                      { ( _tlIpredSameScopeCounter,tl_3) | True ->
                                                                                          (case (_tlIpredSameScopeCounter) of
                                                                                           { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                                           (case ((let sem_CaseAlts_Cons_3 :: T_CaseAlts_3 
                                                                                                       sem_CaseAlts_Cons_3  =
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
                                                                                                                         { _tlOtyKiGlobFreeTvarS | _tlOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                         (case (_lhsItyKiGam) of
                                                                                                                          { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                                                          (case (_lhsItyGam) of
                                                                                                                           { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                                                                           (case (_lhsIpredScope) of
                                                                                                                            { _tlOpredScope | _tlOpredScope `seq` (True) ->
                                                                                                                            (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                             { _hdOtyKiGlobFreeTvarS | _hdOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                             (case (_lhsItyKiGam) of
                                                                                                                              { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                                                              (case (_lhsItyGam) of
                                                                                                                               { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                                                                                                               (case (_lhsIpredScope) of
                                                                                                                                { _hdOpredScope | _hdOpredScope `seq` (True) ->
                                                                                                                                (case (_lhsIpolVarMp) of
                                                                                                                                 { _hdOpolVarMp | _hdOpolVarMp `seq` (True) ->
                                                                                                                                 (case (_lhsIpolGam) of
                                                                                                                                  { _hdOpolGam | _hdOpolGam `seq` (True) ->
                                                                                                                                  (case (_lhsIopts) of
                                                                                                                                   { _hdOopts | _hdOopts `seq` (True) ->
                                                                                                                                   (case (_lhsIkiVarMp) of
                                                                                                                                    { _hdOkiVarMp | _hdOkiVarMp `seq` (True) ->
                                                                                                                                    (case (hd_3 _hdOkiVarMp _hdOopts _hdOpolGam _hdOpolVarMp _hdOpredScope _hdOtyGam _hdOtyKiGam _hdOtyKiGlobFreeTvarS ) of
                                                                                                                                     { ( _hdIkiVarMp,_hdIpolVarMp,hd_4) | True ->
                                                                                                                                         (case (_hdIpolVarMp) of
                                                                                                                                          { _tlOpolVarMp | _tlOpolVarMp `seq` (True) ->
                                                                                                                                          (case (_lhsIpolGam) of
                                                                                                                                           { _tlOpolGam | _tlOpolGam `seq` (True) ->
                                                                                                                                           (case (_lhsIopts) of
                                                                                                                                            { _tlOopts | _tlOopts `seq` (True) ->
                                                                                                                                            (case (_hdIkiVarMp) of
                                                                                                                                             { _tlOkiVarMp | _tlOkiVarMp `seq` (True) ->
                                                                                                                                             (case (tl_3 _tlOkiVarMp _tlOopts _tlOpolGam _tlOpolVarMp _tlOpredScope _tlOtyGam _tlOtyKiGam _tlOtyKiGlobFreeTvarS ) of
                                                                                                                                              { ( _tlIkiVarMp,_tlIpolVarMp,tl_4) | True ->
                                                                                                                                                  (case (_tlIkiVarMp) of
                                                                                                                                                   { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                   (case (_tlIpolVarMp) of
                                                                                                                                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                    (case ((let sem_CaseAlts_Cons_4 :: T_CaseAlts_4 
                                                                                                                                                                sem_CaseAlts_Cons_4  =
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
                                                                                                                                                                               { _tlOtyTyTySigFreeTvarS | _tlOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                               (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                 { _tlOfinKiVarMp | _tlOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                 (case (_lhsIdataGam) of
                                                                                                                                                                                  { _tlOdataGam | _tlOdataGam `seq` (True) ->
                                                                                                                                                                                  (case (_lhsIclGam) of
                                                                                                                                                                                   { _tlOclGam | _tlOclGam `seq` (True) ->
                                                                                                                                                                                   (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                    { _hdOtyTyTySigFreeTvarS | _hdOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                     { _hdOfinTyKiGam | _hdOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                      { _hdOfinKiVarMp | _hdOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                      (case (_lhsIdataGam) of
                                                                                                                                                                                       { _hdOdataGam | _hdOdataGam `seq` (True) ->
                                                                                                                                                                                       (case (_lhsIclGam) of
                                                                                                                                                                                        { _hdOclGam | _hdOclGam `seq` (True) ->
                                                                                                                                                                                        (case (tl_4 _tlOclGam _tlOdataGam _tlOfinKiVarMp _tlOfinTyKiGam _tlOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                         { ( _tlIchrInstDeclSq,tl_5) | True ->
                                                                                                                                                                                             (case (hd_4 _hdOclGam _hdOdataGam _hdOfinKiVarMp _hdOfinTyKiGam _hdOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                              { ( _hdIchrInstDeclSq,hd_5) | True ->
                                                                                                                                                                                                  (case (_hdIchrInstDeclSq `Seq.union` _tlIchrInstDeclSq) of
                                                                                                                                                                                                   { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                   (case ((let sem_CaseAlts_Cons_5 :: T_CaseAlts_5 
                                                                                                                                                                                                               sem_CaseAlts_Cons_5  =
                                                                                                                                                                                                                   (\ _lhsIknPatTy
                                                                                                                                                                                                                      _lhsIknTy
                                                                                                                                                                                                                      _lhsIpatTyVarMp
                                                                                                                                                                                                                      _lhsIvalGam ->
                                                                                                                                                                                                                        _lhsIknPatTy `seq`
                                                                                                                                                                                                                        (_lhsIknTy `seq`
                                                                                                                                                                                                                         (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                          (_lhsIvalGam `seq`
                                                                                                                                                                                                                           ((case (_lhsIvalGam) of
                                                                                                                                                                                                                             { _tlOvalGam | _tlOvalGam `seq` (True) ->
                                                                                                                                                                                                                             (case (_lhsIvalGam) of
                                                                                                                                                                                                                              { _hdOvalGam | _hdOvalGam `seq` (True) ->
                                                                                                                                                                                                                              (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                               { _hdOpatTyVarMp | _hdOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                               (case (_lhsIknPatTy) of
                                                                                                                                                                                                                                { _hdOknPatTy | _hdOknPatTy `seq` (True) ->
                                                                                                                                                                                                                                (case (hd_5 _hdOknPatTy _hdOpatTyVarMp _hdOvalGam ) of
                                                                                                                                                                                                                                 { ( _hdIpatTyVarMp,hd_6) | True ->
                                                                                                                                                                                                                                     (case (_hdIpatTyVarMp) of
                                                                                                                                                                                                                                      { _tlOpatTyVarMp | _tlOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                      (case (_lhsIknPatTy) of
                                                                                                                                                                                                                                       { _tlOknPatTy | _tlOknPatTy `seq` (True) ->
                                                                                                                                                                                                                                       (case (_lhsIknTy) of
                                                                                                                                                                                                                                        { _tlOknTy | _tlOknTy `seq` (True) ->
                                                                                                                                                                                                                                        (case (tl_5 _tlOknPatTy _tlOknTy _tlOpatTyVarMp _tlOvalGam ) of
                                                                                                                                                                                                                                         { ( _tlIpatTyVarMp,_tlIty,tl_6) | True ->
                                                                                                                                                                                                                                             (case (_tlIpatTyVarMp) of
                                                                                                                                                                                                                                              { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                              (case (_tlIty) of
                                                                                                                                                                                                                                               { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                               (case ((let sem_CaseAlts_Cons_6 :: T_CaseAlts_6 
                                                                                                                                                                                                                                                           sem_CaseAlts_Cons_6  =
                                                                                                                                                                                                                                                               (\ _lhsIchrStore
                                                                                                                                                                                                                                                                  _lhsIclDfGam
                                                                                                                                                                                                                                                                  _lhsIfiOpts
                                                                                                                                                                                                                                                                  _lhsItvKiVarMp
                                                                                                                                                                                                                                                                  _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                  _lhsItyVarMp
                                                                                                                                                                                                                                                                  _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                    _lhsIchrStore `seq`
                                                                                                                                                                                                                                                                    (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                     (_lhsIfiOpts `seq`
                                                                                                                                                                                                                                                                      (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                       (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                        (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                         (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                          ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                            { _tlOvalTyGlobFreeTvarS | _tlOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                             { _hdOvalTyGlobFreeTvarS | _hdOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_lhsItyVarMp) of
                                                                                                                                                                                                                                                                              { _hdOtyVarMp | _hdOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                               { _hdOtyTyGlobFreeTvarS | _hdOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                { _hdOtvKiVarMp | _hdOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                 { _hdOfiOpts | _hdOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                  { _hdOclDfGam | _hdOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                   { _hdOchrStore | _hdOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_lhsIknTy) of
                                                                                                                                                                                                                                                                                    { _hdOknTy | _hdOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (hd_6 _hdOchrStore _hdOclDfGam _hdOfiOpts _hdOknTy _hdOtvKiVarMp _hdOtyTyGlobFreeTvarS _hdOtyVarMp _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                     { ( _hdIgathCnstrMp,_hdIgathRangeMp,_hdItyVarMp,hd_7) | True ->
                                                                                                                                                                                                                                                                                         (case (_hdItyVarMp) of
                                                                                                                                                                                                                                                                                          { _tlOtyVarMp | _tlOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                           { _tlOtyTyGlobFreeTvarS | _tlOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                            { _tlOtvKiVarMp | _tlOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                             { _tlOfiOpts | _tlOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                              { _tlOclDfGam | _tlOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                               { _tlOchrStore | _tlOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (tl_6 _tlOchrStore _tlOclDfGam _tlOfiOpts _tlOtvKiVarMp _tlOtyTyGlobFreeTvarS _tlOtyVarMp _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                { ( _tlIgathCnstrMp,_tlIgathRangeMp,_tlItyVarMp,tl_7) | True ->
                                                                                                                                                                                                                                                                                                    (case (_hdIgathCnstrMp `cnstrMpUnion` _tlIgathCnstrMp) of
                                                                                                                                                                                                                                                                                                     { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                     (case (_hdIgathRangeMp `Map.union` _tlIgathRangeMp) of
                                                                                                                                                                                                                                                                                                      { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (_tlItyVarMp) of
                                                                                                                                                                                                                                                                                                       { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case ((let sem_CaseAlts_Cons_7 :: T_CaseAlts_7 
                                                                                                                                                                                                                                                                                                                   sem_CaseAlts_Cons_7  =
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
                                                                                                                                                                                                                                                                                                                                    { _tlOrangeMp | _tlOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                     { _tlOfinValGam | _tlOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                      { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                                                       { _hdOrangeMp | _hdOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                        { _hdOfinValGam | _hdOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                        (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                         { _hdOfinTyVarMp | _hdOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                         (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                          { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                          (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                           { _tlOchrScopeBindMp | _tlOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                           (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                            { _tlOchrEvidBindMp | _tlOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                            (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                             { _hdOchrScopeBindMp | _hdOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                             (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                              { _hdOchrEvidBindMp | _hdOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                              (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                               { _hdOcSubst | _hdOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                               (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                (case (hd_7 _hdOcSubst _hdOchrEvidBindMp _hdOchrScopeBindMp _hdOfinTyVarMp _hdOfinValGam _hdOmoduleNm _hdOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                 { ( _hdIallErrSq,_hdIcSubst,_hdIerrSq,_hdIgathMentrelFilterMp,_hdIgathTvKiVarMp,_hdIpatTy,_hdIpp,_hdIralt,_hdIralt',_hdIty) | True ->
                                                                                                                                                                                                                                                                                                                                                     (case (_hdIcSubst) of
                                                                                                                                                                                                                                                                                                                                                      { _tlOcSubst | _tlOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                      (case (tl_7 _tlOcSubst _tlOchrEvidBindMp _tlOchrScopeBindMp _tlOfinTyVarMp _tlOfinValGam _tlOmoduleNm _tlOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                       { ( _tlIallErrSq,_tlIcSubst,_tlIerrSq,_tlIgathMentrelFilterMp,_tlIgathTvKiVarMp,_tlIpatTy,_tlIpp,_tlIppL,_tlIraltL,_tlIraltL') | True ->
                                                                                                                                                                                                                                                                                                                                                           (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                            { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                            (case (_tlIcSubst) of
                                                                                                                                                                                                                                                                                                                                                             { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                             (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                                                                                                                                                                                                                              { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                              (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                               { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                               (case (_hdIgathTvKiVarMp `varmpUnion` _tlIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (_tlIpatTy) of
                                                                                                                                                                                                                                                                                                                                                                 { _lhsOpatTy | _lhsOpatTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                                                                                                                                                                                                                                  { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                  (case (empty) of
                                                                                                                                                                                                                                                                                                                                                                   { _extraPP | _extraPP `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                   (case ((_hdIpp >|< _extraPP) : _tlIppL) of
                                                                                                                                                                                                                                                                                                                                                                    { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                    (case (_hdIralt  : _tlIraltL) of
                                                                                                                                                                                                                                                                                                                                                                     { _lhsOraltL | _lhsOraltL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                     (case (_hdIralt' : _tlIraltL') of
                                                                                                                                                                                                                                                                                                                                                                      { _lhsOraltL' | _lhsOraltL' `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                      ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTy,_lhsOpp,_lhsOppL,_lhsOraltL,_lhsOraltL') }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                                                                               in  sem_CaseAlts_Cons_7)) of
                                                                                                                                                                                                                                                                                                        { ( sem_CaseAlts_7) | True ->
                                                                                                                                                                                                                                                                                                        ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOtyVarMp,sem_CaseAlts_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                       in  sem_CaseAlts_Cons_6)) of
                                                                                                                                                                                                                                                { ( sem_CaseAlts_6) | True ->
                                                                                                                                                                                                                                                ( _lhsOpatTyVarMp,_lhsOty,sem_CaseAlts_6) }) }) }) }) }) }) }) }) }) }) }) }))))))
                                                                                                                                                                                                           in  sem_CaseAlts_Cons_5)) of
                                                                                                                                                                                                    { ( sem_CaseAlts_5) | True ->
                                                                                                                                                                                                    ( _lhsOchrInstDeclSq,sem_CaseAlts_5) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                            in  sem_CaseAlts_Cons_4)) of
                                                                                                                                                     { ( sem_CaseAlts_4) | True ->
                                                                                                                                                     ( _lhsOkiVarMp,_lhsOpolVarMp,sem_CaseAlts_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                   in  sem_CaseAlts_Cons_3)) of
                                                                                            { ( sem_CaseAlts_3) | True ->
                                                                                            ( _lhsOpredSameScopeCounter,sem_CaseAlts_3) }) }) }) }) }) }) }) }) }) })))))
                                                          in  sem_CaseAlts_Cons_2)) of
                                                   { ( sem_CaseAlts_2) | True ->
                                                   ( _lhsOgUniq,sem_CaseAlts_2) }) }) }) }) }) })))
                       in  sem_CaseAlts_Cons_1)) of
                { ( sem_CaseAlts_1) | True ->
                ( _lhsOrange,sem_CaseAlts_1) }) }) }) })

sem_CaseAlts_Nil :: T_CaseAlts 

sem_CaseAlts_Nil  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_CaseAlts_Nil_1 :: T_CaseAlts_1 
                 sem_CaseAlts_Nil_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_CaseAlts_Nil_2 :: T_CaseAlts_2 
                                        sem_CaseAlts_Nil_2  =
                                            (\ _lhsIkiGam
                                               _lhsIlexLev
                                               _lhsIpredSameScopeCounter ->
                                                 _lhsIkiGam `seq`
                                                 (_lhsIlexLev `seq`
                                                  (_lhsIpredSameScopeCounter `seq`
                                                   ((case (_lhsIpredSameScopeCounter) of
                                                     { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                     (case ((let sem_CaseAlts_Nil_3 :: T_CaseAlts_3 
                                                                 sem_CaseAlts_Nil_3  =
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
                                                                                 ((case (_lhsIkiVarMp) of
                                                                                   { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                   (case (_lhsIpolVarMp) of
                                                                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                    (case ((let sem_CaseAlts_Nil_4 :: T_CaseAlts_4 
                                                                                                sem_CaseAlts_Nil_4  =
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
                                                                                                             ((case (Seq.empty) of
                                                                                                               { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                               (case ((let sem_CaseAlts_Nil_5 :: T_CaseAlts_5 
                                                                                                                           sem_CaseAlts_Nil_5  =
                                                                                                                               (\ _lhsIknPatTy
                                                                                                                                  _lhsIknTy
                                                                                                                                  _lhsIpatTyVarMp
                                                                                                                                  _lhsIvalGam ->
                                                                                                                                    _lhsIknPatTy `seq`
                                                                                                                                    (_lhsIknTy `seq`
                                                                                                                                     (_lhsIpatTyVarMp `seq`
                                                                                                                                      (_lhsIvalGam `seq`
                                                                                                                                       ((case (_lhsIpatTyVarMp) of
                                                                                                                                         { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                         (case (_lhsIknTy) of
                                                                                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                          (case ((let sem_CaseAlts_Nil_6 :: T_CaseAlts_6 
                                                                                                                                                      sem_CaseAlts_Nil_6  =
                                                                                                                                                          (\ _lhsIchrStore
                                                                                                                                                             _lhsIclDfGam
                                                                                                                                                             _lhsIfiOpts
                                                                                                                                                             _lhsItvKiVarMp
                                                                                                                                                             _lhsItyTyGlobFreeTvarS
                                                                                                                                                             _lhsItyVarMp
                                                                                                                                                             _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                               _lhsIchrStore `seq`
                                                                                                                                                               (_lhsIclDfGam `seq`
                                                                                                                                                                (_lhsIfiOpts `seq`
                                                                                                                                                                 (_lhsItvKiVarMp `seq`
                                                                                                                                                                  (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                   (_lhsItyVarMp `seq`
                                                                                                                                                                    (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                     ((case (Map.empty) of
                                                                                                                                                                       { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                       (case (Map.empty) of
                                                                                                                                                                        { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                        (case (_lhsItyVarMp) of
                                                                                                                                                                         { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                         (case ((let sem_CaseAlts_Nil_7 :: T_CaseAlts_7 
                                                                                                                                                                                     sem_CaseAlts_Nil_7  =
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
                                                                                                                                                                                                    ((case (Seq.empty) of
                                                                                                                                                                                                      { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                      (case (_lhsIcSubst) of
                                                                                                                                                                                                       { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                       (case (Seq.empty) of
                                                                                                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                        (case (Map.empty) of
                                                                                                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                         (case (emptyVarMp) of
                                                                                                                                                                                                          { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                          (case (_lhsIknPatTy) of
                                                                                                                                                                                                           { _lhsOpatTy | _lhsOpatTy `seq` (True) ->
                                                                                                                                                                                                           (case (empty) of
                                                                                                                                                                                                            { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                            (case ([]) of
                                                                                                                                                                                                             { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                             (case ([]) of
                                                                                                                                                                                                              { _lhsOraltL | _lhsOraltL `seq` (True) ->
                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                               { _lhsOraltL' | _lhsOraltL' `seq` (True) ->
                                                                                                                                                                                                               ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTy,_lhsOpp,_lhsOppL,_lhsOraltL,_lhsOraltL') }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                 in  sem_CaseAlts_Nil_7)) of
                                                                                                                                                                          { ( sem_CaseAlts_7) | True ->
                                                                                                                                                                          ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOtyVarMp,sem_CaseAlts_7) }) }) }) })))))))))
                                                                                                                                                  in  sem_CaseAlts_Nil_6)) of
                                                                                                                                           { ( sem_CaseAlts_6) | True ->
                                                                                                                                           ( _lhsOpatTyVarMp,_lhsOty,sem_CaseAlts_6) }) }) }))))))
                                                                                                                       in  sem_CaseAlts_Nil_5)) of
                                                                                                                { ( sem_CaseAlts_5) | True ->
                                                                                                                ( _lhsOchrInstDeclSq,sem_CaseAlts_5) }) })))))))
                                                                                            in  sem_CaseAlts_Nil_4)) of
                                                                                     { ( sem_CaseAlts_4) | True ->
                                                                                     ( _lhsOkiVarMp,_lhsOpolVarMp,sem_CaseAlts_4) }) }) }))))))))))
                                                             in  sem_CaseAlts_Nil_3)) of
                                                      { ( sem_CaseAlts_3) | True ->
                                                      ( _lhsOpredSameScopeCounter,sem_CaseAlts_3) }) })))))
                                    in  sem_CaseAlts_Nil_2)) of
                             { ( sem_CaseAlts_2) | True ->
                             ( _lhsOgUniq,sem_CaseAlts_2) }) })))
             in  sem_CaseAlts_Nil_1)) of
      { ( sem_CaseAlts_1) | True ->
      ( _lhsOrange,sem_CaseAlts_1) }) })


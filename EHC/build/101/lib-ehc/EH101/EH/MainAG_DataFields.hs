


module EH101.EH.MainAG_DataFields where

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

-- DataFields --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         knPolCtx             : Polarity
         polGam               : PolGam
      chained attributes:
         polVarMp             : VarMp
         tyGam                : TyGam
      synthesized attributes:
         fldAnnL              : [DataConFldAnnInfo]
         fldTyL               : FldTyL
   visit 3:
      chained attribute:
         tyKiGam              : TyKiGam
      synthesized attribute:
         intlTyKiGam          : TyKiGam
   visit 4:
      inherited attribute:
         dataTy               : Ty
      chained attribute:
         kiVarMp              : VarMp
      synthesized attributes:
         fldSelGam            : ValGam
         fldUpdGam            : ValGam
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
         kiL                  : TyL
   visit 5:
      inherited attributes:
         chrEvidBindMp        : EvidKeyToCBindMap
         chrScopeBindMp       : PredScopeToCBindMap
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         clGam                : ClGam
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         finTyVarMp           : VarMp
         kiGam                : KiGam
         lexLev               : Int
         moduleNm             : HsName
         opts                 : EHCOpts
         predScope            : PredScope
         rangeMp              : RangeMp
         tvKiVarMp            : VarMp
         tyKiGlobFreeTvarS    : TyVarIdS
         tyTyGlobFreeTvarS    : TyVarIdS
         tyTyTySigFreeTvarS   : TyVarIdS
         valTyGlobFreeTvarS   : TyVarIdS
      chained attribute:
         patTyVarMp           : VarMp
      synthesized attributes:
         allErrSq             : ErrSq
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : DataField 
         child tl             : DataFields 
      alternative Nil:
-}
sem_DataFields_Cons :: T_DataField  ->
                       T_DataFields  ->
                       T_DataFields 

sem_DataFields_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (case (tl_ ) of
     { ( _tlIrange,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdIrange,hd_1) | True ->
              (case (_hdIrange `rangeUnion` _tlIrange) of
               { _lhsOrange | _lhsOrange `seq` (True) ->
               (case ((let sem_DataFields_Cons_1 :: T_DataFields_1 
                           sem_DataFields_Cons_1  =
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
                                                  (case ((let sem_DataFields_Cons_2 :: T_DataFields_2 
                                                              sem_DataFields_Cons_2  =
                                                                  (\ _lhsIknPolCtx
                                                                     _lhsIpolGam
                                                                     _lhsIpolVarMp
                                                                     _lhsItyGam ->
                                                                       _lhsIknPolCtx `seq`
                                                                       (_lhsIpolGam `seq`
                                                                        (_lhsIpolVarMp `seq`
                                                                         (_lhsItyGam `seq`
                                                                          ((case (_lhsItyGam) of
                                                                            { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                                                            (case (_lhsIpolVarMp) of
                                                                             { _hdOpolVarMp | _hdOpolVarMp `seq` (True) ->
                                                                             (case (_lhsIpolGam) of
                                                                              { _hdOpolGam | _hdOpolGam `seq` (True) ->
                                                                              (case (_lhsIknPolCtx) of
                                                                               { _hdOknPolCtx | _hdOknPolCtx `seq` (True) ->
                                                                               (case (hd_2 _hdOknPolCtx _hdOpolGam _hdOpolVarMp _hdOtyGam ) of
                                                                                { ( _hdIfldAnnL,_hdIfldTyL,_hdIpolVarMp,_hdItyGam,hd_3) | True ->
                                                                                    (case (_hdItyGam) of
                                                                                     { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                                     (case (_hdIpolVarMp) of
                                                                                      { _tlOpolVarMp | _tlOpolVarMp `seq` (True) ->
                                                                                      (case (_lhsIpolGam) of
                                                                                       { _tlOpolGam | _tlOpolGam `seq` (True) ->
                                                                                       (case (_lhsIknPolCtx) of
                                                                                        { _tlOknPolCtx | _tlOknPolCtx `seq` (True) ->
                                                                                        (case (tl_2 _tlOknPolCtx _tlOpolGam _tlOpolVarMp _tlOtyGam ) of
                                                                                         { ( _tlIfldAnnL,_tlIfldTyL,_tlIpolVarMp,_tlItyGam,tl_3) | True ->
                                                                                             (case (_hdIfldAnnL ++ _tlIfldAnnL) of
                                                                                              { _lhsOfldAnnL | _lhsOfldAnnL `seq` (True) ->
                                                                                              (case (_hdIfldTyL ++ _tlIfldTyL) of
                                                                                               { _lhsOfldTyL | _lhsOfldTyL `seq` (True) ->
                                                                                               (case (_tlIpolVarMp) of
                                                                                                { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                (case (_tlItyGam) of
                                                                                                 { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                                 (case ((let sem_DataFields_Cons_3 :: T_DataFields_3 
                                                                                                             sem_DataFields_Cons_3  =
                                                                                                                 (\ _lhsItyKiGam ->
                                                                                                                      _lhsItyKiGam `seq`
                                                                                                                      ((case (_lhsItyKiGam) of
                                                                                                                        { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                                                        (case (hd_3 _hdOtyKiGam ) of
                                                                                                                         { ( _hdIintlTyKiGam,_hdItyKiGam,hd_4) | True ->
                                                                                                                             (case (_hdItyKiGam) of
                                                                                                                              { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                                                              (case (tl_3 _tlOtyKiGam ) of
                                                                                                                               { ( _tlIintlTyKiGam,_tlItyKiGam,tl_4) | True ->
                                                                                                                                   (case (_hdIintlTyKiGam `gamUnion` _tlIintlTyKiGam) of
                                                                                                                                    { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                    (case (_tlItyKiGam) of
                                                                                                                                     { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                     (case ((let sem_DataFields_Cons_4 :: T_DataFields_4 
                                                                                                                                                 sem_DataFields_Cons_4  =
                                                                                                                                                     (\ _lhsIdataTy
                                                                                                                                                        _lhsIkiVarMp ->
                                                                                                                                                          _lhsIdataTy `seq`
                                                                                                                                                          (_lhsIkiVarMp `seq`
                                                                                                                                                           ((case (_lhsIdataTy) of
                                                                                                                                                             { _tlOdataTy | _tlOdataTy `seq` (True) ->
                                                                                                                                                             (case (_lhsIdataTy) of
                                                                                                                                                              { _hdOdataTy | _hdOdataTy `seq` (True) ->
                                                                                                                                                              (case (_lhsIkiVarMp) of
                                                                                                                                                               { _hdOkiVarMp | _hdOkiVarMp `seq` (True) ->
                                                                                                                                                               (case (hd_4 _hdOdataTy _hdOkiVarMp ) of
                                                                                                                                                                { ( _hdIfldSelGam,_hdIfldUpdGam,_hdIgathCnstrMp,_hdIgathRangeMp,_hdIkiL,_hdIkiVarMp,hd_5) | True ->
                                                                                                                                                                    (case (_hdIkiVarMp) of
                                                                                                                                                                     { _tlOkiVarMp | _tlOkiVarMp `seq` (True) ->
                                                                                                                                                                     (case (tl_4 _tlOdataTy _tlOkiVarMp ) of
                                                                                                                                                                      { ( _tlIfldSelGam,_tlIfldUpdGam,_tlIgathCnstrMp,_tlIgathRangeMp,_tlIkiL,_tlIkiVarMp,tl_5) | True ->
                                                                                                                                                                          (case (_hdIfldSelGam `gamUnion` _tlIfldSelGam) of
                                                                                                                                                                           { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                                                                                           (case (_hdIfldUpdGam `gamUnion` _tlIfldUpdGam) of
                                                                                                                                                                            { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                                                                                            (case (_hdIgathCnstrMp `cnstrMpUnion` _tlIgathCnstrMp) of
                                                                                                                                                                             { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                             (case (_hdIgathRangeMp `Map.union` _tlIgathRangeMp) of
                                                                                                                                                                              { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                              (case (_hdIkiL ++ _tlIkiL) of
                                                                                                                                                                               { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                                                                                               (case (_tlIkiVarMp) of
                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                (case ((let sem_DataFields_Cons_5 :: T_DataFields_5 
                                                                                                                                                                                            sem_DataFields_Cons_5  =
                                                                                                                                                                                                (\ _lhsIchrEvidBindMp
                                                                                                                                                                                                   _lhsIchrScopeBindMp
                                                                                                                                                                                                   _lhsIchrStore
                                                                                                                                                                                                   _lhsIclDfGam
                                                                                                                                                                                                   _lhsIclGam
                                                                                                                                                                                                   _lhsIfinKiVarMp
                                                                                                                                                                                                   _lhsIfinTyKiGam
                                                                                                                                                                                                   _lhsIfinTyVarMp
                                                                                                                                                                                                   _lhsIkiGam
                                                                                                                                                                                                   _lhsIlexLev
                                                                                                                                                                                                   _lhsImoduleNm
                                                                                                                                                                                                   _lhsIopts
                                                                                                                                                                                                   _lhsIpatTyVarMp
                                                                                                                                                                                                   _lhsIpredScope
                                                                                                                                                                                                   _lhsIrangeMp
                                                                                                                                                                                                   _lhsItvKiVarMp
                                                                                                                                                                                                   _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                   _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                   _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                   _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                     _lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                     (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                      (_lhsIchrStore `seq`
                                                                                                                                                                                                       (_lhsIclDfGam `seq`
                                                                                                                                                                                                        (_lhsIclGam `seq`
                                                                                                                                                                                                         (_lhsIfinKiVarMp `seq`
                                                                                                                                                                                                          (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                           (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                            (_lhsIkiGam `seq`
                                                                                                                                                                                                             (_lhsIlexLev `seq`
                                                                                                                                                                                                              (_lhsImoduleNm `seq`
                                                                                                                                                                                                               (_lhsIopts `seq`
                                                                                                                                                                                                                (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                 (_lhsIpredScope `seq`
                                                                                                                                                                                                                  (_lhsIrangeMp `seq`
                                                                                                                                                                                                                   (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                    (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                     (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                      (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                       (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                        ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                          { _tlOvalTyGlobFreeTvarS | _tlOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                          (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                           { _tlOtyTyTySigFreeTvarS | _tlOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                           (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                            { _tlOtyTyGlobFreeTvarS | _tlOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                            (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                             { _tlOtyKiGlobFreeTvarS | _tlOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                             (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                              { _tlOtvKiVarMp | _tlOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                              (case (_lhsIrangeMp) of
                                                                                                                                                                                                                               { _tlOrangeMp | _tlOrangeMp `seq` (True) ->
                                                                                                                                                                                                                               (case (_lhsIpredScope) of
                                                                                                                                                                                                                                { _tlOpredScope | _tlOpredScope `seq` (True) ->
                                                                                                                                                                                                                                (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                                 { _hdOpatTyVarMp | _hdOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                 (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                  { _hdOvalTyGlobFreeTvarS | _hdOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                  (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                   { _hdOtyTyTySigFreeTvarS | _hdOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                   (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                    { _hdOtyTyGlobFreeTvarS | _hdOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                    (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                     { _hdOtyKiGlobFreeTvarS | _hdOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                     (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                      { _hdOtvKiVarMp | _hdOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                      (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                       { _hdOrangeMp | _hdOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                       (case (_lhsIpredScope) of
                                                                                                                                                                                                                                        { _hdOpredScope | _hdOpredScope `seq` (True) ->
                                                                                                                                                                                                                                        (case (_lhsIopts) of
                                                                                                                                                                                                                                         { _hdOopts | _hdOopts `seq` (True) ->
                                                                                                                                                                                                                                         (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                          { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                          (case (_lhsIlexLev) of
                                                                                                                                                                                                                                           { _hdOlexLev | _hdOlexLev `seq` (True) ->
                                                                                                                                                                                                                                           (case (_lhsIkiGam) of
                                                                                                                                                                                                                                            { _hdOkiGam | _hdOkiGam `seq` (True) ->
                                                                                                                                                                                                                                            (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                             { _hdOfinTyVarMp | _hdOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                             (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                              { _hdOfinTyKiGam | _hdOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                              (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                               { _hdOfinKiVarMp | _hdOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                               (case (_lhsIclGam) of
                                                                                                                                                                                                                                                { _hdOclGam | _hdOclGam `seq` (True) ->
                                                                                                                                                                                                                                                (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                 { _hdOclDfGam | _hdOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                 (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                  { _hdOchrStore | _hdOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                  (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                   { _hdOchrScopeBindMp | _hdOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                   (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                    { _hdOchrEvidBindMp | _hdOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                    (case (hd_5 _hdOchrEvidBindMp _hdOchrScopeBindMp _hdOchrStore _hdOclDfGam _hdOclGam _hdOfinKiVarMp _hdOfinTyKiGam _hdOfinTyVarMp _hdOkiGam _hdOlexLev _hdOmoduleNm _hdOopts _hdOpatTyVarMp _hdOpredScope _hdOrangeMp _hdOtvKiVarMp _hdOtyKiGlobFreeTvarS _hdOtyTyGlobFreeTvarS _hdOtyTyTySigFreeTvarS _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                     { ( _hdIallErrSq,_hdIerrSq,_hdIgathMentrelFilterMp,_hdIgathTvKiVarMp,_hdIpatTyVarMp,_hdIpp) | True ->
                                                                                                                                                                                                                                                         (case (_hdIpatTyVarMp) of
                                                                                                                                                                                                                                                          { _tlOpatTyVarMp | _tlOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                          (case (_lhsIopts) of
                                                                                                                                                                                                                                                           { _tlOopts | _tlOopts `seq` (True) ->
                                                                                                                                                                                                                                                           (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                            { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                            (case (_lhsIlexLev) of
                                                                                                                                                                                                                                                             { _tlOlexLev | _tlOlexLev `seq` (True) ->
                                                                                                                                                                                                                                                             (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                              { _tlOkiGam | _tlOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                              (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                               { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                               (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                 { _tlOfinKiVarMp | _tlOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                 (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                  { _tlOclGam | _tlOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                  (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                   { _tlOclDfGam | _tlOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                   (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                    { _tlOchrStore | _tlOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                    (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                     { _tlOchrScopeBindMp | _tlOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                     (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                      { _tlOchrEvidBindMp | _tlOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                      (case (tl_5 _tlOchrEvidBindMp _tlOchrScopeBindMp _tlOchrStore _tlOclDfGam _tlOclGam _tlOfinKiVarMp _tlOfinTyKiGam _tlOfinTyVarMp _tlOkiGam _tlOlexLev _tlOmoduleNm _tlOopts _tlOpatTyVarMp _tlOpredScope _tlOrangeMp _tlOtvKiVarMp _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                       { ( _tlIallErrSq,_tlIerrSq,_tlIgathMentrelFilterMp,_tlIgathTvKiVarMp,_tlIpatTyVarMp,_tlIpp,_tlIppL) | True ->
                                                                                                                                                                                                                                                                           (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                                                                                                                                            { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                                                                                                                                             { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_hdIgathTvKiVarMp `varmpUnion` _tlIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                               { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_tlIpatTyVarMp) of
                                                                                                                                                                                                                                                                                { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                                                                                                                                                 { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_hdIpp : _tlIppL) of
                                                                                                                                                                                                                                                                                  { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                  ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTyVarMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))))))))))
                                                                                                                                                                                        in  sem_DataFields_Cons_5)) of
                                                                                                                                                                                 { ( sem_DataFields_5) | True ->
                                                                                                                                                                                 ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOkiL,_lhsOkiVarMp,sem_DataFields_5) }) }) }) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                                             in  sem_DataFields_Cons_4)) of
                                                                                                                                      { ( sem_DataFields_4) | True ->
                                                                                                                                      ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_DataFields_4) }) }) }) }) }) }) })))
                                                                                                         in  sem_DataFields_Cons_3)) of
                                                                                                  { ( sem_DataFields_3) | True ->
                                                                                                  ( _lhsOfldAnnL,_lhsOfldTyL,_lhsOpolVarMp,_lhsOtyGam,sem_DataFields_3) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))
                                                          in  sem_DataFields_Cons_2)) of
                                                   { ( sem_DataFields_2) | True ->
                                                   ( _lhsOgUniq,sem_DataFields_2) }) }) }) }) }) })))
                       in  sem_DataFields_Cons_1)) of
                { ( sem_DataFields_1) | True ->
                ( _lhsOrange,sem_DataFields_1) }) }) }) })

sem_DataFields_Nil :: T_DataFields 

sem_DataFields_Nil  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_DataFields_Nil_1 :: T_DataFields_1 
                 sem_DataFields_Nil_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_DataFields_Nil_2 :: T_DataFields_2 
                                        sem_DataFields_Nil_2  =
                                            (\ _lhsIknPolCtx
                                               _lhsIpolGam
                                               _lhsIpolVarMp
                                               _lhsItyGam ->
                                                 _lhsIknPolCtx `seq`
                                                 (_lhsIpolGam `seq`
                                                  (_lhsIpolVarMp `seq`
                                                   (_lhsItyGam `seq`
                                                    ((case ([]) of
                                                      { _lhsOfldAnnL | _lhsOfldAnnL `seq` (True) ->
                                                      (case ([]) of
                                                       { _lhsOfldTyL | _lhsOfldTyL `seq` (True) ->
                                                       (case (_lhsIpolVarMp) of
                                                        { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                        (case (_lhsItyGam) of
                                                         { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                         (case ((let sem_DataFields_Nil_3 :: T_DataFields_3 
                                                                     sem_DataFields_Nil_3  =
                                                                         (\ _lhsItyKiGam ->
                                                                              _lhsItyKiGam `seq`
                                                                              ((case (emptyGam) of
                                                                                { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                (case (_lhsItyKiGam) of
                                                                                 { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                 (case ((let sem_DataFields_Nil_4 :: T_DataFields_4 
                                                                                             sem_DataFields_Nil_4  =
                                                                                                 (\ _lhsIdataTy
                                                                                                    _lhsIkiVarMp ->
                                                                                                      _lhsIdataTy `seq`
                                                                                                      (_lhsIkiVarMp `seq`
                                                                                                       ((case (emptyGam) of
                                                                                                         { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                         (case (emptyGam) of
                                                                                                          { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                          (case (Map.empty) of
                                                                                                           { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                           (case (Map.empty) of
                                                                                                            { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                            (case ([]) of
                                                                                                             { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                             (case (_lhsIkiVarMp) of
                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                              (case ((let sem_DataFields_Nil_5 :: T_DataFields_5 
                                                                                                                          sem_DataFields_Nil_5  =
                                                                                                                              (\ _lhsIchrEvidBindMp
                                                                                                                                 _lhsIchrScopeBindMp
                                                                                                                                 _lhsIchrStore
                                                                                                                                 _lhsIclDfGam
                                                                                                                                 _lhsIclGam
                                                                                                                                 _lhsIfinKiVarMp
                                                                                                                                 _lhsIfinTyKiGam
                                                                                                                                 _lhsIfinTyVarMp
                                                                                                                                 _lhsIkiGam
                                                                                                                                 _lhsIlexLev
                                                                                                                                 _lhsImoduleNm
                                                                                                                                 _lhsIopts
                                                                                                                                 _lhsIpatTyVarMp
                                                                                                                                 _lhsIpredScope
                                                                                                                                 _lhsIrangeMp
                                                                                                                                 _lhsItvKiVarMp
                                                                                                                                 _lhsItyKiGlobFreeTvarS
                                                                                                                                 _lhsItyTyGlobFreeTvarS
                                                                                                                                 _lhsItyTyTySigFreeTvarS
                                                                                                                                 _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                   _lhsIchrEvidBindMp `seq`
                                                                                                                                   (_lhsIchrScopeBindMp `seq`
                                                                                                                                    (_lhsIchrStore `seq`
                                                                                                                                     (_lhsIclDfGam `seq`
                                                                                                                                      (_lhsIclGam `seq`
                                                                                                                                       (_lhsIfinKiVarMp `seq`
                                                                                                                                        (_lhsIfinTyKiGam `seq`
                                                                                                                                         (_lhsIfinTyVarMp `seq`
                                                                                                                                          (_lhsIkiGam `seq`
                                                                                                                                           (_lhsIlexLev `seq`
                                                                                                                                            (_lhsImoduleNm `seq`
                                                                                                                                             (_lhsIopts `seq`
                                                                                                                                              (_lhsIpatTyVarMp `seq`
                                                                                                                                               (_lhsIpredScope `seq`
                                                                                                                                                (_lhsIrangeMp `seq`
                                                                                                                                                 (_lhsItvKiVarMp `seq`
                                                                                                                                                  (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                   (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                    (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                     (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                      ((case (Seq.empty) of
                                                                                                                                                        { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                        (case (Seq.empty) of
                                                                                                                                                         { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                         (case (Map.empty) of
                                                                                                                                                          { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                          (case (emptyVarMp) of
                                                                                                                                                           { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                           (case (_lhsIpatTyVarMp) of
                                                                                                                                                            { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                            (case (empty) of
                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                             (case ([]) of
                                                                                                                                                              { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                              ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTyVarMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }))))))))))))))))))))))
                                                                                                                      in  sem_DataFields_Nil_5)) of
                                                                                                               { ( sem_DataFields_5) | True ->
                                                                                                               ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOkiL,_lhsOkiVarMp,sem_DataFields_5) }) }) }) }) }) }) }))))
                                                                                         in  sem_DataFields_Nil_4)) of
                                                                                  { ( sem_DataFields_4) | True ->
                                                                                  ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_DataFields_4) }) }) })))
                                                                 in  sem_DataFields_Nil_3)) of
                                                          { ( sem_DataFields_3) | True ->
                                                          ( _lhsOfldAnnL,_lhsOfldTyL,_lhsOpolVarMp,_lhsOtyGam,sem_DataFields_3) }) }) }) }) }))))))
                                    in  sem_DataFields_Nil_2)) of
                             { ( sem_DataFields_2) | True ->
                             ( _lhsOgUniq,sem_DataFields_2) }) })))
             in  sem_DataFields_Nil_1)) of
      { ( sem_DataFields_1) | True ->
      ( _lhsOrange,sem_DataFields_1) }) })


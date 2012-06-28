


module EH101.EH.MainAG_DataConstrs where

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

-- DataConstrs -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataTy               : Ty
         knPolCtx             : Polarity
         polGam               : PolGam
         tyGam                : TyGam
      chained attribute:
         polVarMp             : VarMp
      synthesized attribute:
         dataAltTyL           : AssocL HsName Ty
   visit 3:
      inherited attribute:
         tyKiGam              : TyKiGam
      chained attribute:
         kiVarMp              : VarMp
      synthesized attributes:
         gathMaxArity         : Int
         intlTyKiGam          : TyKiGam
   visit 4:
      inherited attributes:
         dataAltTy            : Ty
         maxArity             : Int
         tyNm                 : HsName
      synthesized attributes:
         dataAltForNewType    : Ty
         dataConstrNmL        : [HsName]
         dataConstrTagMp      : DataConstrTagMp
   visit 5:
      inherited attributes:
         finTyKiGam           : TyKiGam
         tvKiVarMp            : VarMp
      chained attributes:
         patTyVarMp           : VarMp
         patValGam            : ValGam
      synthesized attributes:
         fldSelGam            : ValGam
         fldUpdGam            : ValGam
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
   visit 6:
      inherited attributes:
         chrEvidBindMp        : EvidKeyToCBindMap
         chrScopeBindMp       : PredScopeToCBindMap
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         clGam                : ClGam
         finKiVarMp           : VarMp
         finTyVarMp           : VarMp
         isNewtype            : Bool
         kiGam                : KiGam
         lexLev               : Int
         moduleNm             : HsName
         opts                 : EHCOpts
         predScope            : PredScope
         rangeMp              : RangeMp
         tyKiGlobFreeTvarS    : TyVarIdS
         tyTyGlobFreeTvarS    : TyVarIdS
         tyTyTySigFreeTvarS   : TyVarIdS
         valGam               : ValGam
         valTyGlobFreeTvarS   : TyVarIdS
      synthesized attributes:
         allErrSq             : ErrSq
         cbindL               : CBindL
         errSq                : ErrSq
         ffeCBindL            : CBindL
         ffiCBindL            : CBindL
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : DataConstr 
         child tl             : DataConstrs 
      alternative Nil:
-}
sem_DataConstrs_Cons :: T_DataConstr  ->
                        T_DataConstrs  ->
                        T_DataConstrs 

sem_DataConstrs_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (case (tl_ ) of
     { ( _tlIrange,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdIrange,hd_1) | True ->
              (case (_hdIrange `rangeUnion` _tlIrange) of
               { _lhsOrange | _lhsOrange `seq` (True) ->
               (case ((let sem_DataConstrs_Cons_1 :: T_DataConstrs_1 
                           sem_DataConstrs_Cons_1  =
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
                                                  (case ((let sem_DataConstrs_Cons_2 :: T_DataConstrs_2 
                                                              sem_DataConstrs_Cons_2  =
                                                                  (\ _lhsIdataTy
                                                                     _lhsIknPolCtx
                                                                     _lhsIpolGam
                                                                     _lhsIpolVarMp
                                                                     _lhsItyGam ->
                                                                       _lhsIdataTy `seq`
                                                                       (_lhsIknPolCtx `seq`
                                                                        (_lhsIpolGam `seq`
                                                                         (_lhsIpolVarMp `seq`
                                                                          (_lhsItyGam `seq`
                                                                           ((case (_lhsItyGam) of
                                                                             { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                             (case (_lhsIdataTy) of
                                                                              { _tlOdataTy | _tlOdataTy `seq` (True) ->
                                                                              (case (_lhsItyGam) of
                                                                               { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                                                               (case (_lhsIdataTy) of
                                                                                { _hdOdataTy | _hdOdataTy `seq` (True) ->
                                                                                (case (_lhsIpolVarMp) of
                                                                                 { _hdOpolVarMp | _hdOpolVarMp `seq` (True) ->
                                                                                 (case (_lhsIpolGam) of
                                                                                  { _hdOpolGam | _hdOpolGam `seq` (True) ->
                                                                                  (case (_lhsIknPolCtx) of
                                                                                   { _hdOknPolCtx | _hdOknPolCtx `seq` (True) ->
                                                                                   (case (hd_2 _hdOdataTy _hdOknPolCtx _hdOpolGam _hdOpolVarMp _hdOtyGam ) of
                                                                                    { ( _hdIdataAltTyL,_hdIpolVarMp,hd_3) | True ->
                                                                                        (case (_hdIpolVarMp) of
                                                                                         { _tlOpolVarMp | _tlOpolVarMp `seq` (True) ->
                                                                                         (case (_lhsIpolGam) of
                                                                                          { _tlOpolGam | _tlOpolGam `seq` (True) ->
                                                                                          (case (_lhsIknPolCtx) of
                                                                                           { _tlOknPolCtx | _tlOknPolCtx `seq` (True) ->
                                                                                           (case (tl_2 _tlOdataTy _tlOknPolCtx _tlOpolGam _tlOpolVarMp _tlOtyGam ) of
                                                                                            { ( _tlIdataAltTyL,_tlIpolVarMp,tl_3) | True ->
                                                                                                (case (_hdIdataAltTyL ++ _tlIdataAltTyL) of
                                                                                                 { _lhsOdataAltTyL | _lhsOdataAltTyL `seq` (True) ->
                                                                                                 (case (_tlIpolVarMp) of
                                                                                                  { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                  (case ((let sem_DataConstrs_Cons_3 :: T_DataConstrs_3 
                                                                                                              sem_DataConstrs_Cons_3  =
                                                                                                                  (\ _lhsIkiVarMp
                                                                                                                     _lhsItyKiGam ->
                                                                                                                       _lhsIkiVarMp `seq`
                                                                                                                       (_lhsItyKiGam `seq`
                                                                                                                        ((case (_lhsItyKiGam) of
                                                                                                                          { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                                                          (case (_lhsItyKiGam) of
                                                                                                                           { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                                                           (case (_lhsIkiVarMp) of
                                                                                                                            { _hdOkiVarMp | _hdOkiVarMp `seq` (True) ->
                                                                                                                            (case (hd_3 _hdOkiVarMp _hdOtyKiGam ) of
                                                                                                                             { ( _hdIgathMaxArity,_hdIintlTyKiGam,_hdIkiVarMp,hd_4) | True ->
                                                                                                                                 (case (_hdIkiVarMp) of
                                                                                                                                  { _tlOkiVarMp | _tlOkiVarMp `seq` (True) ->
                                                                                                                                  (case (tl_3 _tlOkiVarMp _tlOtyKiGam ) of
                                                                                                                                   { ( _tlIgathMaxArity,_tlIintlTyKiGam,_tlIkiVarMp,tl_4) | True ->
                                                                                                                                       (case (_hdIgathMaxArity `max` _tlIgathMaxArity) of
                                                                                                                                        { _lhsOgathMaxArity | _lhsOgathMaxArity `seq` (True) ->
                                                                                                                                        (case (_hdIintlTyKiGam `gamUnion` _tlIintlTyKiGam) of
                                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                         (case (_tlIkiVarMp) of
                                                                                                                                          { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                          (case ((let sem_DataConstrs_Cons_4 :: T_DataConstrs_4 
                                                                                                                                                      sem_DataConstrs_Cons_4  =
                                                                                                                                                          (\ _lhsIdataAltTy
                                                                                                                                                             _lhsImaxArity
                                                                                                                                                             _lhsItyNm ->
                                                                                                                                                               _lhsIdataAltTy `seq`
                                                                                                                                                               (_lhsImaxArity `seq`
                                                                                                                                                                (_lhsItyNm `seq`
                                                                                                                                                                 ((case (_lhsItyNm) of
                                                                                                                                                                   { _tlOtyNm | _tlOtyNm `seq` (True) ->
                                                                                                                                                                   (case (_lhsImaxArity) of
                                                                                                                                                                    { _tlOmaxArity | _tlOmaxArity `seq` (True) ->
                                                                                                                                                                    (case (_lhsIdataAltTy) of
                                                                                                                                                                     { _tlOdataAltTy | _tlOdataAltTy `seq` (True) ->
                                                                                                                                                                     (case (tl_4 _tlOdataAltTy _tlOmaxArity _tlOtyNm ) of
                                                                                                                                                                      { ( _tlIdataAltForNewType,_tlIdataConstrNmL,_tlIdataConstrTagMp,tl_5) | True ->
                                                                                                                                                                          (case (_lhsItyNm) of
                                                                                                                                                                           { _hdOtyNm | _hdOtyNm `seq` (True) ->
                                                                                                                                                                           (case (_lhsImaxArity) of
                                                                                                                                                                            { _hdOmaxArity | _hdOmaxArity `seq` (True) ->
                                                                                                                                                                            (case (_lhsIdataAltTy) of
                                                                                                                                                                             { _hdOdataAltTy | _hdOdataAltTy `seq` (True) ->
                                                                                                                                                                             (case (hd_4 _hdOdataAltTy _hdOmaxArity _hdOtyNm ) of
                                                                                                                                                                              { ( _hdIdataAltForNewType,_hdIdataConstrNmL,_hdIdataConstrTagMp,hd_5) | True ->
                                                                                                                                                                                  (case (_hdIdataAltForNewType `const` _tlIdataAltForNewType) of
                                                                                                                                                                                   { _lhsOdataAltForNewType | _lhsOdataAltForNewType `seq` (True) ->
                                                                                                                                                                                   (case (_hdIdataConstrNmL ++ _tlIdataConstrNmL) of
                                                                                                                                                                                    { _lhsOdataConstrNmL | _lhsOdataConstrNmL `seq` (True) ->
                                                                                                                                                                                    (case (_hdIdataConstrTagMp `Map.union` _tlIdataConstrTagMp) of
                                                                                                                                                                                     { _lhsOdataConstrTagMp | _lhsOdataConstrTagMp `seq` (True) ->
                                                                                                                                                                                     (case ((let sem_DataConstrs_Cons_5 :: T_DataConstrs_5 
                                                                                                                                                                                                 sem_DataConstrs_Cons_5  =
                                                                                                                                                                                                     (\ _lhsIfinTyKiGam
                                                                                                                                                                                                        _lhsIpatTyVarMp
                                                                                                                                                                                                        _lhsIpatValGam
                                                                                                                                                                                                        _lhsItvKiVarMp ->
                                                                                                                                                                                                          _lhsIfinTyKiGam `seq`
                                                                                                                                                                                                          (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                           (_lhsIpatValGam `seq`
                                                                                                                                                                                                            (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                             ((case (_lhsItvKiVarMp) of
                                                                                                                                                                                                               { _tlOtvKiVarMp | _tlOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                               (case (_lhsIpatValGam) of
                                                                                                                                                                                                                { _hdOpatValGam | _hdOpatValGam `seq` (True) ->
                                                                                                                                                                                                                (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                 { _hdOtvKiVarMp | _hdOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                 (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                  { _hdOpatTyVarMp | _hdOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                  (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                   { _hdOfinTyKiGam | _hdOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                   (case (hd_5 _hdOfinTyKiGam _hdOpatTyVarMp _hdOpatValGam _hdOtvKiVarMp ) of
                                                                                                                                                                                                                    { ( _hdIfldSelGam,_hdIfldUpdGam,_hdIgathCnstrMp,_hdIgathRangeMp,_hdIpatTyVarMp,_hdIpatValGam,hd_6) | True ->
                                                                                                                                                                                                                        (case (_hdIpatValGam) of
                                                                                                                                                                                                                         { _tlOpatValGam | _tlOpatValGam `seq` (True) ->
                                                                                                                                                                                                                         (case (_hdIpatTyVarMp) of
                                                                                                                                                                                                                          { _tlOpatTyVarMp | _tlOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                          (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                           { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                           (case (tl_5 _tlOfinTyKiGam _tlOpatTyVarMp _tlOpatValGam _tlOtvKiVarMp ) of
                                                                                                                                                                                                                            { ( _tlIfldSelGam,_tlIfldUpdGam,_tlIgathCnstrMp,_tlIgathRangeMp,_tlIpatTyVarMp,_tlIpatValGam,tl_6) | True ->
                                                                                                                                                                                                                                (case (_hdIfldSelGam `gamUnion` _tlIfldSelGam) of
                                                                                                                                                                                                                                 { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                                                                                                                                                 (case (_hdIfldUpdGam `gamUnion` _tlIfldUpdGam) of
                                                                                                                                                                                                                                  { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                                                                                                                                                  (case (_hdIgathCnstrMp `cnstrMpUnion` _tlIgathCnstrMp) of
                                                                                                                                                                                                                                   { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                   (case (_hdIgathRangeMp `Map.union` _tlIgathRangeMp) of
                                                                                                                                                                                                                                    { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                    (case (_tlIpatTyVarMp) of
                                                                                                                                                                                                                                     { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                     (case (_tlIpatValGam) of
                                                                                                                                                                                                                                      { _lhsOpatValGam | _lhsOpatValGam `seq` (True) ->
                                                                                                                                                                                                                                      (case ((let sem_DataConstrs_Cons_6 :: T_DataConstrs_6 
                                                                                                                                                                                                                                                  sem_DataConstrs_Cons_6  =
                                                                                                                                                                                                                                                      (\ _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                         _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                         _lhsIchrStore
                                                                                                                                                                                                                                                         _lhsIclDfGam
                                                                                                                                                                                                                                                         _lhsIclGam
                                                                                                                                                                                                                                                         _lhsIfinKiVarMp
                                                                                                                                                                                                                                                         _lhsIfinTyVarMp
                                                                                                                                                                                                                                                         _lhsIisNewtype
                                                                                                                                                                                                                                                         _lhsIkiGam
                                                                                                                                                                                                                                                         _lhsIlexLev
                                                                                                                                                                                                                                                         _lhsImoduleNm
                                                                                                                                                                                                                                                         _lhsIopts
                                                                                                                                                                                                                                                         _lhsIpredScope
                                                                                                                                                                                                                                                         _lhsIrangeMp
                                                                                                                                                                                                                                                         _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                         _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                         _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                         _lhsIvalGam
                                                                                                                                                                                                                                                         _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                           _lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                           (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                            (_lhsIchrStore `seq`
                                                                                                                                                                                                                                                             (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                              (_lhsIclGam `seq`
                                                                                                                                                                                                                                                               (_lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                                (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                 (_lhsIisNewtype `seq`
                                                                                                                                                                                                                                                                  (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                                   (_lhsIlexLev `seq`
                                                                                                                                                                                                                                                                    (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                     (_lhsIopts `seq`
                                                                                                                                                                                                                                                                      (_lhsIpredScope `seq`
                                                                                                                                                                                                                                                                       (_lhsIrangeMp `seq`
                                                                                                                                                                                                                                                                        (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                         (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                          (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                                           (_lhsIvalGam `seq`
                                                                                                                                                                                                                                                                            (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                             ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                               { _tlOvalTyGlobFreeTvarS | _tlOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                                { _tlOvalGam | _tlOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                 { _tlOtyTyTySigFreeTvarS | _tlOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                  { _tlOtyTyGlobFreeTvarS | _tlOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                   { _tlOtyKiGlobFreeTvarS | _tlOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                    { _tlOrangeMp | _tlOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (_lhsIpredScope) of
                                                                                                                                                                                                                                                                                     { _tlOpredScope | _tlOpredScope `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                      { _tlOopts | _tlOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                       { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_lhsIlexLev) of
                                                                                                                                                                                                                                                                                        { _tlOlexLev | _tlOlexLev `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                         { _tlOkiGam | _tlOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_lhsIisNewtype) of
                                                                                                                                                                                                                                                                                          { _tlOisNewtype | _tlOisNewtype `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                           { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
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
                                                                                                                                                                                                                                                                                                 (case (tl_6 _tlOchrEvidBindMp _tlOchrScopeBindMp _tlOchrStore _tlOclDfGam _tlOclGam _tlOfinKiVarMp _tlOfinTyVarMp _tlOisNewtype _tlOkiGam _tlOlexLev _tlOmoduleNm _tlOopts _tlOpredScope _tlOrangeMp _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalGam _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                  { ( _tlIallErrSq,_tlIcbindL,_tlIerrSq,_tlIffeCBindL,_tlIffiCBindL,_tlIgathMentrelFilterMp,_tlIgathTvKiVarMp,_tlIpp,_tlIppL) | True ->
                                                                                                                                                                                                                                                                                                      (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                       { _hdOvalTyGlobFreeTvarS | _hdOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                                                        { _hdOvalGam | _hdOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                         { _hdOtyTyTySigFreeTvarS | _hdOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                         (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                          { _hdOtyTyGlobFreeTvarS | _hdOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                           { _hdOtyKiGlobFreeTvarS | _hdOtyKiGlobFreeTvarS `seq` (True) ->
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
                                                                                                                                                                                                                                                                                                                 (case (_lhsIisNewtype) of
                                                                                                                                                                                                                                                                                                                  { _hdOisNewtype | _hdOisNewtype `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                   { _hdOfinTyVarMp | _hdOfinTyVarMp `seq` (True) ->
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
                                                                                                                                                                                                                                                                                                                         (case (hd_6 _hdOchrEvidBindMp _hdOchrScopeBindMp _hdOchrStore _hdOclDfGam _hdOclGam _hdOfinKiVarMp _hdOfinTyVarMp _hdOisNewtype _hdOkiGam _hdOlexLev _hdOmoduleNm _hdOopts _hdOpredScope _hdOrangeMp _hdOtyKiGlobFreeTvarS _hdOtyTyGlobFreeTvarS _hdOtyTyTySigFreeTvarS _hdOvalGam _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                          { ( _hdIallErrSq,_hdIcbindL,_hdIerrSq,_hdIffeCBindL,_hdIffiCBindL,_hdIgathMentrelFilterMp,_hdIgathTvKiVarMp,_hdIpp) | True ->
                                                                                                                                                                                                                                                                                                                              (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                                                                                                                                                                                               { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                               (case (_hdIcbindL ++ _tlIcbindL) of
                                                                                                                                                                                                                                                                                                                                { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                                                                                                                                                                                                 { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                 (case (_hdIffeCBindL ++ _tlIffeCBindL) of
                                                                                                                                                                                                                                                                                                                                  { _lhsOffeCBindL | _lhsOffeCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                  (case (_hdIffiCBindL ++ _tlIffiCBindL) of
                                                                                                                                                                                                                                                                                                                                   { _lhsOffiCBindL | _lhsOffiCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                   (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                    { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (_hdIgathTvKiVarMp `varmpUnion` _tlIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                     { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                                                                                                                                                                                                      { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_hdIpp : _tlIppL) of
                                                                                                                                                                                                                                                                                                                                       { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       ( _lhsOallErrSq,_lhsOcbindL,_lhsOerrSq,_lhsOffeCBindL,_lhsOffiCBindL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))))))))))))
                                                                                                                                                                                                                                              in  sem_DataConstrs_Cons_6)) of
                                                                                                                                                                                                                                       { ( sem_DataConstrs_6) | True ->
                                                                                                                                                                                                                                       ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOpatTyVarMp,_lhsOpatValGam,sem_DataConstrs_6) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))
                                                                                                                                                                                             in  sem_DataConstrs_Cons_5)) of
                                                                                                                                                                                      { ( sem_DataConstrs_5) | True ->
                                                                                                                                                                                      ( _lhsOdataAltForNewType,_lhsOdataConstrNmL,_lhsOdataConstrTagMp,sem_DataConstrs_5) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                                                                  in  sem_DataConstrs_Cons_4)) of
                                                                                                                                           { ( sem_DataConstrs_4) | True ->
                                                                                                                                           ( _lhsOgathMaxArity,_lhsOintlTyKiGam,_lhsOkiVarMp,sem_DataConstrs_4) }) }) }) }) }) }) }) }) }) }))))
                                                                                                          in  sem_DataConstrs_Cons_3)) of
                                                                                                   { ( sem_DataConstrs_3) | True ->
                                                                                                   ( _lhsOdataAltTyL,_lhsOpolVarMp,sem_DataConstrs_3) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                          in  sem_DataConstrs_Cons_2)) of
                                                   { ( sem_DataConstrs_2) | True ->
                                                   ( _lhsOgUniq,sem_DataConstrs_2) }) }) }) }) }) })))
                       in  sem_DataConstrs_Cons_1)) of
                { ( sem_DataConstrs_1) | True ->
                ( _lhsOrange,sem_DataConstrs_1) }) }) }) })

sem_DataConstrs_Nil :: T_DataConstrs 

sem_DataConstrs_Nil  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_DataConstrs_Nil_1 :: T_DataConstrs_1 
                 sem_DataConstrs_Nil_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_DataConstrs_Nil_2 :: T_DataConstrs_2 
                                        sem_DataConstrs_Nil_2  =
                                            (\ _lhsIdataTy
                                               _lhsIknPolCtx
                                               _lhsIpolGam
                                               _lhsIpolVarMp
                                               _lhsItyGam ->
                                                 _lhsIdataTy `seq`
                                                 (_lhsIknPolCtx `seq`
                                                  (_lhsIpolGam `seq`
                                                   (_lhsIpolVarMp `seq`
                                                    (_lhsItyGam `seq`
                                                     ((case ([]) of
                                                       { _lhsOdataAltTyL | _lhsOdataAltTyL `seq` (True) ->
                                                       (case (_lhsIpolVarMp) of
                                                        { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                        (case ((let sem_DataConstrs_Nil_3 :: T_DataConstrs_3 
                                                                    sem_DataConstrs_Nil_3  =
                                                                        (\ _lhsIkiVarMp
                                                                           _lhsItyKiGam ->
                                                                             _lhsIkiVarMp `seq`
                                                                             (_lhsItyKiGam `seq`
                                                                              ((case (0) of
                                                                                { _lhsOgathMaxArity | _lhsOgathMaxArity `seq` (True) ->
                                                                                (case (emptyGam) of
                                                                                 { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                 (case (_lhsIkiVarMp) of
                                                                                  { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                  (case ((let sem_DataConstrs_Nil_4 :: T_DataConstrs_4 
                                                                                              sem_DataConstrs_Nil_4  =
                                                                                                  (\ _lhsIdataAltTy
                                                                                                     _lhsImaxArity
                                                                                                     _lhsItyNm ->
                                                                                                       _lhsIdataAltTy `seq`
                                                                                                       (_lhsImaxArity `seq`
                                                                                                        (_lhsItyNm `seq`
                                                                                                         ((case (Ty_Any) of
                                                                                                           { _lhsOdataAltForNewType | _lhsOdataAltForNewType `seq` (True) ->
                                                                                                           (case ([]) of
                                                                                                            { _lhsOdataConstrNmL | _lhsOdataConstrNmL `seq` (True) ->
                                                                                                            (case (Map.empty) of
                                                                                                             { _lhsOdataConstrTagMp | _lhsOdataConstrTagMp `seq` (True) ->
                                                                                                             (case ((let sem_DataConstrs_Nil_5 :: T_DataConstrs_5 
                                                                                                                         sem_DataConstrs_Nil_5  =
                                                                                                                             (\ _lhsIfinTyKiGam
                                                                                                                                _lhsIpatTyVarMp
                                                                                                                                _lhsIpatValGam
                                                                                                                                _lhsItvKiVarMp ->
                                                                                                                                  _lhsIfinTyKiGam `seq`
                                                                                                                                  (_lhsIpatTyVarMp `seq`
                                                                                                                                   (_lhsIpatValGam `seq`
                                                                                                                                    (_lhsItvKiVarMp `seq`
                                                                                                                                     ((case (emptyGam) of
                                                                                                                                       { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                                                       (case (emptyGam) of
                                                                                                                                        { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                                                        (case (Map.empty) of
                                                                                                                                         { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                         (case (Map.empty) of
                                                                                                                                          { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                          (case (_lhsIpatTyVarMp) of
                                                                                                                                           { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                           (case (_lhsIpatValGam) of
                                                                                                                                            { _lhsOpatValGam | _lhsOpatValGam `seq` (True) ->
                                                                                                                                            (case ((let sem_DataConstrs_Nil_6 :: T_DataConstrs_6 
                                                                                                                                                        sem_DataConstrs_Nil_6  =
                                                                                                                                                            (\ _lhsIchrEvidBindMp
                                                                                                                                                               _lhsIchrScopeBindMp
                                                                                                                                                               _lhsIchrStore
                                                                                                                                                               _lhsIclDfGam
                                                                                                                                                               _lhsIclGam
                                                                                                                                                               _lhsIfinKiVarMp
                                                                                                                                                               _lhsIfinTyVarMp
                                                                                                                                                               _lhsIisNewtype
                                                                                                                                                               _lhsIkiGam
                                                                                                                                                               _lhsIlexLev
                                                                                                                                                               _lhsImoduleNm
                                                                                                                                                               _lhsIopts
                                                                                                                                                               _lhsIpredScope
                                                                                                                                                               _lhsIrangeMp
                                                                                                                                                               _lhsItyKiGlobFreeTvarS
                                                                                                                                                               _lhsItyTyGlobFreeTvarS
                                                                                                                                                               _lhsItyTyTySigFreeTvarS
                                                                                                                                                               _lhsIvalGam
                                                                                                                                                               _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                 _lhsIchrEvidBindMp `seq`
                                                                                                                                                                 (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                  (_lhsIchrStore `seq`
                                                                                                                                                                   (_lhsIclDfGam `seq`
                                                                                                                                                                    (_lhsIclGam `seq`
                                                                                                                                                                     (_lhsIfinKiVarMp `seq`
                                                                                                                                                                      (_lhsIfinTyVarMp `seq`
                                                                                                                                                                       (_lhsIisNewtype `seq`
                                                                                                                                                                        (_lhsIkiGam `seq`
                                                                                                                                                                         (_lhsIlexLev `seq`
                                                                                                                                                                          (_lhsImoduleNm `seq`
                                                                                                                                                                           (_lhsIopts `seq`
                                                                                                                                                                            (_lhsIpredScope `seq`
                                                                                                                                                                             (_lhsIrangeMp `seq`
                                                                                                                                                                              (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                               (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                 (_lhsIvalGam `seq`
                                                                                                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                   ((case (Seq.empty) of
                                                                                                                                                                                     { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                     (case ([]) of
                                                                                                                                                                                      { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                      (case (Seq.empty) of
                                                                                                                                                                                       { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                        { _lhsOffeCBindL | _lhsOffeCBindL `seq` (True) ->
                                                                                                                                                                                        (case ([]) of
                                                                                                                                                                                         { _lhsOffiCBindL | _lhsOffiCBindL `seq` (True) ->
                                                                                                                                                                                         (case (Map.empty) of
                                                                                                                                                                                          { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                          (case (emptyVarMp) of
                                                                                                                                                                                           { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                           (case (empty) of
                                                                                                                                                                                            { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                            (case ([]) of
                                                                                                                                                                                             { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                             ( _lhsOallErrSq,_lhsOcbindL,_lhsOerrSq,_lhsOffeCBindL,_lhsOffiCBindL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) })))))))))))))))))))))
                                                                                                                                                    in  sem_DataConstrs_Nil_6)) of
                                                                                                                                             { ( sem_DataConstrs_6) | True ->
                                                                                                                                             ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOpatTyVarMp,_lhsOpatValGam,sem_DataConstrs_6) }) }) }) }) }) }) }))))))
                                                                                                                     in  sem_DataConstrs_Nil_5)) of
                                                                                                              { ( sem_DataConstrs_5) | True ->
                                                                                                              ( _lhsOdataAltForNewType,_lhsOdataConstrNmL,_lhsOdataConstrTagMp,sem_DataConstrs_5) }) }) }) })))))
                                                                                          in  sem_DataConstrs_Nil_4)) of
                                                                                   { ( sem_DataConstrs_4) | True ->
                                                                                   ( _lhsOgathMaxArity,_lhsOintlTyKiGam,_lhsOkiVarMp,sem_DataConstrs_4) }) }) }) }))))
                                                                in  sem_DataConstrs_Nil_3)) of
                                                         { ( sem_DataConstrs_3) | True ->
                                                         ( _lhsOdataAltTyL,_lhsOpolVarMp,sem_DataConstrs_3) }) }) })))))))
                                    in  sem_DataConstrs_Nil_2)) of
                             { ( sem_DataConstrs_2) | True ->
                             ( _lhsOgUniq,sem_DataConstrs_2) }) })))
             in  sem_DataConstrs_Nil_1)) of
      { ( sem_DataConstrs_1) | True ->
      ( _lhsOrange,sem_DataConstrs_1) }) })


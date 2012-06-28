


module EH101.EH.MainAG_DataField where

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

-- DataField ---------------------------------------------------
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
   alternatives:
      alternative Field:
         child hsrange        : {Range}
         child mbLabels       : {Maybe [HsName]}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 2:
            local nrFieldsForLabel : {Int}
            local fldTyL      : {FldTyL}
         visit 3:
            intra fldTyL      : {FldTyL}
            intra nrFieldsForLabel : {Int}
         visit 4:
            local fldSelGam   : _
            local fldUpdGam   : _
            intra fldTyL      : {FldTyL}
            intra nrFieldsForLabel : {Int}
-}
sem_DataField_Field :: Range ->
                       (Maybe [HsName]) ->
                       T_TyExpr  ->
                       T_DataField 

sem_DataField_Field hsrange_ mbLabels_ tyExpr_  | hsrange_ `seq` (mbLabels_ `seq` (tyExpr_ `seq` (True))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_DataField_Field_1 :: T_DataField_1 
                       sem_DataField_Field_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                  (case (tyExpr_1 _tyExprOgUniq ) of
                                   { ( _tyExprIgUniq,tyExpr_2) | True ->
                                       (case (_tyExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_DataField_Field_2 :: T_DataField_2 
                                                    sem_DataField_Field_2  =
                                                        (\ _lhsIknPolCtx
                                                           _lhsIpolGam
                                                           _lhsIpolVarMp
                                                           _lhsItyGam ->
                                                             _lhsIknPolCtx `seq`
                                                             (_lhsIpolGam `seq`
                                                              (_lhsIpolVarMp `seq`
                                                               (_lhsItyGam `seq`
                                                                ((case (maybe 1 length mbLabels_) of
                                                                  { _nrFieldsForLabel | _nrFieldsForLabel `seq` (True) ->
                                                                  (case (_lhsItyGam) of
                                                                   { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                                   (case (tyExpr_2 _tyExprOtyGam ) of
                                                                    { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                        (case (_lhsIknPolCtx) of
                                                                         { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                         (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                          { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                              (case (_lhsIpolVarMp) of
                                                                               { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                               (case (_lhsIpolGam) of
                                                                                { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                 { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                     (case (replicate _nrFieldsForLabel
                                                                                                                                                      ( DataConFldAnnInfo
                                                                                                  (maybe Strictness_NonStrict id _tyExprImbStrictness)
                                                                                              )) of
                                                                                      { _lhsOfldAnnL | _lhsOfldAnnL `seq` (True) ->
                                                                                      (case (case mbLabels_ of
                                                                                               Just ls -> zipWith (\l t -> (Just l,t)) ls (repeat _tyExprIty)
                                                                                               _       -> [(Nothing,_tyExprIty)]) of
                                                                                       { _fldTyL | _fldTyL `seq` (True) ->
                                                                                       (case (_fldTyL) of
                                                                                        { _lhsOfldTyL | _lhsOfldTyL `seq` (True) ->
                                                                                        (case (_tyExprIpolVarMp) of
                                                                                         { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                         (case (_tyExprItyGam) of
                                                                                          { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                          (case ((let sem_DataField_Field_3 :: T_DataField_3 
                                                                                                      sem_DataField_Field_3  =
                                                                                                          (\ _lhsItyKiGam ->
                                                                                                               _lhsItyKiGam `seq`
                                                                                                               ((case (_lhsItyKiGam) of
                                                                                                                 { _tyExprOtyKiGam | _tyExprOtyKiGam `seq` (True) ->
                                                                                                                 (case (tyExpr_5 _tyExprOtyKiGam ) of
                                                                                                                  { ( _tyExprIintlTyKiGam,_tyExprItyKiGam,tyExpr_6) | True ->
                                                                                                                      (case (_tyExprIintlTyKiGam) of
                                                                                                                       { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                       (case (_tyExprItyKiGam) of
                                                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                        (case ((let sem_DataField_Field_4 :: T_DataField_4 
                                                                                                                                    sem_DataField_Field_4  =
                                                                                                                                        (\ _lhsIdataTy
                                                                                                                                           _lhsIkiVarMp ->
                                                                                                                                             _lhsIdataTy `seq`
                                                                                                                                             (_lhsIkiVarMp `seq`
                                                                                                                                              ((case (assocLToGam [ (l,ValGamInfo $ [_lhsIdataTy] `mkArrow` t) | (Just l,t) <- _fldTyL ]) of
                                                                                                                                                { _fldSelGam | _fldSelGam `seq` (True) ->
                                                                                                                                                (case (_fldSelGam) of
                                                                                                                                                 { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                                                                 (case (let mk t = [t,fr _lhsIdataTy] `mkArrow` _lhsIdataTy
                                                                                                                                                                 where fv = tyFtvMp t
                                                                                                                                                                       fr dt = dt
                                                                                                                                                        in  assocLToGam [ (hsnFldUpd l,ValGamInfo $ mk t) | (Just l,t) <- _fldTyL ]) of
                                                                                                                                                  { _fldUpdGam | _fldUpdGam `seq` (True) ->
                                                                                                                                                  (case (_fldUpdGam) of
                                                                                                                                                   { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                                                                   (case (Map.empty) of
                                                                                                                                                    { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                    (case (Map.empty) of
                                                                                                                                                     { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                     (case (_lhsIkiVarMp) of
                                                                                                                                                      { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                      (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                       { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                           (case (replicate _nrFieldsForLabel _tyExprIki) of
                                                                                                                                                            { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                                                                            (case (_tyExprIkiVarMp) of
                                                                                                                                                             { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                             (case ((let sem_DataField_Field_5 :: T_DataField_5 
                                                                                                                                                                         sem_DataField_Field_5  =
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
                                                                                                                                                                                                     ((case (_lhsIclGam) of
                                                                                                                                                                                                       { _tyExprOclGam | _tyExprOclGam `seq` (True) ->
                                                                                                                                                                                                       (case (tyExpr_7 _tyExprOclGam ) of
                                                                                                                                                                                                        { ( _tyExprIevTy,tyExpr_8) | True ->
                                                                                                                                                                                                            (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                             { _tyExprOvalTyGlobFreeTvarS | _tyExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                             (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                              { _tyExprOtyTyTySigFreeTvarS | _tyExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                              (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                               { _tyExprOtyTyGlobFreeTvarS | _tyExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                               (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                { _tyExprOtyKiGlobFreeTvarS | _tyExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                (case (_lhsIopts) of
                                                                                                                                                                                                                 { _tyExprOopts | _tyExprOopts `seq` (True) ->
                                                                                                                                                                                                                 (case (_lhsImoduleNm) of
                                                                                                                                                                                                                  { _tyExprOmoduleNm | _tyExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                  (case (_lhsIkiGam) of
                                                                                                                                                                                                                   { _tyExprOkiGam | _tyExprOkiGam `seq` (True) ->
                                                                                                                                                                                                                   (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                    { _tyExprOfinTyVarMp | _tyExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                     { _tyExprOfinTyKiGam | _tyExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                      { _tyExprOfinKiVarMp | _tyExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                      (case (tyExpr_8 _tyExprOfinKiVarMp _tyExprOfinTyKiGam _tyExprOfinTyVarMp _tyExprOkiGam _tyExprOmoduleNm _tyExprOopts _tyExprOtyKiGlobFreeTvarS _tyExprOtyTyGlobFreeTvarS _tyExprOtyTyTySigFreeTvarS _tyExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                       { ( _tyExprIallErrSq,_tyExprIappArgPPL,_tyExprIappFunNm,_tyExprIappFunPP,_tyExprIclMissNmS,_tyExprIclNmS,_tyExprIerrSq,_tyExprIgathMentrelFilterMp,_tyExprIpp,_tyExprItyWildL) | True ->
                                                                                                                                                                                                                           (case (_tyExprIallErrSq) of
                                                                                                                                                                                                                            { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                            (case (_tyExprIerrSq) of
                                                                                                                                                                                                                             { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                             (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                              (case (emptyVarMp) of
                                                                                                                                                                                                                               { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                               (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                                { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                (case (case mbLabels_ of
                                                                                                                                                                                                                                         Just l  -> ppCommas' l >#< "::" >#< _tyExprIpp
                                                                                                                                                                                                                                         Nothing -> _tyExprIpp) of
                                                                                                                                                                                                                                 { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatTyVarMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))))))))))
                                                                                                                                                                     in  sem_DataField_Field_5)) of
                                                                                                                                                              { ( sem_DataField_5) | True ->
                                                                                                                                                              ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOkiL,_lhsOkiVarMp,sem_DataField_5) }) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                                in  sem_DataField_Field_4)) of
                                                                                                                         { ( sem_DataField_4) | True ->
                                                                                                                         ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_DataField_4) }) }) }) }) })))
                                                                                                  in  sem_DataField_Field_3)) of
                                                                                           { ( sem_DataField_3) | True ->
                                                                                           ( _lhsOfldAnnL,_lhsOfldTyL,_lhsOpolVarMp,_lhsOtyGam,sem_DataField_3) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))
                                                in  sem_DataField_Field_2)) of
                                         { ( sem_DataField_2) | True ->
                                         ( _lhsOgUniq,sem_DataField_2) }) }) }) })))
                   in  sem_DataField_Field_1)) of
            { ( sem_DataField_1) | True ->
            ( _lhsOrange,sem_DataField_1) }) }) }) })


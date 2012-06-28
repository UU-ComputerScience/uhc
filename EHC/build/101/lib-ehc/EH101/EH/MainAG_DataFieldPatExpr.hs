


module EH101.EH.MainAG_DataFieldPatExpr where

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

-- DataFieldPatExpr --------------------------------------------
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
         tyKiGlobFreeTvarS    : TyVarIdS
      chained attributes:
         kiVarMp              : VarMp
         polVarMp             : VarMp
         tyGam                : TyGam
         tyKiGam              : TyKiGam
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
         fiOpts               : FIOpts
         knTy                 : Ty
         tySigGam             : ValGam
      chained attributes:
         patTyVarMp           : VarMp
         valGam               : ValGam
   visit 6:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         tvKiVarMp            : VarMp
         tyTyGlobFreeTvarS    : TyVarIdS
         valTyGlobFreeTvarS   : TyVarIdS
      chained attribute:
         tyVarMp              : VarMp
      synthesized attributes:
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
         scopeGam             : ScopeGam
         ty                   : Ty
   visit 7:
      inherited attributes:
         ceParentNm           : HsName
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
         cbindL               : CBindL
         dti                  : DataTagInfo
         errSq                : ErrSq
         fldL                 : [HsName]
         fsRPatL              : FieldSplitL
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         patCRest             : CPatRest
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Con:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup17      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup17      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup17      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup17      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local tyvar       : _
            local knTyShape   : _
            local _tup16      : {(Ty,ErrL)}
            local gTy         : {Ty}
            local fo_         : {FIOut}
            intra _tup17      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            local ty          : {Ty}
            intra _tup16      : {(Ty,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra gTy         : {Ty}
         visit 7:
            local dgi         : {DataGamInfo}
            local dti         : _
            local pp          : _
            local nmErrs      : {ErrL}
            local gathMentrelFilterMp : _
            intra _tup16      : {(Ty,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra gTy         : {Ty}
      alternative Ext:
         child hsrange        : {Range}
         child dataFieldPatExpr : DataFieldPatExpr 
         child nm             : {HsName}
         child patExpr        : PatExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup19      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup19      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup19      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup19      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local knFldTy     : _
            local _tup18      : {(Ty,ErrL)}
            local gTy         : {Ty}
            local fo_         : {FIOut}
            intra _tup19      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            local ty          : {Ty}
            intra _tup18      : {(Ty,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra gTy         : {Ty}
         visit 7:
            local pp          : _
            local nmErrs      : {ErrL}
            local ioffset     : _
            local gathMentrelFilterMp : _
            intra _tup18      : {(Ty,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra gTy         : {Ty}
-}
sem_DataFieldPatExpr_Con :: Range ->
                            HsName ->
                            T_DataFieldPatExpr 

sem_DataFieldPatExpr_Con hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_DataFieldPatExpr_Con_1 :: T_DataFieldPatExpr_1 
                  sem_DataFieldPatExpr_Con_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )) of
                             { __tup17 | __tup17 `seq` (True) ->
                             (case (__tup17) of
                              { (_lhsOgUniq,_,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_DataFieldPatExpr_Con_2 :: T_DataFieldPatExpr_2 
                                          sem_DataFieldPatExpr_Con_2  =
                                              (\ _lhsIkiGam
                                                 _lhsIlexLev
                                                 _lhsIpredSameScopeCounter ->
                                                   _lhsIkiGam `seq`
                                                   (_lhsIlexLev `seq`
                                                    (_lhsIpredSameScopeCounter `seq`
                                                     ((case (_lhsIpredSameScopeCounter) of
                                                       { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                       (case ((let sem_DataFieldPatExpr_Con_3 :: T_DataFieldPatExpr_3 
                                                                   sem_DataFieldPatExpr_Con_3  =
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
                                                                                      (case (_lhsItyGam) of
                                                                                       { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                       (case (_lhsItyKiGam) of
                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                        (case ((let sem_DataFieldPatExpr_Con_4 :: T_DataFieldPatExpr_4 
                                                                                                    sem_DataFieldPatExpr_Con_4  =
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
                                                                                                                   (case ((let sem_DataFieldPatExpr_Con_5 :: T_DataFieldPatExpr_5 
                                                                                                                               sem_DataFieldPatExpr_Con_5  =
                                                                                                                                   (\ _lhsIfiOpts
                                                                                                                                      _lhsIknTy
                                                                                                                                      _lhsIpatTyVarMp
                                                                                                                                      _lhsItySigGam
                                                                                                                                      _lhsIvalGam ->
                                                                                                                                        _lhsIfiOpts `seq`
                                                                                                                                        (_lhsIknTy `seq`
                                                                                                                                         (_lhsIpatTyVarMp `seq`
                                                                                                                                          (_lhsItySigGam `seq`
                                                                                                                                           (_lhsIvalGam `seq`
                                                                                                                                            ((case (__tup17) of
                                                                                                                                              { (_,_,_lUniq2) | _lUniq2 `seq` (True) ->
                                                                                                                                              (case (__tup17) of
                                                                                                                                               { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                                                                                                               (case (defaultFIEnv
                                                                                                                                                          { feEHCOpts = _lhsIopts
                                                                                                                                                          , fePredScope = _lhsIpredScope
                                                                                                                                                          , feTyGam = _lhsItyGam
                                                                                                                                                          , fePolGam = _lhsIpolGam
                                                                                                                                                          , feRange = _range
                                                                                                                                                          }) of
                                                                                                                                                { _fe | _fe `seq` (True) ->
                                                                                                                                                (case (mkTyVar _lUniq) of
                                                                                                                                                 { _tyvar | _tyvar `seq` (True) ->
                                                                                                                                                 (case ([_lhsIknTy] `mkArrow` _tyvar) of
                                                                                                                                                  { _knTyShape | _knTyShape `seq` (True) ->
                                                                                                                                                  (case (valGamLookupTy (hsnUn nm_) _lhsIvalGam) of
                                                                                                                                                   { __tup16 | __tup16 `seq` (True) ->
                                                                                                                                                   (case (__tup16) of
                                                                                                                                                    { (_gTy,_) | _gTy `seq` (True) ->
                                                                                                                                                    (case (fitsIn _lhsIfiOpts _fe _lUniq2 _lhsIpatTyVarMp _gTy _knTyShape) of
                                                                                                                                                     { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                     (case (foVarMp _fo_ `varUpd` _lhsIpatTyVarMp) of
                                                                                                                                                      { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                      (case (_lhsIvalGam) of
                                                                                                                                                       { _lhsOvalGam | _lhsOvalGam `seq` (True) ->
                                                                                                                                                       (case ((let sem_DataFieldPatExpr_Con_6 :: T_DataFieldPatExpr_6 
                                                                                                                                                                   sem_DataFieldPatExpr_Con_6  =
                                                                                                                                                                       (\ _lhsIchrStore
                                                                                                                                                                          _lhsIclDfGam
                                                                                                                                                                          _lhsItvKiVarMp
                                                                                                                                                                          _lhsItyTyGlobFreeTvarS
                                                                                                                                                                          _lhsItyVarMp
                                                                                                                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                            _lhsIchrStore `seq`
                                                                                                                                                                            (_lhsIclDfGam `seq`
                                                                                                                                                                             (_lhsItvKiVarMp `seq`
                                                                                                                                                                              (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                               (_lhsItyVarMp `seq`
                                                                                                                                                                                (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                 ((case (Map.empty) of
                                                                                                                                                                                   { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                   (case (Map.empty) of
                                                                                                                                                                                    { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                    (case (emptyGam) of
                                                                                                                                                                                     { _lhsOscopeGam | _lhsOscopeGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIknTy) of
                                                                                                                                                                                      { _ty | _ty `seq` (True) ->
                                                                                                                                                                                      (case (_ty) of
                                                                                                                                                                                       { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                       (case (_lhsItyVarMp) of
                                                                                                                                                                                        { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                        (case ((let sem_DataFieldPatExpr_Con_7 :: T_DataFieldPatExpr_7 
                                                                                                                                                                                                    sem_DataFieldPatExpr_Con_7  =
                                                                                                                                                                                                        (\ _lhsIcSubst
                                                                                                                                                                                                           _lhsIceParentNm
                                                                                                                                                                                                           _lhsIchrEvidBindMp
                                                                                                                                                                                                           _lhsIchrScopeBindMp
                                                                                                                                                                                                           _lhsIfinTyVarMp
                                                                                                                                                                                                           _lhsIfinValGam
                                                                                                                                                                                                           _lhsImoduleNm
                                                                                                                                                                                                           _lhsIrangeMp ->
                                                                                                                                                                                                             _lhsIcSubst `seq`
                                                                                                                                                                                                             (_lhsIceParentNm `seq`
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
                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                        { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                        (case (let (_,t,_) = valGamTyOfDataCon nm_ _lhsIvalGam
                                                                                                                                                                                                                               in  panicJust "DataFieldPatExpr.dgi" $ dataGamDgiOfTy t _lhsIdataGam) of
                                                                                                                                                                                                                         { _dgi | _dgi `seq` (True) ->
                                                                                                                                                                                                                         (case (dgiDtiOfCon nm_ _dgi) of
                                                                                                                                                                                                                          { _dti | _dti `seq` (True) ->
                                                                                                                                                                                                                          (case (_dti) of
                                                                                                                                                                                                                           { _lhsOdti | _lhsOdti `seq` (True) ->
                                                                                                                                                                                                                           (case (pp nm_) of
                                                                                                                                                                                                                            { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                            (case (__tup16) of
                                                                                                                                                                                                                             { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                             (case (rngLift _range mkNestErr' _pp [Seq.fromList _nmErrs, foErrSq _fo_]) of
                                                                                                                                                                                                                              { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                               { _lhsOfldL | _lhsOfldL `seq` (True) ->
                                                                                                                                                                                                                               (case ([]) of
                                                                                                                                                                                                                                { _lhsOfsRPatL | _lhsOfsRPatL `seq` (True) ->
                                                                                                                                                                                                                                (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Val (hsnUn nm_)
                                                                                                                                                                                                                                       `mentrelFilterMpUnion`
                                                                                                                                                                                                                                       tyUsedNames _lhsImoduleNm _gTy) of
                                                                                                                                                                                                                                 { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                 (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                  { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                  (case (emptyVarMp) of
                                                                                                                                                                                                                                   { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                   (case (CPatRest_Empty) of
                                                                                                                                                                                                                                    { _lhsOpatCRest | _lhsOpatCRest `seq` (True) ->
                                                                                                                                                                                                                                    (case (_pp) of
                                                                                                                                                                                                                                     { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                     (case ([]) of
                                                                                                                                                                                                                                      { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                      ( _lhsOallErrSq,_lhsOcSubst,_lhsOcbindL,_lhsOdti,_lhsOerrSq,_lhsOfldL,_lhsOfsRPatL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatCRest,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                in  sem_DataFieldPatExpr_Con_7)) of
                                                                                                                                                                                         { ( sem_DataFieldPatExpr_7) | True ->
                                                                                                                                                                                         ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOscopeGam,_lhsOty,_lhsOtyVarMp,sem_DataFieldPatExpr_7) }) }) }) }) }) }) }))))))))
                                                                                                                                                               in  sem_DataFieldPatExpr_Con_6)) of
                                                                                                                                                        { ( sem_DataFieldPatExpr_6) | True ->
                                                                                                                                                        ( _lhsOpatTyVarMp,_lhsOvalGam,sem_DataFieldPatExpr_6) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                           in  sem_DataFieldPatExpr_Con_5)) of
                                                                                                                    { ( sem_DataFieldPatExpr_5) | True ->
                                                                                                                    ( _lhsOchrInstDeclSq,sem_DataFieldPatExpr_5) }) })))))))
                                                                                                in  sem_DataFieldPatExpr_Con_4)) of
                                                                                         { ( sem_DataFieldPatExpr_4) | True ->
                                                                                         ( _lhsOkiVarMp,_lhsOpolVarMp,_lhsOtyGam,_lhsOtyKiGam,sem_DataFieldPatExpr_4) }) }) }) }) }))))))))))
                                                               in  sem_DataFieldPatExpr_Con_3)) of
                                                        { ( sem_DataFieldPatExpr_3) | True ->
                                                        ( _lhsOpredSameScopeCounter,sem_DataFieldPatExpr_3) }) })))))
                                      in  sem_DataFieldPatExpr_Con_2)) of
                               { ( sem_DataFieldPatExpr_2) | True ->
                               ( _lhsOgUniq,sem_DataFieldPatExpr_2) }) }) })))
              in  sem_DataFieldPatExpr_Con_1)) of
       { ( sem_DataFieldPatExpr_1) | True ->
       ( _lhsOrange,sem_DataFieldPatExpr_1) }) }) })

sem_DataFieldPatExpr_Ext :: Range ->
                            T_DataFieldPatExpr  ->
                            HsName ->
                            T_PatExpr  ->
                            T_DataFieldPatExpr 

sem_DataFieldPatExpr_Ext hsrange_ dataFieldPatExpr_ nm_ patExpr_  | hsrange_ `seq` (dataFieldPatExpr_ `seq` (nm_ `seq` (patExpr_ `seq` (True)))) =
    (case (patExpr_ ) of
     { ( _patExprIrange,patExpr_1) | True ->
         (case (dataFieldPatExpr_ ) of
          { ( _dataFieldPatExprIrange,dataFieldPatExpr_1) | True ->
              (case (rangeUnions [hsrange_, _dataFieldPatExprIrange
                                                          , _patExprIrange
                                                                         ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_DataFieldPatExpr_Ext_1 :: T_DataFieldPatExpr_1 
                            sem_DataFieldPatExpr_Ext_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )) of
                                       { __tup19 | __tup19 `seq` (True) ->
                                       (case (__tup19) of
                                        { (_dataFieldPatExprOgUniq,_,_) | _dataFieldPatExprOgUniq `seq` (True) ->
                                        (case (dataFieldPatExpr_1 _dataFieldPatExprOgUniq ) of
                                         { ( _dataFieldPatExprIgUniq,dataFieldPatExpr_2) | True ->
                                             (case (_dataFieldPatExprIgUniq) of
                                              { _patExprOgUniq | _patExprOgUniq `seq` (True) ->
                                              (case (patExpr_1 _patExprOgUniq ) of
                                               { ( _patExprIgUniq,patExpr_2) | True ->
                                                   (case (_patExprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_DataFieldPatExpr_Ext_2 :: T_DataFieldPatExpr_2 
                                                                sem_DataFieldPatExpr_Ext_2  =
                                                                    (\ _lhsIkiGam
                                                                       _lhsIlexLev
                                                                       _lhsIpredSameScopeCounter ->
                                                                         _lhsIkiGam `seq`
                                                                         (_lhsIlexLev `seq`
                                                                          (_lhsIpredSameScopeCounter `seq`
                                                                           ((case (_lhsIpredSameScopeCounter) of
                                                                             { _dataFieldPatExprOpredSameScopeCounter | _dataFieldPatExprOpredSameScopeCounter `seq` (True) ->
                                                                             (case (_lhsIlexLev) of
                                                                              { _dataFieldPatExprOlexLev | _dataFieldPatExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _dataFieldPatExprOkiGam | _dataFieldPatExprOkiGam `seq` (True) ->
                                                                               (case (dataFieldPatExpr_2 _dataFieldPatExprOkiGam _dataFieldPatExprOlexLev _dataFieldPatExprOpredSameScopeCounter ) of
                                                                                { ( _dataFieldPatExprIpredSameScopeCounter,dataFieldPatExpr_3) | True ->
                                                                                    (case (_dataFieldPatExprIpredSameScopeCounter) of
                                                                                     { _patExprOpredSameScopeCounter | _patExprOpredSameScopeCounter `seq` (True) ->
                                                                                     (case (_lhsIlexLev) of
                                                                                      { _patExprOlexLev | _patExprOlexLev `seq` (True) ->
                                                                                      (case (_lhsIkiGam) of
                                                                                       { _patExprOkiGam | _patExprOkiGam `seq` (True) ->
                                                                                       (case (patExpr_2 _patExprOkiGam _patExprOlexLev _patExprOpredSameScopeCounter ) of
                                                                                        { ( _patExprIpredSameScopeCounter,patExpr_3) | True ->
                                                                                            (case (_patExprIpredSameScopeCounter) of
                                                                                             { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                                             (case ((let sem_DataFieldPatExpr_Ext_3 :: T_DataFieldPatExpr_3 
                                                                                                         sem_DataFieldPatExpr_Ext_3  =
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
                                                                                                                           (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                            { _dataFieldPatExprOtyKiGlobFreeTvarS | _dataFieldPatExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                            (case (_lhsItyKiGam) of
                                                                                                                             { _dataFieldPatExprOtyKiGam | _dataFieldPatExprOtyKiGam `seq` (True) ->
                                                                                                                             (case (_lhsItyGam) of
                                                                                                                              { _dataFieldPatExprOtyGam | _dataFieldPatExprOtyGam `seq` (True) ->
                                                                                                                              (case (_lhsIpredScope) of
                                                                                                                               { _dataFieldPatExprOpredScope | _dataFieldPatExprOpredScope `seq` (True) ->
                                                                                                                               (case (_lhsIpolVarMp) of
                                                                                                                                { _dataFieldPatExprOpolVarMp | _dataFieldPatExprOpolVarMp `seq` (True) ->
                                                                                                                                (case (_lhsIpolGam) of
                                                                                                                                 { _dataFieldPatExprOpolGam | _dataFieldPatExprOpolGam `seq` (True) ->
                                                                                                                                 (case (_lhsIopts) of
                                                                                                                                  { _dataFieldPatExprOopts | _dataFieldPatExprOopts `seq` (True) ->
                                                                                                                                  (case (_lhsIkiVarMp) of
                                                                                                                                   { _dataFieldPatExprOkiVarMp | _dataFieldPatExprOkiVarMp `seq` (True) ->
                                                                                                                                   (case (dataFieldPatExpr_3 _dataFieldPatExprOkiVarMp _dataFieldPatExprOopts _dataFieldPatExprOpolGam _dataFieldPatExprOpolVarMp _dataFieldPatExprOpredScope _dataFieldPatExprOtyGam _dataFieldPatExprOtyKiGam _dataFieldPatExprOtyKiGlobFreeTvarS ) of
                                                                                                                                    { ( _dataFieldPatExprIkiVarMp,_dataFieldPatExprIpolVarMp,_dataFieldPatExprItyGam,_dataFieldPatExprItyKiGam,dataFieldPatExpr_4) | True ->
                                                                                                                                        (case (_dataFieldPatExprItyKiGam) of
                                                                                                                                         { _patExprOtyKiGam | _patExprOtyKiGam `seq` (True) ->
                                                                                                                                         (case (_dataFieldPatExprItyGam) of
                                                                                                                                          { _patExprOtyGam | _patExprOtyGam `seq` (True) ->
                                                                                                                                          (case (_lhsIpredScope) of
                                                                                                                                           { _patExprOpredScope | _patExprOpredScope `seq` (True) ->
                                                                                                                                           (case (_dataFieldPatExprIpolVarMp) of
                                                                                                                                            { _patExprOpolVarMp | _patExprOpolVarMp `seq` (True) ->
                                                                                                                                            (case (_lhsIpolGam) of
                                                                                                                                             { _patExprOpolGam | _patExprOpolGam `seq` (True) ->
                                                                                                                                             (case (_lhsIopts) of
                                                                                                                                              { _patExprOopts | _patExprOopts `seq` (True) ->
                                                                                                                                              (case (_dataFieldPatExprIkiVarMp) of
                                                                                                                                               { _patExprOkiVarMp | _patExprOkiVarMp `seq` (True) ->
                                                                                                                                               (case (patExpr_3 _patExprOkiVarMp _patExprOopts _patExprOpolGam _patExprOpolVarMp _patExprOpredScope _patExprOtyGam _patExprOtyKiGam _patExprOtyKiGlobFreeTvarS ) of
                                                                                                                                                { ( _patExprIkiVarMp,_patExprIpolVarMp,_patExprItyGam,_patExprItyKiGam,patExpr_4) | True ->
                                                                                                                                                    (case (_patExprIkiVarMp) of
                                                                                                                                                     { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                     (case (_patExprIpolVarMp) of
                                                                                                                                                      { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                      (case (_patExprItyGam) of
                                                                                                                                                       { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                                                                                       (case (_patExprItyKiGam) of
                                                                                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                        (case ((let sem_DataFieldPatExpr_Ext_4 :: T_DataFieldPatExpr_4 
                                                                                                                                                                    sem_DataFieldPatExpr_Ext_4  =
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
                                                                                                                                                                                   { _patExprOtyTyTySigFreeTvarS | _patExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                   (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                    { _patExprOfinTyKiGam | _patExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                     { _patExprOfinKiVarMp | _patExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIdataGam) of
                                                                                                                                                                                      { _patExprOdataGam | _patExprOdataGam `seq` (True) ->
                                                                                                                                                                                      (case (_lhsIclGam) of
                                                                                                                                                                                       { _patExprOclGam | _patExprOclGam `seq` (True) ->
                                                                                                                                                                                       (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                        { _dataFieldPatExprOtyTyTySigFreeTvarS | _dataFieldPatExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                        (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                         { _dataFieldPatExprOfinTyKiGam | _dataFieldPatExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                         (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                          { _dataFieldPatExprOfinKiVarMp | _dataFieldPatExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                          (case (_lhsIdataGam) of
                                                                                                                                                                                           { _dataFieldPatExprOdataGam | _dataFieldPatExprOdataGam `seq` (True) ->
                                                                                                                                                                                           (case (_lhsIclGam) of
                                                                                                                                                                                            { _dataFieldPatExprOclGam | _dataFieldPatExprOclGam `seq` (True) ->
                                                                                                                                                                                            (case (patExpr_4 _patExprOclGam _patExprOdataGam _patExprOfinKiVarMp _patExprOfinTyKiGam _patExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                             { ( _patExprIchrInstDeclSq,_patExprItopNm,patExpr_5) | True ->
                                                                                                                                                                                                 (case (dataFieldPatExpr_4 _dataFieldPatExprOclGam _dataFieldPatExprOdataGam _dataFieldPatExprOfinKiVarMp _dataFieldPatExprOfinTyKiGam _dataFieldPatExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                                  { ( _dataFieldPatExprIchrInstDeclSq,dataFieldPatExpr_5) | True ->
                                                                                                                                                                                                      (case (_dataFieldPatExprIchrInstDeclSq `Seq.union` _patExprIchrInstDeclSq) of
                                                                                                                                                                                                       { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                       (case ((let sem_DataFieldPatExpr_Ext_5 :: T_DataFieldPatExpr_5 
                                                                                                                                                                                                                   sem_DataFieldPatExpr_Ext_5  =
                                                                                                                                                                                                                       (\ _lhsIfiOpts
                                                                                                                                                                                                                          _lhsIknTy
                                                                                                                                                                                                                          _lhsIpatTyVarMp
                                                                                                                                                                                                                          _lhsItySigGam
                                                                                                                                                                                                                          _lhsIvalGam ->
                                                                                                                                                                                                                            _lhsIfiOpts `seq`
                                                                                                                                                                                                                            (_lhsIknTy `seq`
                                                                                                                                                                                                                             (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                              (_lhsItySigGam `seq`
                                                                                                                                                                                                                               (_lhsIvalGam `seq`
                                                                                                                                                                                                                                ((case (_lhsIvalGam) of
                                                                                                                                                                                                                                  { _dataFieldPatExprOvalGam | _dataFieldPatExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                  (case (_lhsItySigGam) of
                                                                                                                                                                                                                                   { _dataFieldPatExprOtySigGam | _dataFieldPatExprOtySigGam `seq` (True) ->
                                                                                                                                                                                                                                   (case (_lhsIknTy) of
                                                                                                                                                                                                                                    { _dataFieldPatExprOknTy | _dataFieldPatExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                    (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                     { _dataFieldPatExprOfiOpts | _dataFieldPatExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                     (case (__tup19) of
                                                                                                                                                                                                                                      { (_,_,_lUniq2) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                                      (case (__tup19) of
                                                                                                                                                                                                                                       { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                       (case (defaultFIEnv
                                                                                                                                                                                                                                                  { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                                                  , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                                                  , feTyGam = _lhsItyGam
                                                                                                                                                                                                                                                  , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                                  , feRange = _range
                                                                                                                                                                                                                                                  }) of
                                                                                                                                                                                                                                        { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                                                        (case (mkNewTyVar _lUniq) of
                                                                                                                                                                                                                                         { _knFldTy | _knFldTy `seq` (True) ->
                                                                                                                                                                                                                                         (case (valGamLookupTy nm_ _lhsIvalGam) of
                                                                                                                                                                                                                                          { __tup18 | __tup18 `seq` (True) ->
                                                                                                                                                                                                                                          (case (__tup18) of
                                                                                                                                                                                                                                           { (_gTy,_) | _gTy `seq` (True) ->
                                                                                                                                                                                                                                           (case (fitsIn _lhsIfiOpts _fe _lUniq2 _lhsIpatTyVarMp _gTy ([_lhsIknTy] `mkArrow` _knFldTy)) of
                                                                                                                                                                                                                                            { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                                                            (case (foVarMp _fo_ `varUpd` _lhsIpatTyVarMp) of
                                                                                                                                                                                                                                             { _dataFieldPatExprOpatTyVarMp | _dataFieldPatExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                             (case (dataFieldPatExpr_5 _dataFieldPatExprOfiOpts _dataFieldPatExprOknTy _dataFieldPatExprOpatTyVarMp _dataFieldPatExprOtySigGam _dataFieldPatExprOvalGam ) of
                                                                                                                                                                                                                                              { ( _dataFieldPatExprIpatTyVarMp,_dataFieldPatExprIvalGam,dataFieldPatExpr_6) | True ->
                                                                                                                                                                                                                                                  (case (_dataFieldPatExprIvalGam) of
                                                                                                                                                                                                                                                   { _patExprOvalGam | _patExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                   (case (_lhsItySigGam) of
                                                                                                                                                                                                                                                    { _patExprOtySigGam | _patExprOtySigGam `seq` (True) ->
                                                                                                                                                                                                                                                    (case (_dataFieldPatExprIpatTyVarMp) of
                                                                                                                                                                                                                                                     { _patExprOpatTyVarMp | _patExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                     (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                      { _patExprOfiOpts | _patExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                      (case (_knFldTy) of
                                                                                                                                                                                                                                                       { _patExprOknTy | _patExprOknTy `seq` (True) ->
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
                                                                                                                                                                                                                                                                    (case (_patExprIvalGam) of
                                                                                                                                                                                                                                                                     { _lhsOvalGam | _lhsOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                     (case ((let sem_DataFieldPatExpr_Ext_6 :: T_DataFieldPatExpr_6 
                                                                                                                                                                                                                                                                                 sem_DataFieldPatExpr_Ext_6  =
                                                                                                                                                                                                                                                                                     (\ _lhsIchrStore
                                                                                                                                                                                                                                                                                        _lhsIclDfGam
                                                                                                                                                                                                                                                                                        _lhsItvKiVarMp
                                                                                                                                                                                                                                                                                        _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                                        _lhsItyVarMp
                                                                                                                                                                                                                                                                                        _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                                          _lhsIchrStore `seq`
                                                                                                                                                                                                                                                                                          (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                                           (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                                            (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                             (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                                              (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                               ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                 { _patExprOvalTyGlobFreeTvarS | _patExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                 (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                  { _dataFieldPatExprOvalTyGlobFreeTvarS | _dataFieldPatExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                  (case (_lhsItyVarMp) of
                                                                                                                                                                                                                                                                                                   { _dataFieldPatExprOtyVarMp | _dataFieldPatExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                   (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                    { _dataFieldPatExprOtyTyGlobFreeTvarS | _dataFieldPatExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                    (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                     { _dataFieldPatExprOtvKiVarMp | _dataFieldPatExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                     (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                      { _dataFieldPatExprOclDfGam | _dataFieldPatExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                       { _dataFieldPatExprOchrStore | _dataFieldPatExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (dataFieldPatExpr_6 _dataFieldPatExprOchrStore _dataFieldPatExprOclDfGam _dataFieldPatExprOtvKiVarMp _dataFieldPatExprOtyTyGlobFreeTvarS _dataFieldPatExprOtyVarMp _dataFieldPatExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                        { ( _dataFieldPatExprIgathCnstrMp,_dataFieldPatExprIgathRangeMp,_dataFieldPatExprIscopeGam,_dataFieldPatExprIty,_dataFieldPatExprItyVarMp,dataFieldPatExpr_7) | True ->
                                                                                                                                                                                                                                                                                                            (case (_dataFieldPatExprItyVarMp) of
                                                                                                                                                                                                                                                                                                             { _patExprOtyVarMp | _patExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                              { _patExprOtyTyGlobFreeTvarS | _patExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                               { _patExprOtvKiVarMp | _patExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                                { _patExprOclDfGam | _patExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                                 { _patExprOchrStore | _patExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (patExpr_7 _patExprOchrStore _patExprOclDfGam _patExprOtvKiVarMp _patExprOtyTyGlobFreeTvarS _patExprOtyVarMp _patExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                  { ( _patExprIcpNm,_patExprIgathCnstrMp,_patExprIgathRangeMp,_patExprIscopeGam,_patExprIty,_patExprItyVarMp,patExpr_8) | True ->
                                                                                                                                                                                                                                                                                                                      (case (_dataFieldPatExprIgathCnstrMp `cnstrMpUnion` _patExprIgathCnstrMp) of
                                                                                                                                                                                                                                                                                                                       { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (_dataFieldPatExprIgathRangeMp `Map.union` _patExprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                                        { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case (_dataFieldPatExprIscopeGam `gamUnion` _patExprIscopeGam) of
                                                                                                                                                                                                                                                                                                                         { _lhsOscopeGam | _lhsOscopeGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (_dataFieldPatExprIty) of
                                                                                                                                                                                                                                                                                                                          { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                          (case (_ty) of
                                                                                                                                                                                                                                                                                                                           { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                           (case (_patExprItyVarMp) of
                                                                                                                                                                                                                                                                                                                            { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                            (case ((let sem_DataFieldPatExpr_Ext_7 :: T_DataFieldPatExpr_7 
                                                                                                                                                                                                                                                                                                                                        sem_DataFieldPatExpr_Ext_7  =
                                                                                                                                                                                                                                                                                                                                            (\ _lhsIcSubst
                                                                                                                                                                                                                                                                                                                                               _lhsIceParentNm
                                                                                                                                                                                                                                                                                                                                               _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                                                                                                               _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                                                                                                               _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                                                                               _lhsIfinValGam
                                                                                                                                                                                                                                                                                                                                               _lhsImoduleNm
                                                                                                                                                                                                                                                                                                                                               _lhsIrangeMp ->
                                                                                                                                                                                                                                                                                                                                                 _lhsIcSubst `seq`
                                                                                                                                                                                                                                                                                                                                                 (_lhsIceParentNm `seq`
                                                                                                                                                                                                                                                                                                                                                  (_lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                                                                                                                   (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                                                                                                                    (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                                                                                                     (_lhsIfinValGam `seq`
                                                                                                                                                                                                                                                                                                                                                      (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                                                                                                       (_lhsIrangeMp `seq`
                                                                                                                                                                                                                                                                                                                                                        ((case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                                                                          { _patExprOrangeMp | _patExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                          (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                           { _patExprOfinValGam | _patExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                           (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                            { _patExprOfinTyVarMp | _patExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                            (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                                                                             { _dataFieldPatExprOrangeMp | _dataFieldPatExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                             (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                              { _dataFieldPatExprOfinValGam | _dataFieldPatExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                              (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                               { _dataFieldPatExprOfinTyVarMp | _dataFieldPatExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                               (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                { _patExprOmoduleNm | _patExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                 { _patExprOchrScopeBindMp | _patExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                  { _patExprOchrEvidBindMp | _patExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIceParentNm) of
                                                                                                                                                                                                                                                                                                                                                                   { _patExprOceParentNm | _patExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                   (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                    { _dataFieldPatExprOchrScopeBindMp | _dataFieldPatExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                    (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                     { _dataFieldPatExprOchrEvidBindMp | _dataFieldPatExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                     (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                      { _dataFieldPatExprOcSubst | _dataFieldPatExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                      (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                       { _dataFieldPatExprOmoduleNm | _dataFieldPatExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                       (case (_lhsIceParentNm) of
                                                                                                                                                                                                                                                                                                                                                                        { _dataFieldPatExprOceParentNm | _dataFieldPatExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                        (case (dataFieldPatExpr_7 _dataFieldPatExprOcSubst _dataFieldPatExprOceParentNm _dataFieldPatExprOchrEvidBindMp _dataFieldPatExprOchrScopeBindMp _dataFieldPatExprOfinTyVarMp _dataFieldPatExprOfinValGam _dataFieldPatExprOmoduleNm _dataFieldPatExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                         { ( _dataFieldPatExprIallErrSq,_dataFieldPatExprIcSubst,_dataFieldPatExprIcbindL,_dataFieldPatExprIdti,_dataFieldPatExprIerrSq,_dataFieldPatExprIfldL,_dataFieldPatExprIfsRPatL,_dataFieldPatExprIgathMentrelFilterMp,_dataFieldPatExprIgathTvKiVarMp,_dataFieldPatExprIpatCRest,_dataFieldPatExprIpp,_dataFieldPatExprIppL) | True ->
                                                                                                                                                                                                                                                                                                                                                                             (case (_dataFieldPatExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                              { _patExprOcSubst | _patExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                              (case (patExpr_8 _patExprOcSubst _patExprOceParentNm _patExprOchrEvidBindMp _patExprOchrScopeBindMp _patExprOfinTyVarMp _patExprOfinValGam _patExprOmoduleNm _patExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                               { ( _patExprIallErrSq,_patExprIappArgPPL,_patExprIappFunNm,_patExprIappFunPP,_patExprIcSubst,_patExprIcbindL,_patExprIerrSq,_patExprIfsRPatL,_patExprIgathMentrelFilterMp,_patExprIgathTvKiVarMp,_patExprIisBang,_patExprIpatCRest,_patExprIpp,_patExprIrpat) | True ->
                                                                                                                                                                                                                                                                                                                                                                                   (case (_dataFieldPatExprIallErrSq `Seq.union` _patExprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                                    { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                    (case (_patExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                     { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                     (case (_dataFieldPatExprIcbindL ++ _patExprIcbindL) of
                                                                                                                                                                                                                                                                                                                                                                                      { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                      (case (_dataFieldPatExprIdti) of
                                                                                                                                                                                                                                                                                                                                                                                       { _lhsOdti | _lhsOdti `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                       (case (ppFld "=" Nothing nm_ (pp nm_) _patExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                        { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                        (case (__tup18) of
                                                                                                                                                                                                                                                                                                                                                                                         { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                         (case (Seq.unions [ _dataFieldPatExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                           , rngLift _range mkNestErr' _pp [_patExprIerrSq, Seq.fromList _nmErrs, foErrSq _fo_]
                                                                                                                                                                                                                                                                                                                                                                                                           ]) of
                                                                                                                                                                                                                                                                                                                                                                                          { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                          (case (nm_ : _dataFieldPatExprIfldL) of
                                                                                                                                                                                                                                                                                                                                                                                           { _lhsOfldL | _lhsOfldL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                           (case (dfiOffset $ panicJust "DataFieldPatExpr.Ext.ioffset" $ Map.lookup nm_ $ dtiFldMp _dataFieldPatExprIdti) of
                                                                                                                                                                                                                                                                                                                                                                                            { _ioffset | _ioffset `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                            (case ((FldKnownOffset nm_ _ioffset,_patExprIrpat) : _dataFieldPatExprIfsRPatL) of
                                                                                                                                                                                                                                                                                                                                                                                             { _lhsOfsRPatL | _lhsOfsRPatL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                             (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Val nm_
                                                                                                                                                                                                                                                                                                                                                                                                    `mentrelFilterMpUnion`
                                                                                                                                                                                                                                                                                                                                                                                                    tyUsedNames _lhsImoduleNm _gTy) of
                                                                                                                                                                                                                                                                                                                                                                                              { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                              (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                                               { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                               (case (_dataFieldPatExprIgathTvKiVarMp `varmpUnion` _patExprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                                { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                (case (_dataFieldPatExprIpatCRest) of
                                                                                                                                                                                                                                                                                                                                                                                                 { _lhsOpatCRest | _lhsOpatCRest `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                 (case (_dataFieldPatExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                  (case (_pp : _dataFieldPatExprIppL) of
                                                                                                                                                                                                                                                                                                                                                                                                   { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                   ( _lhsOallErrSq,_lhsOcSubst,_lhsOcbindL,_lhsOdti,_lhsOerrSq,_lhsOfldL,_lhsOfsRPatL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatCRest,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                                                                                                    in  sem_DataFieldPatExpr_Ext_7)) of
                                                                                                                                                                                                                                                                                                                             { ( sem_DataFieldPatExpr_7) | True ->
                                                                                                                                                                                                                                                                                                                             ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOscopeGam,_lhsOty,_lhsOtyVarMp,sem_DataFieldPatExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                                                                                                                             in  sem_DataFieldPatExpr_Ext_6)) of
                                                                                                                                                                                                                                                                      { ( sem_DataFieldPatExpr_6) | True ->
                                                                                                                                                                                                                                                                      ( _lhsOpatTyVarMp,_lhsOvalGam,sem_DataFieldPatExpr_6) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                                                                               in  sem_DataFieldPatExpr_Ext_5)) of
                                                                                                                                                                                                        { ( sem_DataFieldPatExpr_5) | True ->
                                                                                                                                                                                                        ( _lhsOchrInstDeclSq,sem_DataFieldPatExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                                in  sem_DataFieldPatExpr_Ext_4)) of
                                                                                                                                                         { ( sem_DataFieldPatExpr_4) | True ->
                                                                                                                                                         ( _lhsOkiVarMp,_lhsOpolVarMp,_lhsOtyGam,_lhsOtyKiGam,sem_DataFieldPatExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                     in  sem_DataFieldPatExpr_Ext_3)) of
                                                                                              { ( sem_DataFieldPatExpr_3) | True ->
                                                                                              ( _lhsOpredSameScopeCounter,sem_DataFieldPatExpr_3) }) }) }) }) }) }) }) }) }) })))))
                                                            in  sem_DataFieldPatExpr_Ext_2)) of
                                                     { ( sem_DataFieldPatExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_DataFieldPatExpr_2) }) }) }) }) }) }) })))
                        in  sem_DataFieldPatExpr_Ext_1)) of
                 { ( sem_DataFieldPatExpr_1) | True ->
                 ( _lhsOrange,sem_DataFieldPatExpr_1) }) }) }) }) })


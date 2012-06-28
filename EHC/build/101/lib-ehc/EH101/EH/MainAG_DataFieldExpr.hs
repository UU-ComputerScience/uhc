


module EH101.EH.MainAG_DataFieldExpr where

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

-- DataFieldExpr -----------------------------------------------
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
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         tyTyTySigFreeTvarS   : TyVarIdS
      chained attribute:
         gathDataGam          : DataGam
   visit 5:
      inherited attribute:
         dataGam              : DataGam
      synthesized attribute:
         chrInstDeclSq        : Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)
   visit 6:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         fiOpts               : FIOpts
         tvKiVarMp            : VarMp
         tyTyGlobFreeTvarS    : TyVarIdS
         valGam               : ValGam
         valTyGlobFreeTvarS   : TyVarIdS
      chained attribute:
         updExprTyVarMp       : VarMp
      synthesized attributes:
         fldL                 : [HsName]
         mbConNm              : Maybe HsName
         updExprTy            : Ty
   visit 7:
      inherited attributes:
         fldFIOpts            : FIOpts
         knTy                 : Ty
      chained attribute:
         tyVarMp              : VarMp
      synthesized attributes:
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
         noLetQuantTyVarIdS   : TyVarIdS
         ty                   : Ty
   visit 8:
      inherited attributes:
         chrEvidBindMp        : EvidKeyToCBindMap
         chrScopeBindMp       : PredScopeToCBindMap
         dgi                  : DataGamInfo
         finTyVarMp           : VarMp
         finValGam            : ValGam
         moduleNm             : HsName
         rangeMp              : RangeMp
      chained attribute:
         cSubst               : CSubst
      synthesized attributes:
         allErrSq             : ErrSq
         dfeCBindL            : CBindL
         dfeCExpr             : CExpr
         errSq                : ErrSq
         fuCExprL             : FieldUpdateL (DataTagInfo -> Int -> CExpr)
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         mbDti                : Maybe DataTagInfo
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Con:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 7:
            local ty          : {Ty}
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local _tup11      : {(Ty,ErrL)}
            local gTy         : {Ty}
            local fo_         : {FIOut}
            intra _tup12      : {(UID,UID,UID)}
            intra range       : {Range}
         visit 8:
            local dti         : _
            local pp          : _
            local nmErrs      : {ErrL}
            local gathMentrelFilterMp : _
            intra _tup11      : {(Ty,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra gTy         : {Ty}
      alternative Expr:
         child hsrange        : {Range}
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup13      : {(UID,UID)}
         visit 2:
            intra _tup13      : {(UID,UID)}
         visit 3:
            intra _tup13      : {(UID,UID)}
         visit 4:
            intra _tup13      : {(UID,UID)}
         visit 5:
            intra _tup13      : {(UID,UID)}
         visit 6:
            local lUniq       : {UID}
            intra _tup13      : {(UID,UID)}
         visit 7:
            local ty          : {Ty}
         visit 8:
            local dfeCBindL   : _
            local dfeCExpr    : _
            local fuCExprL    : _
            local pp          : _
      alternative Upd:
         child hsrange        : {Range}
         child dataFieldExpr  : DataFieldExpr 
         child nm             : {HsName}
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 5:
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 6:
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 7:
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local _tup14      : {(Ty,Ty,Ty,FIOut,ErrL)}
            local fo_         : {FIOut}
            local knDataTy    : {Ty}
            local ty          : {Ty}
            intra _tup15      : {(UID,UID)}
            intra range       : {Range}
         visit 8:
            local fldExprNm   : _
            local finalTyExpr : _
            local finalTyExprExpanded : _
            local pp          : _
            local nmErrs      : {ErrL}
            local gTy         : {Ty}
            local gathMentrelFilterMp : _
            intra lUniq       : {UID}
            intra fe          : {FIEnv}
            intra _tup14      : {(Ty,Ty,Ty,FIOut,ErrL)}
            intra fo_         : {FIOut}
            intra range       : {Range}
-}
sem_DataFieldExpr_Con :: Range ->
                         HsName ->
                         T_DataFieldExpr 

sem_DataFieldExpr_Con hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_DataFieldExpr_Con_1 :: T_DataFieldExpr_1 
                  sem_DataFieldExpr_Con_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> (__cont, lUniq,lUniq2)}} )) of
                             { __tup12 | __tup12 `seq` (True) ->
                             (case (__tup12) of
                              { (_lhsOgUniq,_,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_DataFieldExpr_Con_2 :: T_DataFieldExpr_2 
                                          sem_DataFieldExpr_Con_2  =
                                              (\ _lhsIkiGam
                                                 _lhsIlexLev
                                                 _lhsIpredSameScopeCounter ->
                                                   _lhsIkiGam `seq`
                                                   (_lhsIlexLev `seq`
                                                    (_lhsIpredSameScopeCounter `seq`
                                                     ((case (_lhsIpredSameScopeCounter) of
                                                       { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                       (case ((let sem_DataFieldExpr_Con_3 :: T_DataFieldExpr_3 
                                                                   sem_DataFieldExpr_Con_3  =
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
                                                                                      (case ((let sem_DataFieldExpr_Con_4 :: T_DataFieldExpr_4 
                                                                                                  sem_DataFieldExpr_Con_4  =
                                                                                                      (\ _lhsIclGam
                                                                                                         _lhsIfinKiVarMp
                                                                                                         _lhsIfinTyKiGam
                                                                                                         _lhsIgathDataGam
                                                                                                         _lhsItyTyTySigFreeTvarS ->
                                                                                                           _lhsIclGam `seq`
                                                                                                           (_lhsIfinKiVarMp `seq`
                                                                                                            (_lhsIfinTyKiGam `seq`
                                                                                                             (_lhsIgathDataGam `seq`
                                                                                                              (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                               ((case (_lhsIgathDataGam) of
                                                                                                                 { _lhsOgathDataGam | _lhsOgathDataGam `seq` (True) ->
                                                                                                                 (case ((let sem_DataFieldExpr_Con_5 :: T_DataFieldExpr_5 
                                                                                                                             sem_DataFieldExpr_Con_5  =
                                                                                                                                 (\ _lhsIdataGam ->
                                                                                                                                      _lhsIdataGam `seq`
                                                                                                                                      ((case (Seq.empty) of
                                                                                                                                        { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                        (case ((let sem_DataFieldExpr_Con_6 :: T_DataFieldExpr_6 
                                                                                                                                                    sem_DataFieldExpr_Con_6  =
                                                                                                                                                        (\ _lhsIchrStore
                                                                                                                                                           _lhsIclDfGam
                                                                                                                                                           _lhsIfiOpts
                                                                                                                                                           _lhsItvKiVarMp
                                                                                                                                                           _lhsItyTyGlobFreeTvarS
                                                                                                                                                           _lhsIupdExprTyVarMp
                                                                                                                                                           _lhsIvalGam
                                                                                                                                                           _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                             _lhsIchrStore `seq`
                                                                                                                                                             (_lhsIclDfGam `seq`
                                                                                                                                                              (_lhsIfiOpts `seq`
                                                                                                                                                               (_lhsItvKiVarMp `seq`
                                                                                                                                                                (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                 (_lhsIupdExprTyVarMp `seq`
                                                                                                                                                                  (_lhsIvalGam `seq`
                                                                                                                                                                   (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                    ((case ([]) of
                                                                                                                                                                      { _lhsOfldL | _lhsOfldL `seq` (True) ->
                                                                                                                                                                      (case (Just nm_) of
                                                                                                                                                                       { _lhsOmbConNm | _lhsOmbConNm `seq` (True) ->
                                                                                                                                                                       (case (Ty_Any) of
                                                                                                                                                                        { _lhsOupdExprTy | _lhsOupdExprTy `seq` (True) ->
                                                                                                                                                                        (case (_lhsIupdExprTyVarMp) of
                                                                                                                                                                         { _lhsOupdExprTyVarMp | _lhsOupdExprTyVarMp `seq` (True) ->
                                                                                                                                                                         (case ((let sem_DataFieldExpr_Con_7 :: T_DataFieldExpr_7 
                                                                                                                                                                                     sem_DataFieldExpr_Con_7  =
                                                                                                                                                                                         (\ _lhsIfldFIOpts
                                                                                                                                                                                            _lhsIknTy
                                                                                                                                                                                            _lhsItyVarMp ->
                                                                                                                                                                                              _lhsIfldFIOpts `seq`
                                                                                                                                                                                              (_lhsIknTy `seq`
                                                                                                                                                                                               (_lhsItyVarMp `seq`
                                                                                                                                                                                                ((case (Map.empty) of
                                                                                                                                                                                                  { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                  (case (Map.empty) of
                                                                                                                                                                                                   { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                   (case (Set.empty) of
                                                                                                                                                                                                    { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                    (case (_lhsIknTy) of
                                                                                                                                                                                                     { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                     (case (_ty) of
                                                                                                                                                                                                      { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                      (case (__tup12) of
                                                                                                                                                                                                       { (_,_,_lUniq2) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                       (case (__tup12) of
                                                                                                                                                                                                        { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                        (case (defaultFIEnv
                                                                                                                                                                                                                   { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                   , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                   , feTyGam = _lhsItyGam
                                                                                                                                                                                                                   , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                   , feRange = _range
                                                                                                                                                                                                                   }) of
                                                                                                                                                                                                         { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                         (case (valGamLookupTy (hsnUn nm_) _lhsIvalGam) of
                                                                                                                                                                                                          { __tup11 | __tup11 `seq` (True) ->
                                                                                                                                                                                                          (case (__tup11) of
                                                                                                                                                                                                           { (_gTy,_) | _gTy `seq` (True) ->
                                                                                                                                                                                                           (case (let [u] = mkNewTyVarL 1 _lUniq
                                                                                                                                                                                                                  in  fitsIn _lhsIfiOpts _fe _lUniq2 _lhsItyVarMp (_gTy) ([_lhsIknTy] `mkArrow` u)) of
                                                                                                                                                                                                            { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                            (case (foVarMp _fo_ `varUpd` _lhsItyVarMp) of
                                                                                                                                                                                                             { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                             (case ((let sem_DataFieldExpr_Con_8 :: T_DataFieldExpr_8 
                                                                                                                                                                                                                         sem_DataFieldExpr_Con_8  =
                                                                                                                                                                                                                             (\ _lhsIcSubst
                                                                                                                                                                                                                                _lhsIchrEvidBindMp
                                                                                                                                                                                                                                _lhsIchrScopeBindMp
                                                                                                                                                                                                                                _lhsIdgi
                                                                                                                                                                                                                                _lhsIfinTyVarMp
                                                                                                                                                                                                                                _lhsIfinValGam
                                                                                                                                                                                                                                _lhsImoduleNm
                                                                                                                                                                                                                                _lhsIrangeMp ->
                                                                                                                                                                                                                                  _lhsIcSubst `seq`
                                                                                                                                                                                                                                  (_lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                   (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                    (_lhsIdgi `seq`
                                                                                                                                                                                                                                     (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                      (_lhsIfinValGam `seq`
                                                                                                                                                                                                                                       (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                        (_lhsIrangeMp `seq`
                                                                                                                                                                                                                                         ((case (Seq.empty) of
                                                                                                                                                                                                                                           { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                           (case (_lhsIcSubst) of
                                                                                                                                                                                                                                            { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                            (case ([]) of
                                                                                                                                                                                                                                             { _lhsOdfeCBindL | _lhsOdfeCBindL `seq` (True) ->
                                                                                                                                                                                                                                             (case (dgiDtiOfCon nm_ _lhsIdgi) of
                                                                                                                                                                                                                                              { _dti | _dti `seq` (True) ->
                                                                                                                                                                                                                                              (case (CExpr_Tup (dtiCTag _dti)) of
                                                                                                                                                                                                                                               { _lhsOdfeCExpr | _lhsOdfeCExpr `seq` (True) ->
                                                                                                                                                                                                                                               (case (pp nm_) of
                                                                                                                                                                                                                                                { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                (case (__tup11) of
                                                                                                                                                                                                                                                 { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                                                 (case (rngLift _range mkNestErr' _pp [Seq.fromList _nmErrs, foErrSq _fo_]) of
                                                                                                                                                                                                                                                  { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                  (case ([]) of
                                                                                                                                                                                                                                                   { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                   (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Val (hsnUn nm_)
                                                                                                                                                                                                                                                          `mentrelFilterMpUnion`
                                                                                                                                                                                                                                                          tyUsedNames _lhsImoduleNm _gTy) of
                                                                                                                                                                                                                                                    { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                    (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                                     { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                     (case (emptyVarMp) of
                                                                                                                                                                                                                                                      { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                      (case (Just _dti) of
                                                                                                                                                                                                                                                       { _lhsOmbDti | _lhsOmbDti `seq` (True) ->
                                                                                                                                                                                                                                                       (case (_pp) of
                                                                                                                                                                                                                                                        { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                        (case ([]) of
                                                                                                                                                                                                                                                         { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                         ( _lhsOallErrSq,_lhsOcSubst,_lhsOdfeCBindL,_lhsOdfeCExpr,_lhsOerrSq,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOmbDti,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                     in  sem_DataFieldExpr_Con_8)) of
                                                                                                                                                                                                              { ( sem_DataFieldExpr_8) | True ->
                                                                                                                                                                                                              ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOty,_lhsOtyVarMp,sem_DataFieldExpr_8) }) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                                                                                                 in  sem_DataFieldExpr_Con_7)) of
                                                                                                                                                                          { ( sem_DataFieldExpr_7) | True ->
                                                                                                                                                                          ( _lhsOfldL,_lhsOmbConNm,_lhsOupdExprTy,_lhsOupdExprTyVarMp,sem_DataFieldExpr_7) }) }) }) }) }))))))))))
                                                                                                                                                in  sem_DataFieldExpr_Con_6)) of
                                                                                                                                         { ( sem_DataFieldExpr_6) | True ->
                                                                                                                                         ( _lhsOchrInstDeclSq,sem_DataFieldExpr_6) }) })))
                                                                                                                         in  sem_DataFieldExpr_Con_5)) of
                                                                                                                  { ( sem_DataFieldExpr_5) | True ->
                                                                                                                  ( _lhsOgathDataGam,sem_DataFieldExpr_5) }) })))))))
                                                                                              in  sem_DataFieldExpr_Con_4)) of
                                                                                       { ( sem_DataFieldExpr_4) | True ->
                                                                                       ( _lhsOkiVarMp,_lhsOpolVarMp,sem_DataFieldExpr_4) }) }) }))))))))))
                                                               in  sem_DataFieldExpr_Con_3)) of
                                                        { ( sem_DataFieldExpr_3) | True ->
                                                        ( _lhsOpredSameScopeCounter,sem_DataFieldExpr_3) }) })))))
                                      in  sem_DataFieldExpr_Con_2)) of
                               { ( sem_DataFieldExpr_2) | True ->
                               ( _lhsOgUniq,sem_DataFieldExpr_2) }) }) })))
              in  sem_DataFieldExpr_Con_1)) of
       { ( sem_DataFieldExpr_1) | True ->
       ( _lhsOrange,sem_DataFieldExpr_1) }) }) })

sem_DataFieldExpr_Expr :: Range ->
                          T_Expr  ->
                          T_DataFieldExpr 

sem_DataFieldExpr_Expr hsrange_ expr_  | hsrange_ `seq` (expr_ `seq` (True)) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (rangeUnions [hsrange_, _exprIrange   , _exprIrange  ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_DataFieldExpr_Expr_1 :: T_DataFieldExpr_1 
                       sem_DataFieldExpr_Expr_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                                  { __tup13 | __tup13 `seq` (True) ->
                                  (case (__tup13) of
                                   { (_exprOgUniq,_) | _exprOgUniq `seq` (True) ->
                                   (case (expr_1 _exprOgUniq ) of
                                    { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                        (case (_exprIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_DataFieldExpr_Expr_2 :: T_DataFieldExpr_2 
                                                     sem_DataFieldExpr_Expr_2  =
                                                         (\ _lhsIkiGam
                                                            _lhsIlexLev
                                                            _lhsIpredSameScopeCounter ->
                                                              _lhsIkiGam `seq`
                                                              (_lhsIlexLev `seq`
                                                               (_lhsIpredSameScopeCounter `seq`
                                                                ((case (_lhsIpredSameScopeCounter) of
                                                                  { _exprOpredSameScopeCounter | _exprOpredSameScopeCounter `seq` (True) ->
                                                                  (case (_lhsIlexLev) of
                                                                   { _exprOlexLev | _exprOlexLev `seq` (True) ->
                                                                   (case (_lhsIkiGam) of
                                                                    { _exprOkiGam | _exprOkiGam `seq` (True) ->
                                                                    (case (True) of
                                                                     { _exprOisFirstLet | _exprOisFirstLet `seq` (True) ->
                                                                     (case (expr_2 _exprOisFirstLet _exprOkiGam _exprOlexLev _exprOpredSameScopeCounter ) of
                                                                      { ( _exprIpredSameScopeCounter,expr_3) | True ->
                                                                          (case (_exprIpredSameScopeCounter) of
                                                                           { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                           (case ((let sem_DataFieldExpr_Expr_3 :: T_DataFieldExpr_3 
                                                                                       sem_DataFieldExpr_Expr_3  =
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
                                                                                                         { _exprOtyKiGlobFreeTvarS | _exprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                         (case (_lhsItyKiGam) of
                                                                                                          { _exprOtyKiGam | _exprOtyKiGam `seq` (True) ->
                                                                                                          (case (_lhsItyGam) of
                                                                                                           { _exprOtyGam | _exprOtyGam `seq` (True) ->
                                                                                                           (case (_lhsIpredScope) of
                                                                                                            { _exprOpredScope | _exprOpredScope `seq` (True) ->
                                                                                                            (case (_lhsIpolVarMp) of
                                                                                                             { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                                                                             (case (_lhsIpolGam) of
                                                                                                              { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                                                                                              (case (_lhsIopts) of
                                                                                                               { _exprOopts | _exprOopts `seq` (True) ->
                                                                                                               (case (_lhsIkiVarMp) of
                                                                                                                { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                                                                     (case (_exprIkiVarMp) of
                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                      (case (_exprIpolVarMp) of
                                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                       (case ((let sem_DataFieldExpr_Expr_4 :: T_DataFieldExpr_4 
                                                                                                                                   sem_DataFieldExpr_Expr_4  =
                                                                                                                                       (\ _lhsIclGam
                                                                                                                                          _lhsIfinKiVarMp
                                                                                                                                          _lhsIfinTyKiGam
                                                                                                                                          _lhsIgathDataGam
                                                                                                                                          _lhsItyTyTySigFreeTvarS ->
                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                            (_lhsIfinKiVarMp `seq`
                                                                                                                                             (_lhsIfinTyKiGam `seq`
                                                                                                                                              (_lhsIgathDataGam `seq`
                                                                                                                                               (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                ((case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                  { _exprOtyTyTySigFreeTvarS | _exprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                  (case (_lhsIgathDataGam) of
                                                                                                                                                   { _exprOgathDataGam | _exprOgathDataGam `seq` (True) ->
                                                                                                                                                   (case (_lhsIfinTyKiGam) of
                                                                                                                                                    { _exprOfinTyKiGam | _exprOfinTyKiGam `seq` (True) ->
                                                                                                                                                    (case (_lhsIfinKiVarMp) of
                                                                                                                                                     { _exprOfinKiVarMp | _exprOfinKiVarMp `seq` (True) ->
                                                                                                                                                     (case (_lhsIclGam) of
                                                                                                                                                      { _exprOclGam | _exprOclGam `seq` (True) ->
                                                                                                                                                      (case (expr_4 _exprOclGam _exprOfinKiVarMp _exprOfinTyKiGam _exprOgathDataGam _exprOtyTyTySigFreeTvarS ) of
                                                                                                                                                       { ( _exprIgathDataGam,expr_5) | True ->
                                                                                                                                                           (case (_exprIgathDataGam) of
                                                                                                                                                            { _lhsOgathDataGam | _lhsOgathDataGam `seq` (True) ->
                                                                                                                                                            (case ((let sem_DataFieldExpr_Expr_5 :: T_DataFieldExpr_5 
                                                                                                                                                                        sem_DataFieldExpr_Expr_5  =
                                                                                                                                                                            (\ _lhsIdataGam ->
                                                                                                                                                                                 _lhsIdataGam `seq`
                                                                                                                                                                                 ((case (_lhsIdataGam) of
                                                                                                                                                                                   { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                   (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                    { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                        (case (_exprIchrInstDeclSq) of
                                                                                                                                                                                         { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                         (case ((let sem_DataFieldExpr_Expr_6 :: T_DataFieldExpr_6 
                                                                                                                                                                                                     sem_DataFieldExpr_Expr_6  =
                                                                                                                                                                                                         (\ _lhsIchrStore
                                                                                                                                                                                                            _lhsIclDfGam
                                                                                                                                                                                                            _lhsIfiOpts
                                                                                                                                                                                                            _lhsItvKiVarMp
                                                                                                                                                                                                            _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                            _lhsIupdExprTyVarMp
                                                                                                                                                                                                            _lhsIvalGam
                                                                                                                                                                                                            _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                              _lhsIchrStore `seq`
                                                                                                                                                                                                              (_lhsIclDfGam `seq`
                                                                                                                                                                                                               (_lhsIfiOpts `seq`
                                                                                                                                                                                                                (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                 (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                  (_lhsIupdExprTyVarMp `seq`
                                                                                                                                                                                                                   (_lhsIvalGam `seq`
                                                                                                                                                                                                                    (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                     ((case ([]) of
                                                                                                                                                                                                                       { _lhsOfldL | _lhsOfldL `seq` (True) ->
                                                                                                                                                                                                                       (case (Nothing) of
                                                                                                                                                                                                                        { _lhsOmbConNm | _lhsOmbConNm `seq` (True) ->
                                                                                                                                                                                                                        (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                         { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                         (case (_lhsIvalGam) of
                                                                                                                                                                                                                          { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                          (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                           { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                           (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                            { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                            (case (_lhsIfiOpts) of
                                                                                                                                                                                                                             { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                             (case (_lhsIclDfGam) of
                                                                                                                                                                                                                              { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                              (case (_lhsIchrStore) of
                                                                                                                                                                                                                               { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                                                                                                                                               (case (__tup13) of
                                                                                                                                                                                                                                { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                (case (mkNewTyVar _lUniq) of
                                                                                                                                                                                                                                 { _exprOknTy | _exprOknTy `seq` (True) ->
                                                                                                                                                                                                                                 (case (_lhsIupdExprTyVarMp) of
                                                                                                                                                                                                                                  { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                  (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                   { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                       (case (_exprIty) of
                                                                                                                                                                                                                                        { _lhsOupdExprTy | _lhsOupdExprTy `seq` (True) ->
                                                                                                                                                                                                                                        (case (_exprItyVarMp) of
                                                                                                                                                                                                                                         { _lhsOupdExprTyVarMp | _lhsOupdExprTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                         (case ((let sem_DataFieldExpr_Expr_7 :: T_DataFieldExpr_7 
                                                                                                                                                                                                                                                     sem_DataFieldExpr_Expr_7  =
                                                                                                                                                                                                                                                         (\ _lhsIfldFIOpts
                                                                                                                                                                                                                                                            _lhsIknTy
                                                                                                                                                                                                                                                            _lhsItyVarMp ->
                                                                                                                                                                                                                                                              _lhsIfldFIOpts `seq`
                                                                                                                                                                                                                                                              (_lhsIknTy `seq`
                                                                                                                                                                                                                                                               (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                ((case (_exprIgathCnstrMp) of
                                                                                                                                                                                                                                                                  { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                  (case (_exprIgathRangeMp) of
                                                                                                                                                                                                                                                                   { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                   (case (_exprInoLetQuantTyVarIdS) of
                                                                                                                                                                                                                                                                    { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                                                                                    (case (_lhsIknTy) of
                                                                                                                                                                                                                                                                     { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                     (case (_ty) of
                                                                                                                                                                                                                                                                      { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                      (case (_lhsItyVarMp) of
                                                                                                                                                                                                                                                                       { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                       (case ((let sem_DataFieldExpr_Expr_8 :: T_DataFieldExpr_8 
                                                                                                                                                                                                                                                                                   sem_DataFieldExpr_Expr_8  =
                                                                                                                                                                                                                                                                                       (\ _lhsIcSubst
                                                                                                                                                                                                                                                                                          _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                                                          _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                                                          _lhsIdgi
                                                                                                                                                                                                                                                                                          _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                          _lhsIfinValGam
                                                                                                                                                                                                                                                                                          _lhsImoduleNm
                                                                                                                                                                                                                                                                                          _lhsIrangeMp ->
                                                                                                                                                                                                                                                                                            _lhsIcSubst `seq`
                                                                                                                                                                                                                                                                                            (_lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                                                             (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                                                              (_lhsIdgi `seq`
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
                                                                                                                                                                                                                                                                                                       (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                        { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                         { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                         (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                          { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                           { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                           (case (True) of
                                                                                                                                                                                                                                                                                                            { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                                                                                                                                                                                            (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                                                                                                                                                                                             { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                                                                                                                                                                                                 (case (_exprIallErrSq) of
                                                                                                                                                                                                                                                                                                                  { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (_exprIcSubst) of
                                                                                                                                                                                                                                                                                                                   { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case ([]) of
                                                                                                                                                                                                                                                                                                                    { _dfeCBindL | _dfeCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (_dfeCBindL) of
                                                                                                                                                                                                                                                                                                                     { _lhsOdfeCBindL | _lhsOdfeCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (_exprIcexpr) of
                                                                                                                                                                                                                                                                                                                      { _dfeCExpr | _dfeCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                      (case (_dfeCExpr) of
                                                                                                                                                                                                                                                                                                                       { _lhsOdfeCExpr | _lhsOdfeCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (_exprIerrSq) of
                                                                                                                                                                                                                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case ([]) of
                                                                                                                                                                                                                                                                                                                         { _fuCExprL | _fuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (_fuCExprL) of
                                                                                                                                                                                                                                                                                                                          { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                          (case (_exprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                           { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                           (case (_exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                            { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                            (case (Nothing) of
                                                                                                                                                                                                                                                                                                                             { _lhsOmbDti | _lhsOmbDti `seq` (True) ->
                                                                                                                                                                                                                                                                                                                             (case (_exprIpp) of
                                                                                                                                                                                                                                                                                                                              { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                              (case (_pp) of
                                                                                                                                                                                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                               (case ([]) of
                                                                                                                                                                                                                                                                                                                                { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                ( _lhsOallErrSq,_lhsOcSubst,_lhsOdfeCBindL,_lhsOdfeCExpr,_lhsOerrSq,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOmbDti,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                                               in  sem_DataFieldExpr_Expr_8)) of
                                                                                                                                                                                                                                                                        { ( sem_DataFieldExpr_8) | True ->
                                                                                                                                                                                                                                                                        ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOty,_lhsOtyVarMp,sem_DataFieldExpr_8) }) }) }) }) }) }) })))))
                                                                                                                                                                                                                                                 in  sem_DataFieldExpr_Expr_7)) of
                                                                                                                                                                                                                                          { ( sem_DataFieldExpr_7) | True ->
                                                                                                                                                                                                                                          ( _lhsOfldL,_lhsOmbConNm,_lhsOupdExprTy,_lhsOupdExprTyVarMp,sem_DataFieldExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                 in  sem_DataFieldExpr_Expr_6)) of
                                                                                                                                                                                          { ( sem_DataFieldExpr_6) | True ->
                                                                                                                                                                                          ( _lhsOchrInstDeclSq,sem_DataFieldExpr_6) }) }) }) })))
                                                                                                                                                                    in  sem_DataFieldExpr_Expr_5)) of
                                                                                                                                                             { ( sem_DataFieldExpr_5) | True ->
                                                                                                                                                             ( _lhsOgathDataGam,sem_DataFieldExpr_5) }) }) }) }) }) }) }) })))))))
                                                                                                                               in  sem_DataFieldExpr_Expr_4)) of
                                                                                                                        { ( sem_DataFieldExpr_4) | True ->
                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,sem_DataFieldExpr_4) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                   in  sem_DataFieldExpr_Expr_3)) of
                                                                            { ( sem_DataFieldExpr_3) | True ->
                                                                            ( _lhsOpredSameScopeCounter,sem_DataFieldExpr_3) }) }) }) }) }) }) })))))
                                                 in  sem_DataFieldExpr_Expr_2)) of
                                          { ( sem_DataFieldExpr_2) | True ->
                                          ( _lhsOgUniq,sem_DataFieldExpr_2) }) }) }) }) })))
                   in  sem_DataFieldExpr_Expr_1)) of
            { ( sem_DataFieldExpr_1) | True ->
            ( _lhsOrange,sem_DataFieldExpr_1) }) }) }) })

sem_DataFieldExpr_Upd :: Range ->
                         T_DataFieldExpr  ->
                         HsName ->
                         T_Expr  ->
                         T_DataFieldExpr 

sem_DataFieldExpr_Upd hsrange_ dataFieldExpr_ nm_ expr_  | hsrange_ `seq` (dataFieldExpr_ `seq` (nm_ `seq` (expr_ `seq` (True)))) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (dataFieldExpr_ ) of
          { ( _dataFieldExprIrange,dataFieldExpr_1) | True ->
              (case (rangeUnions [hsrange_, _dataFieldExprIrange
                                                          , _exprIrange  ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_DataFieldExpr_Upd_1 :: T_DataFieldExpr_1 
                            sem_DataFieldExpr_Upd_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                                       { __tup15 | __tup15 `seq` (True) ->
                                       (case (__tup15) of
                                        { (_dataFieldExprOgUniq,_) | _dataFieldExprOgUniq `seq` (True) ->
                                        (case (dataFieldExpr_1 _dataFieldExprOgUniq ) of
                                         { ( _dataFieldExprIgUniq,dataFieldExpr_2) | True ->
                                             (case (_dataFieldExprIgUniq) of
                                              { _exprOgUniq | _exprOgUniq `seq` (True) ->
                                              (case (expr_1 _exprOgUniq ) of
                                               { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                                   (case (_exprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_DataFieldExpr_Upd_2 :: T_DataFieldExpr_2 
                                                                sem_DataFieldExpr_Upd_2  =
                                                                    (\ _lhsIkiGam
                                                                       _lhsIlexLev
                                                                       _lhsIpredSameScopeCounter ->
                                                                         _lhsIkiGam `seq`
                                                                         (_lhsIlexLev `seq`
                                                                          (_lhsIpredSameScopeCounter `seq`
                                                                           ((case (_lhsIpredSameScopeCounter) of
                                                                             { _dataFieldExprOpredSameScopeCounter | _dataFieldExprOpredSameScopeCounter `seq` (True) ->
                                                                             (case (_lhsIlexLev) of
                                                                              { _dataFieldExprOlexLev | _dataFieldExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _dataFieldExprOkiGam | _dataFieldExprOkiGam `seq` (True) ->
                                                                               (case (dataFieldExpr_2 _dataFieldExprOkiGam _dataFieldExprOlexLev _dataFieldExprOpredSameScopeCounter ) of
                                                                                { ( _dataFieldExprIpredSameScopeCounter,dataFieldExpr_3) | True ->
                                                                                    (case (_dataFieldExprIpredSameScopeCounter) of
                                                                                     { _exprOpredSameScopeCounter | _exprOpredSameScopeCounter `seq` (True) ->
                                                                                     (case (_lhsIlexLev) of
                                                                                      { _exprOlexLev | _exprOlexLev `seq` (True) ->
                                                                                      (case (_lhsIkiGam) of
                                                                                       { _exprOkiGam | _exprOkiGam `seq` (True) ->
                                                                                       (case (True) of
                                                                                        { _exprOisFirstLet | _exprOisFirstLet `seq` (True) ->
                                                                                        (case (expr_2 _exprOisFirstLet _exprOkiGam _exprOlexLev _exprOpredSameScopeCounter ) of
                                                                                         { ( _exprIpredSameScopeCounter,expr_3) | True ->
                                                                                             (case (_exprIpredSameScopeCounter) of
                                                                                              { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                                              (case ((let sem_DataFieldExpr_Upd_3 :: T_DataFieldExpr_3 
                                                                                                          sem_DataFieldExpr_Upd_3  =
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
                                                                                                                            { _exprOtyKiGlobFreeTvarS | _exprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                            (case (_lhsItyKiGam) of
                                                                                                                             { _exprOtyKiGam | _exprOtyKiGam `seq` (True) ->
                                                                                                                             (case (_lhsItyGam) of
                                                                                                                              { _exprOtyGam | _exprOtyGam `seq` (True) ->
                                                                                                                              (case (_lhsIpredScope) of
                                                                                                                               { _exprOpredScope | _exprOpredScope `seq` (True) ->
                                                                                                                               (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                { _dataFieldExprOtyKiGlobFreeTvarS | _dataFieldExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                (case (_lhsItyKiGam) of
                                                                                                                                 { _dataFieldExprOtyKiGam | _dataFieldExprOtyKiGam `seq` (True) ->
                                                                                                                                 (case (_lhsItyGam) of
                                                                                                                                  { _dataFieldExprOtyGam | _dataFieldExprOtyGam `seq` (True) ->
                                                                                                                                  (case (_lhsIpredScope) of
                                                                                                                                   { _dataFieldExprOpredScope | _dataFieldExprOpredScope `seq` (True) ->
                                                                                                                                   (case (_lhsIpolVarMp) of
                                                                                                                                    { _dataFieldExprOpolVarMp | _dataFieldExprOpolVarMp `seq` (True) ->
                                                                                                                                    (case (_lhsIpolGam) of
                                                                                                                                     { _dataFieldExprOpolGam | _dataFieldExprOpolGam `seq` (True) ->
                                                                                                                                     (case (_lhsIopts) of
                                                                                                                                      { _dataFieldExprOopts | _dataFieldExprOopts `seq` (True) ->
                                                                                                                                      (case (_lhsIkiVarMp) of
                                                                                                                                       { _dataFieldExprOkiVarMp | _dataFieldExprOkiVarMp `seq` (True) ->
                                                                                                                                       (case (dataFieldExpr_3 _dataFieldExprOkiVarMp _dataFieldExprOopts _dataFieldExprOpolGam _dataFieldExprOpolVarMp _dataFieldExprOpredScope _dataFieldExprOtyGam _dataFieldExprOtyKiGam _dataFieldExprOtyKiGlobFreeTvarS ) of
                                                                                                                                        { ( _dataFieldExprIkiVarMp,_dataFieldExprIpolVarMp,dataFieldExpr_4) | True ->
                                                                                                                                            (case (_dataFieldExprIpolVarMp) of
                                                                                                                                             { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                                                                                                             (case (_lhsIpolGam) of
                                                                                                                                              { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                                                                                                                              (case (_lhsIopts) of
                                                                                                                                               { _exprOopts | _exprOopts `seq` (True) ->
                                                                                                                                               (case (_dataFieldExprIkiVarMp) of
                                                                                                                                                { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                                                                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                                                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                                                                                                     (case (_exprIkiVarMp) of
                                                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                      (case (_exprIpolVarMp) of
                                                                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                       (case ((let sem_DataFieldExpr_Upd_4 :: T_DataFieldExpr_4 
                                                                                                                                                                   sem_DataFieldExpr_Upd_4  =
                                                                                                                                                                       (\ _lhsIclGam
                                                                                                                                                                          _lhsIfinKiVarMp
                                                                                                                                                                          _lhsIfinTyKiGam
                                                                                                                                                                          _lhsIgathDataGam
                                                                                                                                                                          _lhsItyTyTySigFreeTvarS ->
                                                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                                                            (_lhsIfinKiVarMp `seq`
                                                                                                                                                                             (_lhsIfinTyKiGam `seq`
                                                                                                                                                                              (_lhsIgathDataGam `seq`
                                                                                                                                                                               (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                ((case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                  { _exprOtyTyTySigFreeTvarS | _exprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                  (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                   { _dataFieldExprOtyTyTySigFreeTvarS | _dataFieldExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                   (case (_lhsIgathDataGam) of
                                                                                                                                                                                    { _dataFieldExprOgathDataGam | _dataFieldExprOgathDataGam `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                     { _dataFieldExprOfinTyKiGam | _dataFieldExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                      { _dataFieldExprOfinKiVarMp | _dataFieldExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                      (case (_lhsIclGam) of
                                                                                                                                                                                       { _dataFieldExprOclGam | _dataFieldExprOclGam `seq` (True) ->
                                                                                                                                                                                       (case (dataFieldExpr_4 _dataFieldExprOclGam _dataFieldExprOfinKiVarMp _dataFieldExprOfinTyKiGam _dataFieldExprOgathDataGam _dataFieldExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                        { ( _dataFieldExprIgathDataGam,dataFieldExpr_5) | True ->
                                                                                                                                                                                            (case (_dataFieldExprIgathDataGam) of
                                                                                                                                                                                             { _exprOgathDataGam | _exprOgathDataGam `seq` (True) ->
                                                                                                                                                                                             (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                              { _exprOfinTyKiGam | _exprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                              (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                               { _exprOfinKiVarMp | _exprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                               (case (_lhsIclGam) of
                                                                                                                                                                                                { _exprOclGam | _exprOclGam `seq` (True) ->
                                                                                                                                                                                                (case (expr_4 _exprOclGam _exprOfinKiVarMp _exprOfinTyKiGam _exprOgathDataGam _exprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                                 { ( _exprIgathDataGam,expr_5) | True ->
                                                                                                                                                                                                     (case (_exprIgathDataGam) of
                                                                                                                                                                                                      { _lhsOgathDataGam | _lhsOgathDataGam `seq` (True) ->
                                                                                                                                                                                                      (case ((let sem_DataFieldExpr_Upd_5 :: T_DataFieldExpr_5 
                                                                                                                                                                                                                  sem_DataFieldExpr_Upd_5  =
                                                                                                                                                                                                                      (\ _lhsIdataGam ->
                                                                                                                                                                                                                           _lhsIdataGam `seq`
                                                                                                                                                                                                                           ((case (_lhsIdataGam) of
                                                                                                                                                                                                                             { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                                                             (case (_lhsIdataGam) of
                                                                                                                                                                                                                              { _dataFieldExprOdataGam | _dataFieldExprOdataGam `seq` (True) ->
                                                                                                                                                                                                                              (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                                                               { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                                                                   (case (dataFieldExpr_5 _dataFieldExprOdataGam ) of
                                                                                                                                                                                                                                    { ( _dataFieldExprIchrInstDeclSq,dataFieldExpr_6) | True ->
                                                                                                                                                                                                                                        (case (_dataFieldExprIchrInstDeclSq `Seq.union` _exprIchrInstDeclSq) of
                                                                                                                                                                                                                                         { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                                                         (case ((let sem_DataFieldExpr_Upd_6 :: T_DataFieldExpr_6 
                                                                                                                                                                                                                                                     sem_DataFieldExpr_Upd_6  =
                                                                                                                                                                                                                                                         (\ _lhsIchrStore
                                                                                                                                                                                                                                                            _lhsIclDfGam
                                                                                                                                                                                                                                                            _lhsIfiOpts
                                                                                                                                                                                                                                                            _lhsItvKiVarMp
                                                                                                                                                                                                                                                            _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                            _lhsIupdExprTyVarMp
                                                                                                                                                                                                                                                            _lhsIvalGam
                                                                                                                                                                                                                                                            _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                              _lhsIchrStore `seq`
                                                                                                                                                                                                                                                              (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                               (_lhsIfiOpts `seq`
                                                                                                                                                                                                                                                                (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                 (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                  (_lhsIupdExprTyVarMp `seq`
                                                                                                                                                                                                                                                                   (_lhsIvalGam `seq`
                                                                                                                                                                                                                                                                    (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                     ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                       { _dataFieldExprOvalTyGlobFreeTvarS | _dataFieldExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                       (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                        { _dataFieldExprOvalGam | _dataFieldExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                        (case (_lhsIupdExprTyVarMp) of
                                                                                                                                                                                                                                                                         { _dataFieldExprOupdExprTyVarMp | _dataFieldExprOupdExprTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                         (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                          { _dataFieldExprOtyTyGlobFreeTvarS | _dataFieldExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                          (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                           { _dataFieldExprOtvKiVarMp | _dataFieldExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                           (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                            { _dataFieldExprOfiOpts | _dataFieldExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                             { _dataFieldExprOclDfGam | _dataFieldExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                              { _dataFieldExprOchrStore | _dataFieldExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (dataFieldExpr_6 _dataFieldExprOchrStore _dataFieldExprOclDfGam _dataFieldExprOfiOpts _dataFieldExprOtvKiVarMp _dataFieldExprOtyTyGlobFreeTvarS _dataFieldExprOupdExprTyVarMp _dataFieldExprOvalGam _dataFieldExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                               { ( _dataFieldExprIfldL,_dataFieldExprImbConNm,_dataFieldExprIupdExprTy,_dataFieldExprIupdExprTyVarMp,dataFieldExpr_7) | True ->
                                                                                                                                                                                                                                                                                   (case (nm_ : _dataFieldExprIfldL) of
                                                                                                                                                                                                                                                                                    { _lhsOfldL | _lhsOfldL `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (_dataFieldExprImbConNm) of
                                                                                                                                                                                                                                                                                     { _lhsOmbConNm | _lhsOmbConNm `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_dataFieldExprIupdExprTy) of
                                                                                                                                                                                                                                                                                      { _lhsOupdExprTy | _lhsOupdExprTy `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_dataFieldExprIupdExprTyVarMp) of
                                                                                                                                                                                                                                                                                       { _lhsOupdExprTyVarMp | _lhsOupdExprTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case ((let sem_DataFieldExpr_Upd_7 :: T_DataFieldExpr_7 
                                                                                                                                                                                                                                                                                                   sem_DataFieldExpr_Upd_7  =
                                                                                                                                                                                                                                                                                                       (\ _lhsIfldFIOpts
                                                                                                                                                                                                                                                                                                          _lhsIknTy
                                                                                                                                                                                                                                                                                                          _lhsItyVarMp ->
                                                                                                                                                                                                                                                                                                            _lhsIfldFIOpts `seq`
                                                                                                                                                                                                                                                                                                            (_lhsIknTy `seq`
                                                                                                                                                                                                                                                                                                             (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                                                              ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                                                                 { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsIfldFIOpts) of
                                                                                                                                                                                                                                                                                                                  { _dataFieldExprOfldFIOpts | _dataFieldExprOfldFIOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (__tup15) of
                                                                                                                                                                                                                                                                                                                   { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (defaultFIEnv
                                                                                                                                                                                                                                                                                                                              { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                                                                                                                              , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                                                                                                                              , feTyGam = _lhsItyGam
                                                                                                                                                                                                                                                                                                                              , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                                                                                                              , feRange = _range
                                                                                                                                                                                                                                                                                                                              }) of
                                                                                                                                                                                                                                                                                                                    { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (dfCheck _lUniq _lhsIfiOpts _fe nm_ _lhsIvalGam _lhsItyVarMp _lhsIknTy) of
                                                                                                                                                                                                                                                                                                                     { __tup14 | __tup14 `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (__tup14) of
                                                                                                                                                                                                                                                                                                                      { (_,_,_,_fo_,_) | _fo_ `seq` (True) ->
                                                                                                                                                                                                                                                                                                                      (case (foVarMp _fo_ `varUpd` _lhsItyVarMp) of
                                                                                                                                                                                                                                                                                                                       { _dataFieldExprOtyVarMp | _dataFieldExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (__tup14) of
                                                                                                                                                                                                                                                                                                                        { (_,_knDataTy,_,_,_) | _knDataTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case (_knDataTy) of
                                                                                                                                                                                                                                                                                                                         { _dataFieldExprOknTy | _dataFieldExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (dataFieldExpr_7 _dataFieldExprOfldFIOpts _dataFieldExprOknTy _dataFieldExprOtyVarMp ) of
                                                                                                                                                                                                                                                                                                                          { ( _dataFieldExprIgathCnstrMp,_dataFieldExprIgathRangeMp,_dataFieldExprInoLetQuantTyVarIdS,_dataFieldExprIty,_dataFieldExprItyVarMp,dataFieldExpr_8) | True ->
                                                                                                                                                                                                                                                                                                                              (case (_dataFieldExprItyVarMp) of
                                                                                                                                                                                                                                                                                                                               { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                               (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                 { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                 (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                                                  { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                  (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                                                   { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                   (case (_lhsIfldFIOpts) of
                                                                                                                                                                                                                                                                                                                                    { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (__tup14) of
                                                                                                                                                                                                                                                                                                                                     { (_,_,_exprOknTy,_,_) | _exprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                                      { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                                                                                                                          (case (_dataFieldExprIgathCnstrMp `cnstrMpUnion` _exprIgathCnstrMp) of
                                                                                                                                                                                                                                                                                                                                           { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                           (case (_dataFieldExprIgathRangeMp `Map.union` _exprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                                                            { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                            (case (_dataFieldExprInoLetQuantTyVarIdS `Set.union` _exprInoLetQuantTyVarIdS) of
                                                                                                                                                                                                                                                                                                                                             { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                             (case (_knDataTy) of
                                                                                                                                                                                                                                                                                                                                              { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                              (case (_ty) of
                                                                                                                                                                                                                                                                                                                                               { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                               (case (_exprItyVarMp) of
                                                                                                                                                                                                                                                                                                                                                { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                (case ((let sem_DataFieldExpr_Upd_8 :: T_DataFieldExpr_8 
                                                                                                                                                                                                                                                                                                                                                            sem_DataFieldExpr_Upd_8  =
                                                                                                                                                                                                                                                                                                                                                                (\ _lhsIcSubst
                                                                                                                                                                                                                                                                                                                                                                   _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                                                                                                                                   _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                                                                                                                                   _lhsIdgi
                                                                                                                                                                                                                                                                                                                                                                   _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                                                                                                   _lhsIfinValGam
                                                                                                                                                                                                                                                                                                                                                                   _lhsImoduleNm
                                                                                                                                                                                                                                                                                                                                                                   _lhsIrangeMp ->
                                                                                                                                                                                                                                                                                                                                                                     _lhsIcSubst `seq`
                                                                                                                                                                                                                                                                                                                                                                     (_lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                                                                                                                                      (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                                                                                                                                       (_lhsIdgi `seq`
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
                                                                                                                                                                                                                                                                                                                                                                                 { _dataFieldExprOrangeMp | _dataFieldExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                                                  { _dataFieldExprOfinValGam | _dataFieldExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                   { _dataFieldExprOfinTyVarMp | _dataFieldExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                   (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                                    { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                    (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                     { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                     (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                      { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                      (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                       { _dataFieldExprOchrScopeBindMp | _dataFieldExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                       (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                        { _dataFieldExprOchrEvidBindMp | _dataFieldExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                        (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                         { _dataFieldExprOcSubst | _dataFieldExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                         (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                                          { _dataFieldExprOmoduleNm | _dataFieldExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                          (case (_lhsIdgi) of
                                                                                                                                                                                                                                                                                                                                                                                           { _dataFieldExprOdgi | _dataFieldExprOdgi `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                           (case (dataFieldExpr_8 _dataFieldExprOcSubst _dataFieldExprOchrEvidBindMp _dataFieldExprOchrScopeBindMp _dataFieldExprOdgi _dataFieldExprOfinTyVarMp _dataFieldExprOfinValGam _dataFieldExprOmoduleNm _dataFieldExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                                            { ( _dataFieldExprIallErrSq,_dataFieldExprIcSubst,_dataFieldExprIdfeCBindL,_dataFieldExprIdfeCExpr,_dataFieldExprIerrSq,_dataFieldExprIfuCExprL,_dataFieldExprIgathMentrelFilterMp,_dataFieldExprIgathTvKiVarMp,_dataFieldExprImbDti,_dataFieldExprIpp,_dataFieldExprIppL) | True ->
                                                                                                                                                                                                                                                                                                                                                                                                (case (_dataFieldExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                                 { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                 (case (True) of
                                                                                                                                                                                                                                                                                                                                                                                                  { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                  (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                                                   { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                                                                                                                                                                                                                                                                                       (case (_dataFieldExprIallErrSq `Seq.union` _exprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                                                        { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                        (case (_exprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                                         { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                         (case (mkHNm _lUniq) of
                                                                                                                                                                                                                                                                                                                                                                                                          { _fldExprNm | _fldExprNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                          (case (_lhsIfinTyVarMp `varUpd` _exprIty) of
                                                                                                                                                                                                                                                                                                                                                                                                           { _finalTyExpr | _finalTyExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                           (case (tyCanonicFFI (emptyTyBetaRedEnv' _fe) _finalTyExpr) of
                                                                                                                                                                                                                                                                                                                                                                                                            { _finalTyExprExpanded | _finalTyExprExpanded `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                            (case (acoreBind1Ty _fldExprNm _finalTyExprExpanded _exprIcexpr : _dataFieldExprIdfeCBindL) of
                                                                                                                                                                                                                                                                                                                                                                                                             { _lhsOdfeCBindL | _lhsOdfeCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                             (case (_dataFieldExprIdfeCExpr) of
                                                                                                                                                                                                                                                                                                                                                                                                              { _lhsOdfeCExpr | _lhsOdfeCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                              (case (ppFld "=" Nothing nm_ (pp nm_) _exprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                                               { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                               (case (__tup14) of
                                                                                                                                                                                                                                                                                                                                                                                                                { (_,_,_,_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                (case (Seq.unions [ _dataFieldExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                                                  , rngLift _range mkNestErr' _pp [_exprIerrSq, Seq.fromList _nmErrs, foErrSq _fo_]
                                                                                                                                                                                                                                                                                                                                                                                                                                  ]) of
                                                                                                                                                                                                                                                                                                                                                                                                                 { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                 (case (let mke dti o = CExpr_TupIns _dataFieldExprIdfeCExpr (dtiCTag dti) nm_ (acoreInt o) (acoreVar _fldExprNm)
                                                                                                                                                                                                                                                                                                                                                                                                                        in  (nm_,(mke,Nothing)) : _dataFieldExprIfuCExprL) of
                                                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                  (case (__tup14) of
                                                                                                                                                                                                                                                                                                                                                                                                                   { (_gTy,_,_,_,_) | _gTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                   (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Val (hsnFldUpd nm_)
                                                                                                                                                                                                                                                                                                                                                                                                                          `mentrelFilterMpUnion`
                                                                                                                                                                                                                                                                                                                                                                                                                          tyUsedNames _lhsImoduleNm _gTy) of
                                                                                                                                                                                                                                                                                                                                                                                                                    { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                    (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                                                                     { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                     (case (_dataFieldExprIgathTvKiVarMp `varmpUnion` _exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                                                      { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                      (case (_dataFieldExprImbDti) of
                                                                                                                                                                                                                                                                                                                                                                                                                       { _lhsOmbDti | _lhsOmbDti `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                       (case (_dataFieldExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                                                        { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                        (case (_pp : _dataFieldExprIppL) of
                                                                                                                                                                                                                                                                                                                                                                                                                         { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                                         ( _lhsOallErrSq,_lhsOcSubst,_lhsOdfeCBindL,_lhsOdfeCExpr,_lhsOerrSq,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOmbDti,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                                                                                                                        in  sem_DataFieldExpr_Upd_8)) of
                                                                                                                                                                                                                                                                                                                                                 { ( sem_DataFieldExpr_8) | True ->
                                                                                                                                                                                                                                                                                                                                                 ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOty,_lhsOtyVarMp,sem_DataFieldExpr_8) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                                                                                                                                                                                                               in  sem_DataFieldExpr_Upd_7)) of
                                                                                                                                                                                                                                                                                        { ( sem_DataFieldExpr_7) | True ->
                                                                                                                                                                                                                                                                                        ( _lhsOfldL,_lhsOmbConNm,_lhsOupdExprTy,_lhsOupdExprTyVarMp,sem_DataFieldExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                 in  sem_DataFieldExpr_Upd_6)) of
                                                                                                                                                                                                                                          { ( sem_DataFieldExpr_6) | True ->
                                                                                                                                                                                                                                          ( _lhsOchrInstDeclSq,sem_DataFieldExpr_6) }) }) }) }) }) })))
                                                                                                                                                                                                              in  sem_DataFieldExpr_Upd_5)) of
                                                                                                                                                                                                       { ( sem_DataFieldExpr_5) | True ->
                                                                                                                                                                                                       ( _lhsOgathDataGam,sem_DataFieldExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                               in  sem_DataFieldExpr_Upd_4)) of
                                                                                                                                                        { ( sem_DataFieldExpr_4) | True ->
                                                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,sem_DataFieldExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                      in  sem_DataFieldExpr_Upd_3)) of
                                                                                               { ( sem_DataFieldExpr_3) | True ->
                                                                                               ( _lhsOpredSameScopeCounter,sem_DataFieldExpr_3) }) }) }) }) }) }) }) }) }) }) })))))
                                                            in  sem_DataFieldExpr_Upd_2)) of
                                                     { ( sem_DataFieldExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_DataFieldExpr_2) }) }) }) }) }) }) })))
                        in  sem_DataFieldExpr_Upd_1)) of
                 { ( sem_DataFieldExpr_1) | True ->
                 ( _lhsOrange,sem_DataFieldExpr_1) }) }) }) }) })


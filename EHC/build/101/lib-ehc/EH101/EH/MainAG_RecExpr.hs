


module EH101.EH.MainAG_RecExpr where

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

-- RecExpr -----------------------------------------------------
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
      chained attribute:
         positionalFldNmL     : [HsName]
      synthesized attribute:
         chrInstDeclSq        : Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)
   visit 6:
      inherited attributes:
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         fiOpts               : FIOpts
         knTy                 : Ty
         tvKiVarMp            : VarMp
         tyTyGlobFreeTvarS    : TyVarIdS
         valGam               : ValGam
         valTyGlobFreeTvarS   : TyVarIdS
      chained attribute:
         tyVarMp              : VarMp
      synthesized attributes:
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
         noLetQuantTyVarIdS   : TyVarIdS
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
         extNm                : HsName
         fuCExprL             : FieldUpdateL CExpr
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         isExtFromEmpty       : Bool
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
         recCExpr             : CExpr
         ty                   : Ty
   alternatives:
      alternative Empty:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 5:
            intra _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 6:
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local fo_         : {FIOut}
            intra _tup200     : {(UID,UID)}
            intra range       : {Range}
         visit 7:
            local pp          : _
            local recCExpr    : _
            local ty          : {Ty}
            intra fo_         : {FIOut}
            intra range       : {Range}
      alternative Expr:
         child hsrange        : {Range}
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 7:
            local pp          : _
            local recCExpr    : _
      alternative Ext:
         child hsrange        : {Range}
         child recExpr        : RecExpr 
         child mbNm           : {Maybe HsName}
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            local _tup201     : {(HsName,[HsName])}
            intra _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local positionalNm : {HsName}
            local nm          : {HsName}
            local knFIOpts    : {FIOpts}
            local _tup202     : _
            local knRecTy     : {Ty}
            local foKnRec     : {FIOut}
            local recTyVarMp  : _
            local knExprTy    : {Ty}
            local knTailTy    : _
            local _tup203     : {(Bool,Ty,Ty)}
            local recKnTlTy   : {Ty}
            local lUniq3      : {UID}
            local prUid       : {PredOccId}
            local knRowTy     : _
            local prOccL      : {[PredOcc]}
            local hereCnstrMp : _
            intra _tup204     : {(UID,UID,UID,UID)}
            intra range       : {Range}
            intra _tup201     : {(HsName,[HsName])}
         visit 7:
            local pp          : _
            local offset      : _
            local rcexpr      : _
            local knRecHasLab : {Bool}
            local ty          : {Ty}
            intra nm          : {HsName}
            intra positionalNm : {HsName}
            intra foKnRec     : {FIOut}
            intra range       : {Range}
            intra prUid       : {PredOccId}
            intra _tup203     : {(Bool,Ty,Ty)}
      alternative Upd:
         child hsrange        : {Range}
         child recExpr        : RecExpr 
         child nm             : {HsName}
         child expr           : Expr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            intra _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            local lUniq3      : {UID}
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local knFIOpts    : {FIOpts}
            local _tup205     : _
            local knRecTy     : {Ty}
            local foKnRec     : {FIOut}
            local recTyVarMp  : _
            local knExprTy    : {Ty}
            local knTailTy    : _
            local _tup206     : {(Bool,Ty,Ty)}
            local recKnTlTy   : {Ty}
            local lUniq4      : {UID}
            local prUid       : {PredOccId}
            local knRowTy     : _
            local prOccL      : {[PredOcc]}
            local hereCnstrMp : _
            intra _tup207     : {(UID,UID,UID,UID,UID)}
            intra range       : {Range}
         visit 7:
            local pp          : _
            local offset      : _
            local rcexpr      : _
            local knRecHasLab : {Bool}
            local ty          : {Ty}
            intra foKnRec     : {FIOut}
            intra range       : {Range}
            intra prUid       : {PredOccId}
            intra _tup206     : {(Bool,Ty,Ty)}
-}
sem_RecExpr_Empty :: Range ->
                     T_RecExpr 

sem_RecExpr_Empty hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_RecExpr_Empty_1 :: T_RecExpr_1 
                  sem_RecExpr_Empty_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup200 | __tup200 `seq` (True) ->
                             (case (__tup200) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_RecExpr_Empty_2 :: T_RecExpr_2 
                                          sem_RecExpr_Empty_2  =
                                              (\ _lhsIkiGam
                                                 _lhsIlexLev
                                                 _lhsIpredSameScopeCounter ->
                                                   _lhsIkiGam `seq`
                                                   (_lhsIlexLev `seq`
                                                    (_lhsIpredSameScopeCounter `seq`
                                                     ((case (_lhsIpredSameScopeCounter) of
                                                       { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                       (case ((let sem_RecExpr_Empty_3 :: T_RecExpr_3 
                                                                   sem_RecExpr_Empty_3  =
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
                                                                                      (case ((let sem_RecExpr_Empty_4 :: T_RecExpr_4 
                                                                                                  sem_RecExpr_Empty_4  =
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
                                                                                                                 (case ((let sem_RecExpr_Empty_5 :: T_RecExpr_5 
                                                                                                                             sem_RecExpr_Empty_5  =
                                                                                                                                 (\ _lhsIdataGam
                                                                                                                                    _lhsIpositionalFldNmL ->
                                                                                                                                      _lhsIdataGam `seq`
                                                                                                                                      (_lhsIpositionalFldNmL `seq`
                                                                                                                                       ((case (Seq.empty) of
                                                                                                                                         { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                         (case (_lhsIpositionalFldNmL) of
                                                                                                                                          { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                          (case ((let sem_RecExpr_Empty_6 :: T_RecExpr_6 
                                                                                                                                                      sem_RecExpr_Empty_6  =
                                                                                                                                                          (\ _lhsIchrStore
                                                                                                                                                             _lhsIclDfGam
                                                                                                                                                             _lhsIfiOpts
                                                                                                                                                             _lhsIknTy
                                                                                                                                                             _lhsItvKiVarMp
                                                                                                                                                             _lhsItyTyGlobFreeTvarS
                                                                                                                                                             _lhsItyVarMp
                                                                                                                                                             _lhsIvalGam
                                                                                                                                                             _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                               _lhsIchrStore `seq`
                                                                                                                                                               (_lhsIclDfGam `seq`
                                                                                                                                                                (_lhsIfiOpts `seq`
                                                                                                                                                                 (_lhsIknTy `seq`
                                                                                                                                                                  (_lhsItvKiVarMp `seq`
                                                                                                                                                                   (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                    (_lhsItyVarMp `seq`
                                                                                                                                                                     (_lhsIvalGam `seq`
                                                                                                                                                                      (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                       ((case (Map.empty) of
                                                                                                                                                                         { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                         (case (Map.empty) of
                                                                                                                                                                          { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                          (case (Set.empty) of
                                                                                                                                                                           { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                           (case (__tup200) of
                                                                                                                                                                            { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                                            (case (defaultFIEnv
                                                                                                                                                                                       { feEHCOpts = _lhsIopts
                                                                                                                                                                                       , fePredScope = _lhsIpredScope
                                                                                                                                                                                       , feTyGam = _lhsItyGam
                                                                                                                                                                                       , fePolGam = _lhsIpolGam
                                                                                                                                                                                       , feRange = _range
                                                                                                                                                                                       }) of
                                                                                                                                                                             { _fe | _fe `seq` (True) ->
                                                                                                                                                                             (case (fitsIn (_lhsIfiOpts) _fe _lUniq _lhsItyVarMp tyRecEmpty _lhsIknTy) of
                                                                                                                                                                              { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                              (case (foVarMp _fo_ `varUpd` _lhsItyVarMp) of
                                                                                                                                                                               { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                               (case ((let sem_RecExpr_Empty_7 :: T_RecExpr_7 
                                                                                                                                                                                           sem_RecExpr_Empty_7  =
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
                                                                                                                                                                                                             (case (hsnORec >|< hsnCRec) of
                                                                                                                                                                                                              { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                              (case (rngLift _range mkNestErr' _pp [foErrSq _fo_]) of
                                                                                                                                                                                                               { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                               (case (hsnRowEmpty) of
                                                                                                                                                                                                                { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                (case ([]) of
                                                                                                                                                                                                                 { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                 (case (Map.empty) of
                                                                                                                                                                                                                  { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                  (case (emptyVarMp) of
                                                                                                                                                                                                                   { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                   (case (True) of
                                                                                                                                                                                                                    { _lhsOisExtFromEmpty | _lhsOisExtFromEmpty `seq` (True) ->
                                                                                                                                                                                                                    (case (_pp) of
                                                                                                                                                                                                                     { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                     (case ([]) of
                                                                                                                                                                                                                      { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                      (case (CExpr_Tup CTagRec) of
                                                                                                                                                                                                                       { _recCExpr | _recCExpr `seq` (True) ->
                                                                                                                                                                                                                       (case (_recCExpr) of
                                                                                                                                                                                                                        { _lhsOrecCExpr | _lhsOrecCExpr `seq` (True) ->
                                                                                                                                                                                                                        (case (foTy _fo_) of
                                                                                                                                                                                                                         { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                         (case (_ty) of
                                                                                                                                                                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                          ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOextNm,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOisExtFromEmpty,_lhsOpp,_lhsOppL,_lhsOrecCExpr,_lhsOty) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                       in  sem_RecExpr_Empty_7)) of
                                                                                                                                                                                { ( sem_RecExpr_7) | True ->
                                                                                                                                                                                ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOtyVarMp,sem_RecExpr_7) }) }) }) }) }) }) }) })))))))))))
                                                                                                                                                  in  sem_RecExpr_Empty_6)) of
                                                                                                                                           { ( sem_RecExpr_6) | True ->
                                                                                                                                           ( _lhsOchrInstDeclSq,_lhsOpositionalFldNmL,sem_RecExpr_6) }) }) }))))
                                                                                                                         in  sem_RecExpr_Empty_5)) of
                                                                                                                  { ( sem_RecExpr_5) | True ->
                                                                                                                  ( _lhsOgathDataGam,sem_RecExpr_5) }) })))))))
                                                                                              in  sem_RecExpr_Empty_4)) of
                                                                                       { ( sem_RecExpr_4) | True ->
                                                                                       ( _lhsOkiVarMp,_lhsOpolVarMp,sem_RecExpr_4) }) }) }))))))))))
                                                               in  sem_RecExpr_Empty_3)) of
                                                        { ( sem_RecExpr_3) | True ->
                                                        ( _lhsOpredSameScopeCounter,sem_RecExpr_3) }) })))))
                                      in  sem_RecExpr_Empty_2)) of
                               { ( sem_RecExpr_2) | True ->
                               ( _lhsOgUniq,sem_RecExpr_2) }) }) })))
              in  sem_RecExpr_Empty_1)) of
       { ( sem_RecExpr_1) | True ->
       ( _lhsOrange,sem_RecExpr_1) }) }) })

sem_RecExpr_Expr :: Range ->
                    T_Expr  ->
                    T_RecExpr 

sem_RecExpr_Expr hsrange_ expr_  | hsrange_ `seq` (expr_ `seq` (True)) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (rangeUnions [hsrange_, _exprIrange   , _exprIrange  ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_RecExpr_Expr_1 :: T_RecExpr_1 
                       sem_RecExpr_Expr_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _exprOgUniq | _exprOgUniq `seq` (True) ->
                                  (case (expr_1 _exprOgUniq ) of
                                   { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                       (case (_exprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_RecExpr_Expr_2 :: T_RecExpr_2 
                                                    sem_RecExpr_Expr_2  =
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
                                                                          (case ((let sem_RecExpr_Expr_3 :: T_RecExpr_3 
                                                                                      sem_RecExpr_Expr_3  =
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
                                                                                                                      (case ((let sem_RecExpr_Expr_4 :: T_RecExpr_4 
                                                                                                                                  sem_RecExpr_Expr_4  =
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
                                                                                                                                                           (case ((let sem_RecExpr_Expr_5 :: T_RecExpr_5 
                                                                                                                                                                       sem_RecExpr_Expr_5  =
                                                                                                                                                                           (\ _lhsIdataGam
                                                                                                                                                                              _lhsIpositionalFldNmL ->
                                                                                                                                                                                _lhsIdataGam `seq`
                                                                                                                                                                                (_lhsIpositionalFldNmL `seq`
                                                                                                                                                                                 ((case (_lhsIdataGam) of
                                                                                                                                                                                   { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                   (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                    { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                        (case (_exprIchrInstDeclSq) of
                                                                                                                                                                                         { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                         (case (_lhsIpositionalFldNmL) of
                                                                                                                                                                                          { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                          (case ((let sem_RecExpr_Expr_6 :: T_RecExpr_6 
                                                                                                                                                                                                      sem_RecExpr_Expr_6  =
                                                                                                                                                                                                          (\ _lhsIchrStore
                                                                                                                                                                                                             _lhsIclDfGam
                                                                                                                                                                                                             _lhsIfiOpts
                                                                                                                                                                                                             _lhsIknTy
                                                                                                                                                                                                             _lhsItvKiVarMp
                                                                                                                                                                                                             _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                             _lhsItyVarMp
                                                                                                                                                                                                             _lhsIvalGam
                                                                                                                                                                                                             _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                               _lhsIchrStore `seq`
                                                                                                                                                                                                               (_lhsIclDfGam `seq`
                                                                                                                                                                                                                (_lhsIfiOpts `seq`
                                                                                                                                                                                                                 (_lhsIknTy `seq`
                                                                                                                                                                                                                  (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                   (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                    (_lhsItyVarMp `seq`
                                                                                                                                                                                                                     (_lhsIvalGam `seq`
                                                                                                                                                                                                                      (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                       ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                         { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                         (case (_lhsIvalGam) of
                                                                                                                                                                                                                          { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                          (case (_lhsItyVarMp) of
                                                                                                                                                                                                                           { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                           (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                            { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
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
                                                                                                                                                                                                                                 (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                  { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                      (case (_exprIgathCnstrMp) of
                                                                                                                                                                                                                                       { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                       (case (_exprIgathRangeMp) of
                                                                                                                                                                                                                                        { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                        (case (_exprInoLetQuantTyVarIdS) of
                                                                                                                                                                                                                                         { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                                                         (case (_exprItyVarMp) of
                                                                                                                                                                                                                                          { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                          (case ((let sem_RecExpr_Expr_7 :: T_RecExpr_7 
                                                                                                                                                                                                                                                      sem_RecExpr_Expr_7  =
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
                                                                                                                                                                                                                                                                                     (case (_exprIerrSq) of
                                                                                                                                                                                                                                                                                      { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                       { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                                                                                        { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_exprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                          { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (False) of
                                                                                                                                                                                                                                                                                           { _lhsOisExtFromEmpty | _lhsOisExtFromEmpty `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_exprIpp) of
                                                                                                                                                                                                                                                                                            { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_pp) of
                                                                                                                                                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case ([]) of
                                                                                                                                                                                                                                                                                              { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_exprIcexpr) of
                                                                                                                                                                                                                                                                                               { _recCExpr | _recCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (_recCExpr) of
                                                                                                                                                                                                                                                                                                { _lhsOrecCExpr | _lhsOrecCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (_exprIty) of
                                                                                                                                                                                                                                                                                                 { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOextNm,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOisExtFromEmpty,_lhsOpp,_lhsOppL,_lhsOrecCExpr,_lhsOty) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                  in  sem_RecExpr_Expr_7)) of
                                                                                                                                                                                                                                           { ( sem_RecExpr_7) | True ->
                                                                                                                                                                                                                                           ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOtyVarMp,sem_RecExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                                                                                                                                                  in  sem_RecExpr_Expr_6)) of
                                                                                                                                                                                           { ( sem_RecExpr_6) | True ->
                                                                                                                                                                                           ( _lhsOchrInstDeclSq,_lhsOpositionalFldNmL,sem_RecExpr_6) }) }) }) }) }))))
                                                                                                                                                                   in  sem_RecExpr_Expr_5)) of
                                                                                                                                                            { ( sem_RecExpr_5) | True ->
                                                                                                                                                            ( _lhsOgathDataGam,sem_RecExpr_5) }) }) }) }) }) }) }) })))))))
                                                                                                                              in  sem_RecExpr_Expr_4)) of
                                                                                                                       { ( sem_RecExpr_4) | True ->
                                                                                                                       ( _lhsOkiVarMp,_lhsOpolVarMp,sem_RecExpr_4) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                  in  sem_RecExpr_Expr_3)) of
                                                                           { ( sem_RecExpr_3) | True ->
                                                                           ( _lhsOpredSameScopeCounter,sem_RecExpr_3) }) }) }) }) }) }) })))))
                                                in  sem_RecExpr_Expr_2)) of
                                         { ( sem_RecExpr_2) | True ->
                                         ( _lhsOgUniq,sem_RecExpr_2) }) }) }) })))
                   in  sem_RecExpr_Expr_1)) of
            { ( sem_RecExpr_1) | True ->
            ( _lhsOrange,sem_RecExpr_1) }) }) }) })

sem_RecExpr_Ext :: Range ->
                   T_RecExpr  ->
                   (Maybe HsName) ->
                   T_Expr  ->
                   T_RecExpr 

sem_RecExpr_Ext hsrange_ recExpr_ mbNm_ expr_  | hsrange_ `seq` (recExpr_ `seq` (mbNm_ `seq` (expr_ `seq` (True)))) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (recExpr_ ) of
          { ( _recExprIrange,recExpr_1) | True ->
              (case (rangeUnions [hsrange_, _recExprIrange, _exprIrange  ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_RecExpr_Ext_1 :: T_RecExpr_1 
                            sem_RecExpr_Ext_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> (__cont, lUniq,lUniq2,lUniq3)}}} )) of
                                       { __tup204 | __tup204 `seq` (True) ->
                                       (case (__tup204) of
                                        { (_recExprOgUniq,_,_,_) | _recExprOgUniq `seq` (True) ->
                                        (case (recExpr_1 _recExprOgUniq ) of
                                         { ( _recExprIgUniq,recExpr_2) | True ->
                                             (case (_recExprIgUniq) of
                                              { _exprOgUniq | _exprOgUniq `seq` (True) ->
                                              (case (expr_1 _exprOgUniq ) of
                                               { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                                   (case (_exprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_RecExpr_Ext_2 :: T_RecExpr_2 
                                                                sem_RecExpr_Ext_2  =
                                                                    (\ _lhsIkiGam
                                                                       _lhsIlexLev
                                                                       _lhsIpredSameScopeCounter ->
                                                                         _lhsIkiGam `seq`
                                                                         (_lhsIlexLev `seq`
                                                                          (_lhsIpredSameScopeCounter `seq`
                                                                           ((case (_lhsIpredSameScopeCounter) of
                                                                             { _recExprOpredSameScopeCounter | _recExprOpredSameScopeCounter `seq` (True) ->
                                                                             (case (_lhsIlexLev) of
                                                                              { _recExprOlexLev | _recExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _recExprOkiGam | _recExprOkiGam `seq` (True) ->
                                                                               (case (recExpr_2 _recExprOkiGam _recExprOlexLev _recExprOpredSameScopeCounter ) of
                                                                                { ( _recExprIpredSameScopeCounter,recExpr_3) | True ->
                                                                                    (case (_recExprIpredSameScopeCounter) of
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
                                                                                              (case ((let sem_RecExpr_Ext_3 :: T_RecExpr_3 
                                                                                                          sem_RecExpr_Ext_3  =
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
                                                                                                                                { _recExprOtyKiGlobFreeTvarS | _recExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                (case (_lhsItyKiGam) of
                                                                                                                                 { _recExprOtyKiGam | _recExprOtyKiGam `seq` (True) ->
                                                                                                                                 (case (_lhsItyGam) of
                                                                                                                                  { _recExprOtyGam | _recExprOtyGam `seq` (True) ->
                                                                                                                                  (case (_lhsIpredScope) of
                                                                                                                                   { _recExprOpredScope | _recExprOpredScope `seq` (True) ->
                                                                                                                                   (case (_lhsIpolVarMp) of
                                                                                                                                    { _recExprOpolVarMp | _recExprOpolVarMp `seq` (True) ->
                                                                                                                                    (case (_lhsIpolGam) of
                                                                                                                                     { _recExprOpolGam | _recExprOpolGam `seq` (True) ->
                                                                                                                                     (case (_lhsIopts) of
                                                                                                                                      { _recExprOopts | _recExprOopts `seq` (True) ->
                                                                                                                                      (case (_lhsIkiVarMp) of
                                                                                                                                       { _recExprOkiVarMp | _recExprOkiVarMp `seq` (True) ->
                                                                                                                                       (case (recExpr_3 _recExprOkiVarMp _recExprOopts _recExprOpolGam _recExprOpolVarMp _recExprOpredScope _recExprOtyGam _recExprOtyKiGam _recExprOtyKiGlobFreeTvarS ) of
                                                                                                                                        { ( _recExprIkiVarMp,_recExprIpolVarMp,recExpr_4) | True ->
                                                                                                                                            (case (_recExprIpolVarMp) of
                                                                                                                                             { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                                                                                                             (case (_lhsIpolGam) of
                                                                                                                                              { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                                                                                                                              (case (_lhsIopts) of
                                                                                                                                               { _exprOopts | _exprOopts `seq` (True) ->
                                                                                                                                               (case (_recExprIkiVarMp) of
                                                                                                                                                { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                                                                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                                                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                                                                                                     (case (_exprIkiVarMp) of
                                                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                      (case (_exprIpolVarMp) of
                                                                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                       (case ((let sem_RecExpr_Ext_4 :: T_RecExpr_4 
                                                                                                                                                                   sem_RecExpr_Ext_4  =
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
                                                                                                                                                                                   { _recExprOtyTyTySigFreeTvarS | _recExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                   (case (_lhsIgathDataGam) of
                                                                                                                                                                                    { _recExprOgathDataGam | _recExprOgathDataGam `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                     { _recExprOfinTyKiGam | _recExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                      { _recExprOfinKiVarMp | _recExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                      (case (_lhsIclGam) of
                                                                                                                                                                                       { _recExprOclGam | _recExprOclGam `seq` (True) ->
                                                                                                                                                                                       (case (recExpr_4 _recExprOclGam _recExprOfinKiVarMp _recExprOfinTyKiGam _recExprOgathDataGam _recExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                        { ( _recExprIgathDataGam,recExpr_5) | True ->
                                                                                                                                                                                            (case (_recExprIgathDataGam) of
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
                                                                                                                                                                                                      (case ((let sem_RecExpr_Ext_5 :: T_RecExpr_5 
                                                                                                                                                                                                                  sem_RecExpr_Ext_5  =
                                                                                                                                                                                                                      (\ _lhsIdataGam
                                                                                                                                                                                                                         _lhsIpositionalFldNmL ->
                                                                                                                                                                                                                           _lhsIdataGam `seq`
                                                                                                                                                                                                                           (_lhsIpositionalFldNmL `seq`
                                                                                                                                                                                                                            ((case (_lhsIdataGam) of
                                                                                                                                                                                                                              { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                                                              (case (_lhsIdataGam) of
                                                                                                                                                                                                                               { _recExprOdataGam | _recExprOdataGam `seq` (True) ->
                                                                                                                                                                                                                               (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                                                                { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                                                                    (case (_lhsIpositionalFldNmL) of
                                                                                                                                                                                                                                     { _recExprOpositionalFldNmL | _recExprOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                                                     (case (recExpr_5 _recExprOdataGam _recExprOpositionalFldNmL ) of
                                                                                                                                                                                                                                      { ( _recExprIchrInstDeclSq,_recExprIpositionalFldNmL,recExpr_6) | True ->
                                                                                                                                                                                                                                          (case (_recExprIchrInstDeclSq `Seq.union` _exprIchrInstDeclSq) of
                                                                                                                                                                                                                                           { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                                                           (case (hdAndTl _recExprIpositionalFldNmL) of
                                                                                                                                                                                                                                            { __tup201 | __tup201 `seq` (True) ->
                                                                                                                                                                                                                                            (case (__tup201) of
                                                                                                                                                                                                                                             { (_,_lhsOpositionalFldNmL) | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                                                             (case ((let sem_RecExpr_Ext_6 :: T_RecExpr_6 
                                                                                                                                                                                                                                                         sem_RecExpr_Ext_6  =
                                                                                                                                                                                                                                                             (\ _lhsIchrStore
                                                                                                                                                                                                                                                                _lhsIclDfGam
                                                                                                                                                                                                                                                                _lhsIfiOpts
                                                                                                                                                                                                                                                                _lhsIknTy
                                                                                                                                                                                                                                                                _lhsItvKiVarMp
                                                                                                                                                                                                                                                                _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                _lhsItyVarMp
                                                                                                                                                                                                                                                                _lhsIvalGam
                                                                                                                                                                                                                                                                _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                  _lhsIchrStore `seq`
                                                                                                                                                                                                                                                                  (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                   (_lhsIfiOpts `seq`
                                                                                                                                                                                                                                                                    (_lhsIknTy `seq`
                                                                                                                                                                                                                                                                     (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                      (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                       (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                        (_lhsIvalGam `seq`
                                                                                                                                                                                                                                                                         (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                          ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                            { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                             { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                              { _recExprOvalTyGlobFreeTvarS | _recExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                               { _recExprOvalGam | _recExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                { _recExprOtyTyGlobFreeTvarS | _recExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                 { _recExprOtvKiVarMp | _recExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                  { _recExprOclDfGam | _recExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                   { _recExprOchrStore | _recExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (__tup204) of
                                                                                                                                                                                                                                                                                    { (_,_,_lUniq2,_) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (__tup204) of
                                                                                                                                                                                                                                                                                     { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (defaultFIEnv
                                                                                                                                                                                                                                                                                                { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                                                                                                , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                                                                                                , feTyGam = _lhsItyGam
                                                                                                                                                                                                                                                                                                , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                                                                                , feRange = _range
                                                                                                                                                                                                                                                                                                }) of
                                                                                                                                                                                                                                                                                      { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (__tup201) of
                                                                                                                                                                                                                                                                                       { (_positionalNm,_) | _positionalNm `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (maybe _positionalNm id mbNm_) of
                                                                                                                                                                                                                                                                                        { _nm | _nm `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_lhsIfiOpts {fioNoRLabElimFor = _nm : fioNoRLabElimFor _lhsIfiOpts}) of
                                                                                                                                                                                                                                                                                         { _recExprOfiOpts | _recExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                          { _knFIOpts | _knFIOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (let  [r,e] = mkNewTyVarL 2 _lUniq
                                                                                                                                                                                                                                                                                                      tl = hsnRec `mkConApp` [r]
                                                                                                                                                                                                                                                                                                 in   (r, tl `mkTyRecExt` [(_nm,e)], tl, e)) of
                                                                                                                                                                                                                                                                                           { __tup202 | __tup202 `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (__tup202) of
                                                                                                                                                                                                                                                                                            { (_,_knRecTy,_,_) | _knRecTy `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (fitsIn _knFIOpts _fe _lUniq2 _lhsItyVarMp _knRecTy _lhsIknTy) of
                                                                                                                                                                                                                                                                                             { _foKnRec | _foKnRec `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (foVarMp _foKnRec `varUpd` _lhsItyVarMp) of
                                                                                                                                                                                                                                                                                              { _recTyVarMp | _recTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (__tup202) of
                                                                                                                                                                                                                                                                                               { (_,_,_,_knExprTy) | _knExprTy `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (__tup202) of
                                                                                                                                                                                                                                                                                                { (_,_,_knTailTy,_) | _knTailTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (maybe (False,_knTailTy,_knExprTy) (\(r,e) -> (True,r,e)) $ tyRecExtrWithLkup (varmpTyLookupCyc2 _recTyVarMp) _nm $ foTy _foKnRec) of
                                                                                                                                                                                                                                                                                                 { __tup203 | __tup203 `seq` (True) ->
                                                                                                                                                                                                                                                                                                 (case (__tup203) of
                                                                                                                                                                                                                                                                                                  { (_,_recKnTlTy,_) | _recKnTlTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                  (case (_recKnTlTy) of
                                                                                                                                                                                                                                                                                                   { _recExprOknTy | _recExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                   (case (_recTyVarMp) of
                                                                                                                                                                                                                                                                                                    { _recExprOtyVarMp | _recExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                    (case (recExpr_6 _recExprOchrStore _recExprOclDfGam _recExprOfiOpts _recExprOknTy _recExprOtvKiVarMp _recExprOtyTyGlobFreeTvarS _recExprOtyVarMp _recExprOvalGam _recExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                     { ( _recExprIgathCnstrMp,_recExprIgathRangeMp,_recExprInoLetQuantTyVarIdS,_recExprItyVarMp,recExpr_7) | True ->
                                                                                                                                                                                                                                                                                                         (case (_recExprItyVarMp) of
                                                                                                                                                                                                                                                                                                          { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                           { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                           (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                            { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                            (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                             { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                              { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (__tup204) of
                                                                                                                                                                                                                                                                                                               { (_,_,_,_lUniq3) | _lUniq3 `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (mkPrIdCHR _lUniq3) of
                                                                                                                                                                                                                                                                                                                { _prUid | _prUid `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (__tup202) of
                                                                                                                                                                                                                                                                                                                 { (_knRowTy,_,_,_) | _knRowTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case ([rngLift _range mkPredOccRng (Pred_Lacks _knRowTy (Label_Lab _nm)) _prUid _lhsIpredScope]) of
                                                                                                                                                                                                                                                                                                                  { _prOccL | _prOccL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (gathPredLToProveCnstrMp _prOccL) of
                                                                                                                                                                                                                                                                                                                   { _hereCnstrMp | _hereCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                                                    { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (__tup203) of
                                                                                                                                                                                                                                                                                                                     { (_,_,_exprOknTy) | _exprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                      { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                                                                                                          (case (cnstrMpUnions [_hereCnstrMp, _recExprIgathCnstrMp, _exprIgathCnstrMp]) of
                                                                                                                                                                                                                                                                                                                           { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                           (case (_recExprIgathRangeMp `Map.union` _exprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                                            { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                            (case (_recExprInoLetQuantTyVarIdS `Set.union` _exprInoLetQuantTyVarIdS) of
                                                                                                                                                                                                                                                                                                                             { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                             (case (_exprItyVarMp) of
                                                                                                                                                                                                                                                                                                                              { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                              (case ((let sem_RecExpr_Ext_7 :: T_RecExpr_7 
                                                                                                                                                                                                                                                                                                                                          sem_RecExpr_Ext_7  =
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
                                                                                                                                                                                                                                                                                                                                                              { _recExprOrangeMp | _recExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                              (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                               { _recExprOfinValGam | _recExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                               (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                                { _recExprOfinTyVarMp | _recExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                 { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                  { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                   { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                   (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                    { _recExprOchrScopeBindMp | _recExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                    (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                     { _recExprOchrEvidBindMp | _recExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                     (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                      { _recExprOcSubst | _recExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                      (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                       { _recExprOmoduleNm | _recExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                       (case (recExpr_7 _recExprOcSubst _recExprOchrEvidBindMp _recExprOchrScopeBindMp _recExprOfinTyVarMp _recExprOfinValGam _recExprOmoduleNm _recExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                        { ( _recExprIallErrSq,_recExprIcSubst,_recExprIerrSq,_recExprIextNm,_recExprIfuCExprL,_recExprIgathMentrelFilterMp,_recExprIgathTvKiVarMp,_recExprIisExtFromEmpty,_recExprIpp,_recExprIppL,_recExprIrecCExpr,_recExprIty) | True ->
                                                                                                                                                                                                                                                                                                                                                                            (case (_recExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                             { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                             (case (True) of
                                                                                                                                                                                                                                                                                                                                                                              { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                              (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                               { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                                                                                                                                                                                                                                                                   (case (_recExprIallErrSq `Seq.union` _exprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                                    { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                    (case (_exprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                     { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                     (case (ppFld "=" (Just _positionalNm) _nm (pp _nm) _exprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                      (case (Seq.unions [ _recExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                        , rngLift _range mkNestErr' _pp [_exprIerrSq, foErrSq _foKnRec]
                                                                                                                                                                                                                                                                                                                                                                                                        ]) of
                                                                                                                                                                                                                                                                                                                                                                                       { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                       (case (_recExprIextNm) of
                                                                                                                                                                                                                                                                                                                                                                                        { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                        (case (cSubstApp _exprIcSubst $ acoreNmHolePred _prUid) of
                                                                                                                                                                                                                                                                                                                                                                                         { _offset | _offset `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                         (case (CExpr_TupIns _recExprIrecCExpr CTagRec _nm _offset _exprIcexpr) of
                                                                                                                                                                                                                                                                                                                                                                                          { _rcexpr | _rcexpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                          (case (__tup203) of
                                                                                                                                                                                                                                                                                                                                                                                           { (_knRecHasLab,_,_) | _knRecHasLab `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                           (case (if _knRecHasLab
                                                                                                                                                                                                                                                                                                                                                                                                  then (_nm,(_rcexpr,Nothing)) : _recExprIfuCExprL
                                                                                                                                                                                                                                                                                                                                                                                                  else _recExprIfuCExprL) of
                                                                                                                                                                                                                                                                                                                                                                                            { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                            (case (_recExprIgathMentrelFilterMp `mentrelFilterMpUnion` _exprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                                             { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                             (case (_recExprIgathTvKiVarMp `varmpUnion` _exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                              { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                              (case (_recExprIisExtFromEmpty) of
                                                                                                                                                                                                                                                                                                                                                                                               { _lhsOisExtFromEmpty | _lhsOisExtFromEmpty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                               (case (_recExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                                { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                (case (_pp : _recExprIppL) of
                                                                                                                                                                                                                                                                                                                                                                                                 { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                 (case (_recExprIrecCExpr) of
                                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOrecCExpr | _lhsOrecCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                  (case (foTy _foKnRec) of
                                                                                                                                                                                                                                                                                                                                                                                                   { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                   (case (_ty) of
                                                                                                                                                                                                                                                                                                                                                                                                    { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                    ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOextNm,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOisExtFromEmpty,_lhsOpp,_lhsOppL,_lhsOrecCExpr,_lhsOty) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                                                                                                      in  sem_RecExpr_Ext_7)) of
                                                                                                                                                                                                                                                                                                                               { ( sem_RecExpr_7) | True ->
                                                                                                                                                                                                                                                                                                                               ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOtyVarMp,sem_RecExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                                                                                                                                                                                                     in  sem_RecExpr_Ext_6)) of
                                                                                                                                                                                                                                              { ( sem_RecExpr_6) | True ->
                                                                                                                                                                                                                                              ( _lhsOchrInstDeclSq,_lhsOpositionalFldNmL,sem_RecExpr_6) }) }) }) }) }) }) }) }) }))))
                                                                                                                                                                                                              in  sem_RecExpr_Ext_5)) of
                                                                                                                                                                                                       { ( sem_RecExpr_5) | True ->
                                                                                                                                                                                                       ( _lhsOgathDataGam,sem_RecExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                               in  sem_RecExpr_Ext_4)) of
                                                                                                                                                        { ( sem_RecExpr_4) | True ->
                                                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,sem_RecExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                      in  sem_RecExpr_Ext_3)) of
                                                                                               { ( sem_RecExpr_3) | True ->
                                                                                               ( _lhsOpredSameScopeCounter,sem_RecExpr_3) }) }) }) }) }) }) }) }) }) }) })))))
                                                            in  sem_RecExpr_Ext_2)) of
                                                     { ( sem_RecExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_RecExpr_2) }) }) }) }) }) }) })))
                        in  sem_RecExpr_Ext_1)) of
                 { ( sem_RecExpr_1) | True ->
                 ( _lhsOrange,sem_RecExpr_1) }) }) }) }) })

sem_RecExpr_Upd :: Range ->
                   T_RecExpr  ->
                   HsName ->
                   T_Expr  ->
                   T_RecExpr 

sem_RecExpr_Upd hsrange_ recExpr_ nm_ expr_  | hsrange_ `seq` (recExpr_ `seq` (nm_ `seq` (expr_ `seq` (True)))) =
    (case (expr_ ) of
     { ( _exprIrange,expr_1) | True ->
         (case (recExpr_ ) of
          { ( _recExprIrange,recExpr_1) | True ->
              (case (rangeUnions [hsrange_, _recExprIrange, _exprIrange  ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_RecExpr_Upd_1 :: T_RecExpr_1 
                            sem_RecExpr_Upd_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> case nextUnique __cont of { (__cont, lUniq4) -> (__cont, lUniq,lUniq2,lUniq3,lUniq4)}}}} )) of
                                       { __tup207 | __tup207 `seq` (True) ->
                                       (case (__tup207) of
                                        { (_recExprOgUniq,_,_,_,_) | _recExprOgUniq `seq` (True) ->
                                        (case (recExpr_1 _recExprOgUniq ) of
                                         { ( _recExprIgUniq,recExpr_2) | True ->
                                             (case (_recExprIgUniq) of
                                              { _exprOgUniq | _exprOgUniq `seq` (True) ->
                                              (case (expr_1 _exprOgUniq ) of
                                               { ( _exprIgUniq,_exprIhasInstDecl,expr_2) | True ->
                                                   (case (_exprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_RecExpr_Upd_2 :: T_RecExpr_2 
                                                                sem_RecExpr_Upd_2  =
                                                                    (\ _lhsIkiGam
                                                                       _lhsIlexLev
                                                                       _lhsIpredSameScopeCounter ->
                                                                         _lhsIkiGam `seq`
                                                                         (_lhsIlexLev `seq`
                                                                          (_lhsIpredSameScopeCounter `seq`
                                                                           ((case (_lhsIpredSameScopeCounter) of
                                                                             { _recExprOpredSameScopeCounter | _recExprOpredSameScopeCounter `seq` (True) ->
                                                                             (case (_lhsIlexLev) of
                                                                              { _recExprOlexLev | _recExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _recExprOkiGam | _recExprOkiGam `seq` (True) ->
                                                                               (case (recExpr_2 _recExprOkiGam _recExprOlexLev _recExprOpredSameScopeCounter ) of
                                                                                { ( _recExprIpredSameScopeCounter,recExpr_3) | True ->
                                                                                    (case (_recExprIpredSameScopeCounter) of
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
                                                                                              (case ((let sem_RecExpr_Upd_3 :: T_RecExpr_3 
                                                                                                          sem_RecExpr_Upd_3  =
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
                                                                                                                                { _recExprOtyKiGlobFreeTvarS | _recExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                (case (_lhsItyKiGam) of
                                                                                                                                 { _recExprOtyKiGam | _recExprOtyKiGam `seq` (True) ->
                                                                                                                                 (case (_lhsItyGam) of
                                                                                                                                  { _recExprOtyGam | _recExprOtyGam `seq` (True) ->
                                                                                                                                  (case (_lhsIpredScope) of
                                                                                                                                   { _recExprOpredScope | _recExprOpredScope `seq` (True) ->
                                                                                                                                   (case (_lhsIpolVarMp) of
                                                                                                                                    { _recExprOpolVarMp | _recExprOpolVarMp `seq` (True) ->
                                                                                                                                    (case (_lhsIpolGam) of
                                                                                                                                     { _recExprOpolGam | _recExprOpolGam `seq` (True) ->
                                                                                                                                     (case (_lhsIopts) of
                                                                                                                                      { _recExprOopts | _recExprOopts `seq` (True) ->
                                                                                                                                      (case (_lhsIkiVarMp) of
                                                                                                                                       { _recExprOkiVarMp | _recExprOkiVarMp `seq` (True) ->
                                                                                                                                       (case (recExpr_3 _recExprOkiVarMp _recExprOopts _recExprOpolGam _recExprOpolVarMp _recExprOpredScope _recExprOtyGam _recExprOtyKiGam _recExprOtyKiGlobFreeTvarS ) of
                                                                                                                                        { ( _recExprIkiVarMp,_recExprIpolVarMp,recExpr_4) | True ->
                                                                                                                                            (case (_recExprIpolVarMp) of
                                                                                                                                             { _exprOpolVarMp | _exprOpolVarMp `seq` (True) ->
                                                                                                                                             (case (_lhsIpolGam) of
                                                                                                                                              { _exprOpolGam | _exprOpolGam `seq` (True) ->
                                                                                                                                              (case (_lhsIopts) of
                                                                                                                                               { _exprOopts | _exprOopts `seq` (True) ->
                                                                                                                                               (case (_recExprIkiVarMp) of
                                                                                                                                                { _exprOkiVarMp | _exprOkiVarMp `seq` (True) ->
                                                                                                                                                (case (expr_3 _exprOkiVarMp _exprOopts _exprOpolGam _exprOpolVarMp _exprOpredScope _exprOtyGam _exprOtyKiGam _exprOtyKiGlobFreeTvarS ) of
                                                                                                                                                 { ( _exprIkiVarMp,_exprIpolVarMp,expr_4) | True ->
                                                                                                                                                     (case (_exprIkiVarMp) of
                                                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                      (case (_exprIpolVarMp) of
                                                                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                       (case ((let sem_RecExpr_Upd_4 :: T_RecExpr_4 
                                                                                                                                                                   sem_RecExpr_Upd_4  =
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
                                                                                                                                                                                   { _recExprOtyTyTySigFreeTvarS | _recExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                   (case (_lhsIgathDataGam) of
                                                                                                                                                                                    { _recExprOgathDataGam | _recExprOgathDataGam `seq` (True) ->
                                                                                                                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                     { _recExprOfinTyKiGam | _recExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                      { _recExprOfinKiVarMp | _recExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                      (case (_lhsIclGam) of
                                                                                                                                                                                       { _recExprOclGam | _recExprOclGam `seq` (True) ->
                                                                                                                                                                                       (case (recExpr_4 _recExprOclGam _recExprOfinKiVarMp _recExprOfinTyKiGam _recExprOgathDataGam _recExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                        { ( _recExprIgathDataGam,recExpr_5) | True ->
                                                                                                                                                                                            (case (_recExprIgathDataGam) of
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
                                                                                                                                                                                                      (case ((let sem_RecExpr_Upd_5 :: T_RecExpr_5 
                                                                                                                                                                                                                  sem_RecExpr_Upd_5  =
                                                                                                                                                                                                                      (\ _lhsIdataGam
                                                                                                                                                                                                                         _lhsIpositionalFldNmL ->
                                                                                                                                                                                                                           _lhsIdataGam `seq`
                                                                                                                                                                                                                           (_lhsIpositionalFldNmL `seq`
                                                                                                                                                                                                                            ((case (_lhsIdataGam) of
                                                                                                                                                                                                                              { _exprOdataGam | _exprOdataGam `seq` (True) ->
                                                                                                                                                                                                                              (case (_lhsIdataGam) of
                                                                                                                                                                                                                               { _recExprOdataGam | _recExprOdataGam `seq` (True) ->
                                                                                                                                                                                                                               (case (expr_5 _exprOdataGam ) of
                                                                                                                                                                                                                                { ( _exprIchrClassDeclSq,_exprIchrFIIn,_exprIchrInstDeclSq,_exprIgathClDfGam,expr_6) | True ->
                                                                                                                                                                                                                                    (case (_lhsIpositionalFldNmL) of
                                                                                                                                                                                                                                     { _recExprOpositionalFldNmL | _recExprOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                                                     (case (recExpr_5 _recExprOdataGam _recExprOpositionalFldNmL ) of
                                                                                                                                                                                                                                      { ( _recExprIchrInstDeclSq,_recExprIpositionalFldNmL,recExpr_6) | True ->
                                                                                                                                                                                                                                          (case (_recExprIchrInstDeclSq `Seq.union` _exprIchrInstDeclSq) of
                                                                                                                                                                                                                                           { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                                                           (case (_recExprIpositionalFldNmL) of
                                                                                                                                                                                                                                            { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                                                            (case ((let sem_RecExpr_Upd_6 :: T_RecExpr_6 
                                                                                                                                                                                                                                                        sem_RecExpr_Upd_6  =
                                                                                                                                                                                                                                                            (\ _lhsIchrStore
                                                                                                                                                                                                                                                               _lhsIclDfGam
                                                                                                                                                                                                                                                               _lhsIfiOpts
                                                                                                                                                                                                                                                               _lhsIknTy
                                                                                                                                                                                                                                                               _lhsItvKiVarMp
                                                                                                                                                                                                                                                               _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                               _lhsItyVarMp
                                                                                                                                                                                                                                                               _lhsIvalGam
                                                                                                                                                                                                                                                               _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                 _lhsIchrStore `seq`
                                                                                                                                                                                                                                                                 (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                  (_lhsIfiOpts `seq`
                                                                                                                                                                                                                                                                   (_lhsIknTy `seq`
                                                                                                                                                                                                                                                                    (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                                                                     (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                      (_lhsItyVarMp `seq`
                                                                                                                                                                                                                                                                       (_lhsIvalGam `seq`
                                                                                                                                                                                                                                                                        (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                         ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                           { _exprOvalTyGlobFreeTvarS | _exprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                           (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                            { _exprOvalGam | _exprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                             { _recExprOvalTyGlobFreeTvarS | _recExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_lhsIvalGam) of
                                                                                                                                                                                                                                                                              { _recExprOvalGam | _recExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                               { _recExprOtyTyGlobFreeTvarS | _recExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                { _recExprOtvKiVarMp | _recExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                 { _recExprOfiOpts | _recExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                  { _recExprOclDfGam | _recExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                   { _recExprOchrStore | _recExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (__tup207) of
                                                                                                                                                                                                                                                                                    { (_,_,_,_lUniq3,_) | _lUniq3 `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (__tup207) of
                                                                                                                                                                                                                                                                                     { (_,_,_lUniq2,_,_) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (__tup207) of
                                                                                                                                                                                                                                                                                      { (_,_lUniq,_,_,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (defaultFIEnv
                                                                                                                                                                                                                                                                                                 { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                                                                                                 , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                                                                                                 , feTyGam = _lhsItyGam
                                                                                                                                                                                                                                                                                                 , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                                                                                 , feRange = _range
                                                                                                                                                                                                                                                                                                 }) of
                                                                                                                                                                                                                                                                                       { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                        { _knFIOpts | _knFIOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (let  [r,e] = mkNewTyVarL 2 _lUniq
                                                                                                                                                                                                                                                                                                    tl = hsnRec `mkConApp` [r]
                                                                                                                                                                                                                                                                                               in   (r, tl `mkTyRecExt` [(nm_,e)], tl, e)) of
                                                                                                                                                                                                                                                                                         { __tup205 | __tup205 `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (__tup205) of
                                                                                                                                                                                                                                                                                          { (_,_knRecTy,_,_) | _knRecTy `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (fitsIn _knFIOpts _fe _lUniq2 _lhsItyVarMp _knRecTy _lhsIknTy) of
                                                                                                                                                                                                                                                                                           { _foKnRec | _foKnRec `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (foVarMp _foKnRec `varUpd` _lhsItyVarMp) of
                                                                                                                                                                                                                                                                                            { _recTyVarMp | _recTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (__tup205) of
                                                                                                                                                                                                                                                                                             { (_,_,_,_knExprTy) | _knExprTy `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (__tup205) of
                                                                                                                                                                                                                                                                                              { (_,_,_knTailTy,_) | _knTailTy `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (maybe (False,_knTailTy,_knExprTy) (\(r,e) -> (True,r,e)) $ tyRecExtrWithLkup (varmpTyLookupCyc2 _recTyVarMp) nm_ $ foTy _foKnRec) of
                                                                                                                                                                                                                                                                                               { __tup206 | __tup206 `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (__tup206) of
                                                                                                                                                                                                                                                                                                { (_,_recKnTlTy,_) | _recKnTlTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (_recKnTlTy `mkTyRecExt` [(nm_,mkNewTyVar _lUniq3)]) of
                                                                                                                                                                                                                                                                                                 { _recExprOknTy | _recExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                 (case (_recTyVarMp) of
                                                                                                                                                                                                                                                                                                  { _recExprOtyVarMp | _recExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                  (case (recExpr_6 _recExprOchrStore _recExprOclDfGam _recExprOfiOpts _recExprOknTy _recExprOtvKiVarMp _recExprOtyTyGlobFreeTvarS _recExprOtyVarMp _recExprOvalGam _recExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                   { ( _recExprIgathCnstrMp,_recExprIgathRangeMp,_recExprInoLetQuantTyVarIdS,_recExprItyVarMp,recExpr_7) | True ->
                                                                                                                                                                                                                                                                                                       (case (_recExprItyVarMp) of
                                                                                                                                                                                                                                                                                                        { _exprOtyVarMp | _exprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                         { _exprOtyTyGlobFreeTvarS | _exprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                         (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                          { _exprOtvKiVarMp | _exprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                           { _exprOclDfGam | _exprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                           (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                            { _exprOchrStore | _exprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                            (case (__tup207) of
                                                                                                                                                                                                                                                                                                             { (_,_,_,_,_lUniq4) | _lUniq4 `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case (mkPrIdCHR _lUniq4) of
                                                                                                                                                                                                                                                                                                              { _prUid | _prUid `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (__tup205) of
                                                                                                                                                                                                                                                                                                               { (_knRowTy,_,_,_) | _knRowTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case ([rngLift _range mkPredOccRng (Pred_Lacks _knRowTy (Label_Lab nm_)) _prUid _lhsIpredScope]) of
                                                                                                                                                                                                                                                                                                                { _prOccL | _prOccL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (gathPredLToProveCnstrMp _prOccL) of
                                                                                                                                                                                                                                                                                                                 { _hereCnstrMp | _hereCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                                                                  { _exprOfiOpts | _exprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (__tup206) of
                                                                                                                                                                                                                                                                                                                   { (_,_,_exprOknTy) | _exprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (expr_6 _exprOchrStore _exprOclDfGam _exprOfiOpts _exprOknTy _exprOtvKiVarMp _exprOtyTyGlobFreeTvarS _exprOtyVarMp _exprOvalGam _exprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                    { ( _exprIgathCnstrMp,_exprIgathRangeMp,_exprIgathValGam,_exprInoLetQuantTyVarIdS,_exprIty,_exprItyVarMp,expr_7) | True ->
                                                                                                                                                                                                                                                                                                                        (case (cnstrMpUnions [_hereCnstrMp, _recExprIgathCnstrMp, _exprIgathCnstrMp]) of
                                                                                                                                                                                                                                                                                                                         { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (_recExprIgathRangeMp `Map.union` _exprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                                          { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                          (case (_recExprInoLetQuantTyVarIdS `Set.union` _exprInoLetQuantTyVarIdS) of
                                                                                                                                                                                                                                                                                                                           { _lhsOnoLetQuantTyVarIdS | _lhsOnoLetQuantTyVarIdS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                           (case (_exprItyVarMp) of
                                                                                                                                                                                                                                                                                                                            { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                            (case ((let sem_RecExpr_Upd_7 :: T_RecExpr_7 
                                                                                                                                                                                                                                                                                                                                        sem_RecExpr_Upd_7  =
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
                                                                                                                                                                                                                                                                                                                                                            { _recExprOrangeMp | _recExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                            (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                             { _recExprOfinValGam | _recExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                             (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                              { _recExprOfinTyVarMp | _recExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                              (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                               { _exprOmoduleNm | _exprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                               (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                { _exprOchrScopeBindMp | _exprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                 { _exprOchrEvidBindMp | _exprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                  { _recExprOchrScopeBindMp | _recExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                   { _recExprOchrEvidBindMp | _recExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                   (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                    { _recExprOcSubst | _recExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                    (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                     { _recExprOmoduleNm | _recExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                     (case (recExpr_7 _recExprOcSubst _recExprOchrEvidBindMp _recExprOchrScopeBindMp _recExprOfinTyVarMp _recExprOfinValGam _recExprOmoduleNm _recExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                      { ( _recExprIallErrSq,_recExprIcSubst,_recExprIerrSq,_recExprIextNm,_recExprIfuCExprL,_recExprIgathMentrelFilterMp,_recExprIgathTvKiVarMp,_recExprIisExtFromEmpty,_recExprIpp,_recExprIppL,_recExprIrecCExpr,_recExprIty) | True ->
                                                                                                                                                                                                                                                                                                                                                                          (case (_recExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                           { _exprOcSubst | _exprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                           (case (True) of
                                                                                                                                                                                                                                                                                                                                                                            { _exprOisTopLam | _exprOisTopLam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                            (case (expr_7 _exprOcSubst _exprOchrEvidBindMp _exprOchrScopeBindMp _exprOfinTyVarMp _exprOfinValGam _exprOisTopLam _exprOmoduleNm _exprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                             { ( _exprIallErrSq,_exprIappArgCoeL,_exprIappArgPPL,_exprIappFunCExpr,_exprIappFunNm,_exprIappFunPP,_exprIbackCBindL,_exprIcSubst,_exprIcaseFailS,_exprIcexpr,_exprIerrSq,_exprIfrontCBindL,_exprIfuCExprL,_exprIgathClGam,_exprIgathHiddenExports,_exprIgathKiGam,_exprIgathLamMp,_exprIgathMentrelFilterMp,_exprIgathPolGam,_exprIgathTvKiVarMp,_exprIgathTyGam,_exprIgathTyKiGam,_exprIisNewtype,_exprIlamArgPPL,_exprIlamBodyPP,_exprIletCBindL,_exprIletCBody,_exprIorphanS,_exprIpp) | True ->
                                                                                                                                                                                                                                                                                                                                                                                 (case (_recExprIallErrSq `Seq.union` _exprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                  (case (_exprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                   { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                   (case (ppFld ":=" Nothing nm_ (pp nm_) _exprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                    { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                    (case (Seq.unions [ _recExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                      , rngLift _range mkNestErr' _pp [_exprIerrSq, foErrSq _foKnRec]
                                                                                                                                                                                                                                                                                                                                                                                                      ]) of
                                                                                                                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                     (case (_recExprIextNm) of
                                                                                                                                                                                                                                                                                                                                                                                      { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                      (case (cSubstApp _exprIcSubst $ acoreNmHolePred _prUid) of
                                                                                                                                                                                                                                                                                                                                                                                       { _offset | _offset `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                       (case (CExpr_TupUpd _recExprIrecCExpr CTagRec nm_ _offset _exprIcexpr) of
                                                                                                                                                                                                                                                                                                                                                                                        { _rcexpr | _rcexpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                        (case (__tup206) of
                                                                                                                                                                                                                                                                                                                                                                                         { (_knRecHasLab,_,_) | _knRecHasLab `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                         (case (if _knRecHasLab
                                                                                                                                                                                                                                                                                                                                                                                                then (nm_,(_rcexpr,Nothing)) : _recExprIfuCExprL
                                                                                                                                                                                                                                                                                                                                                                                                else _recExprIfuCExprL) of
                                                                                                                                                                                                                                                                                                                                                                                          { _lhsOfuCExprL | _lhsOfuCExprL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                          (case (_recExprIgathMentrelFilterMp `mentrelFilterMpUnion` _exprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                                           { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                           (case (_recExprIgathTvKiVarMp `varmpUnion` _exprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                            { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                            (case (not _knRecHasLab && _recExprIisExtFromEmpty) of
                                                                                                                                                                                                                                                                                                                                                                                             { _lhsOisExtFromEmpty | _lhsOisExtFromEmpty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                             (case (_recExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                              { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                              (case (_pp : _recExprIppL) of
                                                                                                                                                                                                                                                                                                                                                                                               { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                               (case (_recExprIrecCExpr) of
                                                                                                                                                                                                                                                                                                                                                                                                { _lhsOrecCExpr | _lhsOrecCExpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                (case (foTy _foKnRec) of
                                                                                                                                                                                                                                                                                                                                                                                                 { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                 (case (_ty) of
                                                                                                                                                                                                                                                                                                                                                                                                  { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                  ( _lhsOallErrSq,_lhsOcSubst,_lhsOerrSq,_lhsOextNm,_lhsOfuCExprL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOisExtFromEmpty,_lhsOpp,_lhsOppL,_lhsOrecCExpr,_lhsOty) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))
                                                                                                                                                                                                                                                                                                                                    in  sem_RecExpr_Upd_7)) of
                                                                                                                                                                                                                                                                                                                             { ( sem_RecExpr_7) | True ->
                                                                                                                                                                                                                                                                                                                             ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOnoLetQuantTyVarIdS,_lhsOtyVarMp,sem_RecExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                                                                                                                                                                                                    in  sem_RecExpr_Upd_6)) of
                                                                                                                                                                                                                                             { ( sem_RecExpr_6) | True ->
                                                                                                                                                                                                                                             ( _lhsOchrInstDeclSq,_lhsOpositionalFldNmL,sem_RecExpr_6) }) }) }) }) }) }) }) }))))
                                                                                                                                                                                                              in  sem_RecExpr_Upd_5)) of
                                                                                                                                                                                                       { ( sem_RecExpr_5) | True ->
                                                                                                                                                                                                       ( _lhsOgathDataGam,sem_RecExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                                                                                                                               in  sem_RecExpr_Upd_4)) of
                                                                                                                                                        { ( sem_RecExpr_4) | True ->
                                                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,sem_RecExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                      in  sem_RecExpr_Upd_3)) of
                                                                                               { ( sem_RecExpr_3) | True ->
                                                                                               ( _lhsOpredSameScopeCounter,sem_RecExpr_3) }) }) }) }) }) }) }) }) }) }) })))))
                                                            in  sem_RecExpr_Upd_2)) of
                                                     { ( sem_RecExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_RecExpr_2) }) }) }) }) }) }) })))
                        in  sem_RecExpr_Upd_1)) of
                 { ( sem_RecExpr_1) | True ->
                 ( _lhsOrange,sem_RecExpr_1) }) }) }) }) })


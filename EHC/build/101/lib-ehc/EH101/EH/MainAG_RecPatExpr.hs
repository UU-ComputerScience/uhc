


module EH101.EH.MainAG_RecPatExpr where

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

-- RecPatExpr --------------------------------------------------
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
      chained attribute:
         positionalFldNmL     : [HsName]
      synthesized attributes:
         chrInstDeclSq        : Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)
         hasAFldRef           : Bool
   visit 5:
      inherited attributes:
         fiOpts               : FIOpts
         knTy                 : Ty
         matchOnFld           : Bool
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
         errSq                : ErrSq
         extNm                : HsName
         fsRPatL              : FieldSplitL
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         patCRest             : CPatRest
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Empty:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup208     : {(UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup208     : {(UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup208     : {(UID,UID)}
            intra range       : {Range}
         visit 4:
            intra _tup208     : {(UID,UID)}
            intra range       : {Range}
         visit 5:
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local fo_         : {FIOut}
            intra _tup208     : {(UID,UID)}
            intra range       : {Range}
         visit 6:
            local ty          : {Ty}
            intra fo_         : {FIOut}
            intra range       : {Range}
         visit 7:
            local pp          : _
            intra fo_         : {FIOut}
            intra range       : {Range}
      alternative Expr:
         child hsrange        : {Range}
         child patExpr        : PatExpr 
         visit 0:
            local range       : {Range}
         visit 7:
            local pp          : _
      alternative Ext:
         child hsrange        : {Range}
         child recPatExpr     : RecPatExpr 
         child mbNm           : {Maybe HsName}
         child patExpr        : PatExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup211     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            intra _tup211     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup211     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            local _tup209     : {(HsName,[HsName])}
            intra _tup211     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            local lUniq2      : {UID}
            local lUniq       : {UID}
            local fe          : {FIEnv}
            local _tup210     : {(Ty,Ty)}
            local rowTy       : {Ty}
            local extTy       : {Ty}
            local patTy       : {Ty}
            local positionalNm : {HsName}
            local nm          : {HsName}
            local recTy       : {Ty}
            local fo_         : {FIOut}
            intra _tup211     : {(UID,UID,UID,UID)}
            intra range       : {Range}
            intra _tup209     : {(HsName,[HsName])}
         visit 6:
            local lUniq3      : {UID}
            local prUid       : {PredOccId}
            local prOccL      : {[PredOcc]}
            local hereCnstrMp : _
            local ty          : {Ty}
            intra _tup211     : {(UID,UID,UID,UID)}
            intra nm          : {HsName}
            intra rowTy       : {Ty}
            intra range       : {Range}
            intra fo_         : {FIOut}
            intra positionalNm : {HsName}
         visit 7:
            local pp          : _
            local offset      : _
            intra fo_         : {FIOut}
            intra range       : {Range}
            intra prUid       : {PredOccId}
            intra nm          : {HsName}
            intra positionalNm : {HsName}
-}
sem_RecPatExpr_Empty :: Range ->
                        T_RecPatExpr 

sem_RecPatExpr_Empty hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_RecPatExpr_Empty_1 :: T_RecPatExpr_1 
                  sem_RecPatExpr_Empty_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup208 | __tup208 `seq` (True) ->
                             (case (__tup208) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_RecPatExpr_Empty_2 :: T_RecPatExpr_2 
                                          sem_RecPatExpr_Empty_2  =
                                              (\ _lhsIkiGam
                                                 _lhsIlexLev
                                                 _lhsIpredSameScopeCounter ->
                                                   _lhsIkiGam `seq`
                                                   (_lhsIlexLev `seq`
                                                    (_lhsIpredSameScopeCounter `seq`
                                                     ((case (_lhsIpredSameScopeCounter) of
                                                       { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                       (case ((let sem_RecPatExpr_Empty_3 :: T_RecPatExpr_3 
                                                                   sem_RecPatExpr_Empty_3  =
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
                                                                                        (case ((let sem_RecPatExpr_Empty_4 :: T_RecPatExpr_4 
                                                                                                    sem_RecPatExpr_Empty_4  =
                                                                                                        (\ _lhsIclGam
                                                                                                           _lhsIdataGam
                                                                                                           _lhsIfinKiVarMp
                                                                                                           _lhsIfinTyKiGam
                                                                                                           _lhsIpositionalFldNmL
                                                                                                           _lhsItyTyTySigFreeTvarS ->
                                                                                                             _lhsIclGam `seq`
                                                                                                             (_lhsIdataGam `seq`
                                                                                                              (_lhsIfinKiVarMp `seq`
                                                                                                               (_lhsIfinTyKiGam `seq`
                                                                                                                (_lhsIpositionalFldNmL `seq`
                                                                                                                 (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                  ((case (Seq.empty) of
                                                                                                                    { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                    (case (False) of
                                                                                                                     { _lhsOhasAFldRef | _lhsOhasAFldRef `seq` (True) ->
                                                                                                                     (case (_lhsIpositionalFldNmL) of
                                                                                                                      { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                      (case ((let sem_RecPatExpr_Empty_5 :: T_RecPatExpr_5 
                                                                                                                                  sem_RecPatExpr_Empty_5  =
                                                                                                                                      (\ _lhsIfiOpts
                                                                                                                                         _lhsIknTy
                                                                                                                                         _lhsImatchOnFld
                                                                                                                                         _lhsIpatTyVarMp
                                                                                                                                         _lhsItySigGam
                                                                                                                                         _lhsIvalGam ->
                                                                                                                                           _lhsIfiOpts `seq`
                                                                                                                                           (_lhsIknTy `seq`
                                                                                                                                            (_lhsImatchOnFld `seq`
                                                                                                                                             (_lhsIpatTyVarMp `seq`
                                                                                                                                              (_lhsItySigGam `seq`
                                                                                                                                               (_lhsIvalGam `seq`
                                                                                                                                                ((case (__tup208) of
                                                                                                                                                  { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                  (case (defaultFIEnv
                                                                                                                                                             { feEHCOpts = _lhsIopts
                                                                                                                                                             , fePredScope = _lhsIpredScope
                                                                                                                                                             , feTyGam = _lhsItyGam
                                                                                                                                                             , fePolGam = _lhsIpolGam
                                                                                                                                                             , feRange = _range
                                                                                                                                                             }) of
                                                                                                                                                   { _fe | _fe `seq` (True) ->
                                                                                                                                                   (case (fitsIn (_lhsIfiOpts) _fe _lUniq _lhsIpatTyVarMp _lhsIknTy tyRecEmpty) of
                                                                                                                                                    { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                    (case (foVarMp _fo_ `varUpd` _lhsIpatTyVarMp) of
                                                                                                                                                     { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                     (case (_lhsIvalGam) of
                                                                                                                                                      { _lhsOvalGam | _lhsOvalGam `seq` (True) ->
                                                                                                                                                      (case ((let sem_RecPatExpr_Empty_6 :: T_RecPatExpr_6 
                                                                                                                                                                  sem_RecPatExpr_Empty_6  =
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
                                                                                                                                                                                    (case (foTy _fo_) of
                                                                                                                                                                                     { _ty | _ty `seq` (True) ->
                                                                                                                                                                                     (case (_ty) of
                                                                                                                                                                                      { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                      (case (_lhsItyVarMp) of
                                                                                                                                                                                       { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                       (case ((let sem_RecPatExpr_Empty_7 :: T_RecPatExpr_7 
                                                                                                                                                                                                   sem_RecPatExpr_Empty_7  =
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
                                                                                                                                                                                                                       (case (hsnORec >|< hsnCRec) of
                                                                                                                                                                                                                        { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                        (case (rngLift _range mkNestErr' _pp [foErrSq _fo_]) of
                                                                                                                                                                                                                         { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                         (case (hsnRowEmpty) of
                                                                                                                                                                                                                          { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                          (case ([]) of
                                                                                                                                                                                                                           { _lhsOfsRPatL | _lhsOfsRPatL `seq` (True) ->
                                                                                                                                                                                                                           (case (Map.empty) of
                                                                                                                                                                                                                            { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                            (case (emptyVarMp) of
                                                                                                                                                                                                                             { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                             (case (CPatRest_Empty) of
                                                                                                                                                                                                                              { _lhsOpatCRest | _lhsOpatCRest `seq` (True) ->
                                                                                                                                                                                                                              (case (_pp) of
                                                                                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                               (case ([]) of
                                                                                                                                                                                                                                { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                ( _lhsOallErrSq,_lhsOcSubst,_lhsOcbindL,_lhsOerrSq,_lhsOextNm,_lhsOfsRPatL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatCRest,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                               in  sem_RecPatExpr_Empty_7)) of
                                                                                                                                                                                        { ( sem_RecPatExpr_7) | True ->
                                                                                                                                                                                        ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOscopeGam,_lhsOty,_lhsOtyVarMp,sem_RecPatExpr_7) }) }) }) }) }) }) }))))))))
                                                                                                                                                              in  sem_RecPatExpr_Empty_6)) of
                                                                                                                                                       { ( sem_RecPatExpr_6) | True ->
                                                                                                                                                       ( _lhsOpatTyVarMp,_lhsOvalGam,sem_RecPatExpr_6) }) }) }) }) }) }))))))))
                                                                                                                              in  sem_RecPatExpr_Empty_5)) of
                                                                                                                       { ( sem_RecPatExpr_5) | True ->
                                                                                                                       ( _lhsOchrInstDeclSq,_lhsOhasAFldRef,_lhsOpositionalFldNmL,sem_RecPatExpr_5) }) }) }) }))))))))
                                                                                                in  sem_RecPatExpr_Empty_4)) of
                                                                                         { ( sem_RecPatExpr_4) | True ->
                                                                                         ( _lhsOkiVarMp,_lhsOpolVarMp,_lhsOtyGam,_lhsOtyKiGam,sem_RecPatExpr_4) }) }) }) }) }))))))))))
                                                               in  sem_RecPatExpr_Empty_3)) of
                                                        { ( sem_RecPatExpr_3) | True ->
                                                        ( _lhsOpredSameScopeCounter,sem_RecPatExpr_3) }) })))))
                                      in  sem_RecPatExpr_Empty_2)) of
                               { ( sem_RecPatExpr_2) | True ->
                               ( _lhsOgUniq,sem_RecPatExpr_2) }) }) })))
              in  sem_RecPatExpr_Empty_1)) of
       { ( sem_RecPatExpr_1) | True ->
       ( _lhsOrange,sem_RecPatExpr_1) }) }) })

sem_RecPatExpr_Expr :: Range ->
                       T_PatExpr  ->
                       T_RecPatExpr 

sem_RecPatExpr_Expr hsrange_ patExpr_  | hsrange_ `seq` (patExpr_ `seq` (True)) =
    (case (patExpr_ ) of
     { ( _patExprIrange,patExpr_1) | True ->
         (case (rangeUnions [hsrange_, _patExprIrange, _patExprIrange
                                                                    ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_RecPatExpr_Expr_1 :: T_RecPatExpr_1 
                       sem_RecPatExpr_Expr_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _patExprOgUniq | _patExprOgUniq `seq` (True) ->
                                  (case (patExpr_1 _patExprOgUniq ) of
                                   { ( _patExprIgUniq,patExpr_2) | True ->
                                       (case (_patExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_RecPatExpr_Expr_2 :: T_RecPatExpr_2 
                                                    sem_RecPatExpr_Expr_2  =
                                                        (\ _lhsIkiGam
                                                           _lhsIlexLev
                                                           _lhsIpredSameScopeCounter ->
                                                             _lhsIkiGam `seq`
                                                             (_lhsIlexLev `seq`
                                                              (_lhsIpredSameScopeCounter `seq`
                                                               ((case (_lhsIpredSameScopeCounter) of
                                                                 { _patExprOpredSameScopeCounter | _patExprOpredSameScopeCounter `seq` (True) ->
                                                                 (case (_lhsIlexLev) of
                                                                  { _patExprOlexLev | _patExprOlexLev `seq` (True) ->
                                                                  (case (_lhsIkiGam) of
                                                                   { _patExprOkiGam | _patExprOkiGam `seq` (True) ->
                                                                   (case (patExpr_2 _patExprOkiGam _patExprOlexLev _patExprOpredSameScopeCounter ) of
                                                                    { ( _patExprIpredSameScopeCounter,patExpr_3) | True ->
                                                                        (case (_patExprIpredSameScopeCounter) of
                                                                         { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                         (case ((let sem_RecPatExpr_Expr_3 :: T_RecPatExpr_3 
                                                                                     sem_RecPatExpr_Expr_3  =
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
                                                                                                        (case (_lhsItyGam) of
                                                                                                         { _patExprOtyGam | _patExprOtyGam `seq` (True) ->
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
                                                                                                                       (case ((let sem_RecPatExpr_Expr_4 :: T_RecPatExpr_4 
                                                                                                                                   sem_RecPatExpr_Expr_4  =
                                                                                                                                       (\ _lhsIclGam
                                                                                                                                          _lhsIdataGam
                                                                                                                                          _lhsIfinKiVarMp
                                                                                                                                          _lhsIfinTyKiGam
                                                                                                                                          _lhsIpositionalFldNmL
                                                                                                                                          _lhsItyTyTySigFreeTvarS ->
                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                            (_lhsIdataGam `seq`
                                                                                                                                             (_lhsIfinKiVarMp `seq`
                                                                                                                                              (_lhsIfinTyKiGam `seq`
                                                                                                                                               (_lhsIpositionalFldNmL `seq`
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
                                                                                                                                                       (case (patExpr_4 _patExprOclGam _patExprOdataGam _patExprOfinKiVarMp _patExprOfinTyKiGam _patExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                        { ( _patExprIchrInstDeclSq,_patExprItopNm,patExpr_5) | True ->
                                                                                                                                                            (case (_patExprIchrInstDeclSq) of
                                                                                                                                                             { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                             (case (False) of
                                                                                                                                                              { _lhsOhasAFldRef | _lhsOhasAFldRef `seq` (True) ->
                                                                                                                                                              (case (_lhsIpositionalFldNmL) of
                                                                                                                                                               { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                                               (case ((let sem_RecPatExpr_Expr_5 :: T_RecPatExpr_5 
                                                                                                                                                                           sem_RecPatExpr_Expr_5  =
                                                                                                                                                                               (\ _lhsIfiOpts
                                                                                                                                                                                  _lhsIknTy
                                                                                                                                                                                  _lhsImatchOnFld
                                                                                                                                                                                  _lhsIpatTyVarMp
                                                                                                                                                                                  _lhsItySigGam
                                                                                                                                                                                  _lhsIvalGam ->
                                                                                                                                                                                    _lhsIfiOpts `seq`
                                                                                                                                                                                    (_lhsIknTy `seq`
                                                                                                                                                                                     (_lhsImatchOnFld `seq`
                                                                                                                                                                                      (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                       (_lhsItySigGam `seq`
                                                                                                                                                                                        (_lhsIvalGam `seq`
                                                                                                                                                                                         ((case (_lhsIvalGam) of
                                                                                                                                                                                           { _patExprOvalGam | _patExprOvalGam `seq` (True) ->
                                                                                                                                                                                           (case (_lhsItySigGam) of
                                                                                                                                                                                            { _patExprOtySigGam | _patExprOtySigGam `seq` (True) ->
                                                                                                                                                                                            (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                             { _patExprOpatTyVarMp | _patExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                             (case (_lhsIknTy) of
                                                                                                                                                                                              { _patExprOknTy | _patExprOknTy `seq` (True) ->
                                                                                                                                                                                              (case (_lhsIfiOpts) of
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
                                                                                                                                                                                                            (case (_patExprIvalGam) of
                                                                                                                                                                                                             { _lhsOvalGam | _lhsOvalGam `seq` (True) ->
                                                                                                                                                                                                             (case ((let sem_RecPatExpr_Expr_6 :: T_RecPatExpr_6 
                                                                                                                                                                                                                         sem_RecPatExpr_Expr_6  =
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
                                                                                                                                                                                                                                         (case (_lhsItyVarMp) of
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
                                                                                                                                                                                                                                                   (case (_patExprIgathCnstrMp) of
                                                                                                                                                                                                                                                    { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                    (case (_patExprIgathRangeMp) of
                                                                                                                                                                                                                                                     { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                     (case (_patExprIscopeGam) of
                                                                                                                                                                                                                                                      { _lhsOscopeGam | _lhsOscopeGam `seq` (True) ->
                                                                                                                                                                                                                                                      (case (_patExprIty) of
                                                                                                                                                                                                                                                       { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                       (case (_patExprItyVarMp) of
                                                                                                                                                                                                                                                        { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                        (case ((let sem_RecPatExpr_Expr_7 :: T_RecPatExpr_7 
                                                                                                                                                                                                                                                                    sem_RecPatExpr_Expr_7  =
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
                                                                                                                                                                                                                                                                                        (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                         { _patExprOmoduleNm | _patExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                          { _patExprOchrScopeBindMp | _patExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                           { _patExprOchrEvidBindMp | _patExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_lhsIceParentNm) of
                                                                                                                                                                                                                                                                                            { _patExprOceParentNm | _patExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                             { _patExprOcSubst | _patExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (patExpr_8 _patExprOcSubst _patExprOceParentNm _patExprOchrEvidBindMp _patExprOchrScopeBindMp _patExprOfinTyVarMp _patExprOfinValGam _patExprOmoduleNm _patExprOrangeMp ) of
                                                                                                                                                                                                                                                                                              { ( _patExprIallErrSq,_patExprIappArgPPL,_patExprIappFunNm,_patExprIappFunPP,_patExprIcSubst,_patExprIcbindL,_patExprIerrSq,_patExprIfsRPatL,_patExprIgathMentrelFilterMp,_patExprIgathTvKiVarMp,_patExprIisBang,_patExprIpatCRest,_patExprIpp,_patExprIrpat) | True ->
                                                                                                                                                                                                                                                                                                  (case (_patExprIallErrSq) of
                                                                                                                                                                                                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                   (case (_patExprIcSubst) of
                                                                                                                                                                                                                                                                                                    { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                    (case (_patExprIcbindL) of
                                                                                                                                                                                                                                                                                                     { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                     (case (_patExprIerrSq) of
                                                                                                                                                                                                                                                                                                      { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                                       { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (_patExprIfsRPatL) of
                                                                                                                                                                                                                                                                                                        { _lhsOfsRPatL | _lhsOfsRPatL `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (_patExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                         (case (_patExprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                          { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_patExprIpatCRest) of
                                                                                                                                                                                                                                                                                                           { _lhsOpatCRest | _lhsOpatCRest `seq` (True) ->
                                                                                                                                                                                                                                                                                                           (case (_patExprIpp) of
                                                                                                                                                                                                                                                                                                            { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                            (case (_pp) of
                                                                                                                                                                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case ([]) of
                                                                                                                                                                                                                                                                                                              { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                              ( _lhsOallErrSq,_lhsOcSubst,_lhsOcbindL,_lhsOerrSq,_lhsOextNm,_lhsOfsRPatL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatCRest,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                                in  sem_RecPatExpr_Expr_7)) of
                                                                                                                                                                                                                                                         { ( sem_RecPatExpr_7) | True ->
                                                                                                                                                                                                                                                         ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOscopeGam,_lhsOty,_lhsOtyVarMp,sem_RecPatExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                                                                     in  sem_RecPatExpr_Expr_6)) of
                                                                                                                                                                                                              { ( sem_RecPatExpr_6) | True ->
                                                                                                                                                                                                              ( _lhsOpatTyVarMp,_lhsOvalGam,sem_RecPatExpr_6) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                       in  sem_RecPatExpr_Expr_5)) of
                                                                                                                                                                { ( sem_RecPatExpr_5) | True ->
                                                                                                                                                                ( _lhsOchrInstDeclSq,_lhsOhasAFldRef,_lhsOpositionalFldNmL,sem_RecPatExpr_5) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                               in  sem_RecPatExpr_Expr_4)) of
                                                                                                                        { ( sem_RecPatExpr_4) | True ->
                                                                                                                        ( _lhsOkiVarMp,_lhsOpolVarMp,_lhsOtyGam,_lhsOtyKiGam,sem_RecPatExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                 in  sem_RecPatExpr_Expr_3)) of
                                                                          { ( sem_RecPatExpr_3) | True ->
                                                                          ( _lhsOpredSameScopeCounter,sem_RecPatExpr_3) }) }) }) }) }) })))))
                                                in  sem_RecPatExpr_Expr_2)) of
                                         { ( sem_RecPatExpr_2) | True ->
                                         ( _lhsOgUniq,sem_RecPatExpr_2) }) }) }) })))
                   in  sem_RecPatExpr_Expr_1)) of
            { ( sem_RecPatExpr_1) | True ->
            ( _lhsOrange,sem_RecPatExpr_1) }) }) }) })

sem_RecPatExpr_Ext :: Range ->
                      T_RecPatExpr  ->
                      (Maybe HsName) ->
                      T_PatExpr  ->
                      T_RecPatExpr 

sem_RecPatExpr_Ext hsrange_ recPatExpr_ mbNm_ patExpr_  | hsrange_ `seq` (recPatExpr_ `seq` (mbNm_ `seq` (patExpr_ `seq` (True)))) =
    (case (patExpr_ ) of
     { ( _patExprIrange,patExpr_1) | True ->
         (case (recPatExpr_ ) of
          { ( _recPatExprIrange,recPatExpr_1) | True ->
              (case (rangeUnions [hsrange_, _recPatExprIrange
                                                          , _patExprIrange
                                                                         ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_RecPatExpr_Ext_1 :: T_RecPatExpr_1 
                            sem_RecPatExpr_Ext_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> (__cont, lUniq,lUniq2,lUniq3)}}} )) of
                                       { __tup211 | __tup211 `seq` (True) ->
                                       (case (__tup211) of
                                        { (_recPatExprOgUniq,_,_,_) | _recPatExprOgUniq `seq` (True) ->
                                        (case (recPatExpr_1 _recPatExprOgUniq ) of
                                         { ( _recPatExprIgUniq,recPatExpr_2) | True ->
                                             (case (_recPatExprIgUniq) of
                                              { _patExprOgUniq | _patExprOgUniq `seq` (True) ->
                                              (case (patExpr_1 _patExprOgUniq ) of
                                               { ( _patExprIgUniq,patExpr_2) | True ->
                                                   (case (_patExprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_RecPatExpr_Ext_2 :: T_RecPatExpr_2 
                                                                sem_RecPatExpr_Ext_2  =
                                                                    (\ _lhsIkiGam
                                                                       _lhsIlexLev
                                                                       _lhsIpredSameScopeCounter ->
                                                                         _lhsIkiGam `seq`
                                                                         (_lhsIlexLev `seq`
                                                                          (_lhsIpredSameScopeCounter `seq`
                                                                           ((case (_lhsIpredSameScopeCounter) of
                                                                             { _recPatExprOpredSameScopeCounter | _recPatExprOpredSameScopeCounter `seq` (True) ->
                                                                             (case (_lhsIlexLev) of
                                                                              { _recPatExprOlexLev | _recPatExprOlexLev `seq` (True) ->
                                                                              (case (_lhsIkiGam) of
                                                                               { _recPatExprOkiGam | _recPatExprOkiGam `seq` (True) ->
                                                                               (case (recPatExpr_2 _recPatExprOkiGam _recPatExprOlexLev _recPatExprOpredSameScopeCounter ) of
                                                                                { ( _recPatExprIpredSameScopeCounter,recPatExpr_3) | True ->
                                                                                    (case (_recPatExprIpredSameScopeCounter) of
                                                                                     { _patExprOpredSameScopeCounter | _patExprOpredSameScopeCounter `seq` (True) ->
                                                                                     (case (_lhsIlexLev) of
                                                                                      { _patExprOlexLev | _patExprOlexLev `seq` (True) ->
                                                                                      (case (_lhsIkiGam) of
                                                                                       { _patExprOkiGam | _patExprOkiGam `seq` (True) ->
                                                                                       (case (patExpr_2 _patExprOkiGam _patExprOlexLev _patExprOpredSameScopeCounter ) of
                                                                                        { ( _patExprIpredSameScopeCounter,patExpr_3) | True ->
                                                                                            (case (_patExprIpredSameScopeCounter) of
                                                                                             { _lhsOpredSameScopeCounter | _lhsOpredSameScopeCounter `seq` (True) ->
                                                                                             (case ((let sem_RecPatExpr_Ext_3 :: T_RecPatExpr_3 
                                                                                                         sem_RecPatExpr_Ext_3  =
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
                                                                                                                            { _recPatExprOtyKiGlobFreeTvarS | _recPatExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                            (case (_lhsItyKiGam) of
                                                                                                                             { _recPatExprOtyKiGam | _recPatExprOtyKiGam `seq` (True) ->
                                                                                                                             (case (_lhsItyGam) of
                                                                                                                              { _recPatExprOtyGam | _recPatExprOtyGam `seq` (True) ->
                                                                                                                              (case (_lhsIpredScope) of
                                                                                                                               { _recPatExprOpredScope | _recPatExprOpredScope `seq` (True) ->
                                                                                                                               (case (_lhsIpolVarMp) of
                                                                                                                                { _recPatExprOpolVarMp | _recPatExprOpolVarMp `seq` (True) ->
                                                                                                                                (case (_lhsIpolGam) of
                                                                                                                                 { _recPatExprOpolGam | _recPatExprOpolGam `seq` (True) ->
                                                                                                                                 (case (_lhsIopts) of
                                                                                                                                  { _recPatExprOopts | _recPatExprOopts `seq` (True) ->
                                                                                                                                  (case (_lhsIkiVarMp) of
                                                                                                                                   { _recPatExprOkiVarMp | _recPatExprOkiVarMp `seq` (True) ->
                                                                                                                                   (case (recPatExpr_3 _recPatExprOkiVarMp _recPatExprOopts _recPatExprOpolGam _recPatExprOpolVarMp _recPatExprOpredScope _recPatExprOtyGam _recPatExprOtyKiGam _recPatExprOtyKiGlobFreeTvarS ) of
                                                                                                                                    { ( _recPatExprIkiVarMp,_recPatExprIpolVarMp,_recPatExprItyGam,_recPatExprItyKiGam,recPatExpr_4) | True ->
                                                                                                                                        (case (_recPatExprItyKiGam) of
                                                                                                                                         { _patExprOtyKiGam | _patExprOtyKiGam `seq` (True) ->
                                                                                                                                         (case (_recPatExprItyGam) of
                                                                                                                                          { _patExprOtyGam | _patExprOtyGam `seq` (True) ->
                                                                                                                                          (case (_lhsIpredScope) of
                                                                                                                                           { _patExprOpredScope | _patExprOpredScope `seq` (True) ->
                                                                                                                                           (case (_recPatExprIpolVarMp) of
                                                                                                                                            { _patExprOpolVarMp | _patExprOpolVarMp `seq` (True) ->
                                                                                                                                            (case (_lhsIpolGam) of
                                                                                                                                             { _patExprOpolGam | _patExprOpolGam `seq` (True) ->
                                                                                                                                             (case (_lhsIopts) of
                                                                                                                                              { _patExprOopts | _patExprOopts `seq` (True) ->
                                                                                                                                              (case (_recPatExprIkiVarMp) of
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
                                                                                                                                                        (case ((let sem_RecPatExpr_Ext_4 :: T_RecPatExpr_4 
                                                                                                                                                                    sem_RecPatExpr_Ext_4  =
                                                                                                                                                                        (\ _lhsIclGam
                                                                                                                                                                           _lhsIdataGam
                                                                                                                                                                           _lhsIfinKiVarMp
                                                                                                                                                                           _lhsIfinTyKiGam
                                                                                                                                                                           _lhsIpositionalFldNmL
                                                                                                                                                                           _lhsItyTyTySigFreeTvarS ->
                                                                                                                                                                             _lhsIclGam `seq`
                                                                                                                                                                             (_lhsIdataGam `seq`
                                                                                                                                                                              (_lhsIfinKiVarMp `seq`
                                                                                                                                                                               (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                (_lhsIpositionalFldNmL `seq`
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
                                                                                                                                                                                         { _recPatExprOtyTyTySigFreeTvarS | _recPatExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                         (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                          { _recPatExprOfinTyKiGam | _recPatExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                          (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                           { _recPatExprOfinKiVarMp | _recPatExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                           (case (_lhsIdataGam) of
                                                                                                                                                                                            { _recPatExprOdataGam | _recPatExprOdataGam `seq` (True) ->
                                                                                                                                                                                            (case (_lhsIclGam) of
                                                                                                                                                                                             { _recPatExprOclGam | _recPatExprOclGam `seq` (True) ->
                                                                                                                                                                                             (case (patExpr_4 _patExprOclGam _patExprOdataGam _patExprOfinKiVarMp _patExprOfinTyKiGam _patExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                              { ( _patExprIchrInstDeclSq,_patExprItopNm,patExpr_5) | True ->
                                                                                                                                                                                                  (case (_lhsIpositionalFldNmL) of
                                                                                                                                                                                                   { _recPatExprOpositionalFldNmL | _recPatExprOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                   (case (recPatExpr_4 _recPatExprOclGam _recPatExprOdataGam _recPatExprOfinKiVarMp _recPatExprOfinTyKiGam _recPatExprOpositionalFldNmL _recPatExprOtyTyTySigFreeTvarS ) of
                                                                                                                                                                                                    { ( _recPatExprIchrInstDeclSq,_recPatExprIhasAFldRef,_recPatExprIpositionalFldNmL,recPatExpr_5) | True ->
                                                                                                                                                                                                        (case (_recPatExprIchrInstDeclSq `Seq.union` _patExprIchrInstDeclSq) of
                                                                                                                                                                                                         { _lhsOchrInstDeclSq | _lhsOchrInstDeclSq `seq` (True) ->
                                                                                                                                                                                                         (case (maybe _recPatExprIhasAFldRef (const True) mbNm_) of
                                                                                                                                                                                                          { _lhsOhasAFldRef | _lhsOhasAFldRef `seq` (True) ->
                                                                                                                                                                                                          (case (hdAndTl _recPatExprIpositionalFldNmL) of
                                                                                                                                                                                                           { __tup209 | __tup209 `seq` (True) ->
                                                                                                                                                                                                           (case (__tup209) of
                                                                                                                                                                                                            { (_,_lhsOpositionalFldNmL) | _lhsOpositionalFldNmL `seq` (True) ->
                                                                                                                                                                                                            (case ((let sem_RecPatExpr_Ext_5 :: T_RecPatExpr_5 
                                                                                                                                                                                                                        sem_RecPatExpr_Ext_5  =
                                                                                                                                                                                                                            (\ _lhsIfiOpts
                                                                                                                                                                                                                               _lhsIknTy
                                                                                                                                                                                                                               _lhsImatchOnFld
                                                                                                                                                                                                                               _lhsIpatTyVarMp
                                                                                                                                                                                                                               _lhsItySigGam
                                                                                                                                                                                                                               _lhsIvalGam ->
                                                                                                                                                                                                                                 _lhsIfiOpts `seq`
                                                                                                                                                                                                                                 (_lhsIknTy `seq`
                                                                                                                                                                                                                                  (_lhsImatchOnFld `seq`
                                                                                                                                                                                                                                   (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                                    (_lhsItySigGam `seq`
                                                                                                                                                                                                                                     (_lhsIvalGam `seq`
                                                                                                                                                                                                                                      ((case (_lhsIvalGam) of
                                                                                                                                                                                                                                        { _recPatExprOvalGam | _recPatExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                        (case (_lhsItySigGam) of
                                                                                                                                                                                                                                         { _recPatExprOtySigGam | _recPatExprOtySigGam `seq` (True) ->
                                                                                                                                                                                                                                         (case (_lhsImatchOnFld) of
                                                                                                                                                                                                                                          { _recPatExprOmatchOnFld | _recPatExprOmatchOnFld `seq` (True) ->
                                                                                                                                                                                                                                          (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                           { _recPatExprOfiOpts | _recPatExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                           (case (__tup211) of
                                                                                                                                                                                                                                            { (_,_,_lUniq2,_) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                                            (case (__tup211) of
                                                                                                                                                                                                                                             { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                             (case (defaultFIEnv
                                                                                                                                                                                                                                                        { feEHCOpts = _lhsIopts
                                                                                                                                                                                                                                                        , fePredScope = _lhsIpredScope
                                                                                                                                                                                                                                                        , feTyGam = _lhsItyGam
                                                                                                                                                                                                                                                        , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                                        , feRange = _range
                                                                                                                                                                                                                                                        }) of
                                                                                                                                                                                                                                              { _fe | _fe `seq` (True) ->
                                                                                                                                                                                                                                              (case (let  [r,e] = mkNewTyVarL 2 _lUniq
                                                                                                                                                                                                                                                     in   (r,e)) of
                                                                                                                                                                                                                                               { __tup210 | __tup210 `seq` (True) ->
                                                                                                                                                                                                                                               (case (__tup210) of
                                                                                                                                                                                                                                                { (_rowTy,_) | _rowTy `seq` (True) ->
                                                                                                                                                                                                                                                (case (hsnRec `mkConApp` [_rowTy]) of
                                                                                                                                                                                                                                                 { _extTy | _extTy `seq` (True) ->
                                                                                                                                                                                                                                                 (case (__tup210) of
                                                                                                                                                                                                                                                  { (_,_patTy) | _patTy `seq` (True) ->
                                                                                                                                                                                                                                                  (case (__tup209) of
                                                                                                                                                                                                                                                   { (_positionalNm,_) | _positionalNm `seq` (True) ->
                                                                                                                                                                                                                                                   (case (case mbNm_ of
                                                                                                                                                                                                                                                            Just nm                     -> nm
                                                                                                                                                                                                                                                            Nothing  | _lhsImatchOnFld  -> _patExprItopNm
                                                                                                                                                                                                                                                                     | otherwise        -> _positionalNm) of
                                                                                                                                                                                                                                                    { _nm | _nm `seq` (True) ->
                                                                                                                                                                                                                                                    (case (_extTy `mkTyRecExt` [(_nm,_patTy)]) of
                                                                                                                                                                                                                                                     { _recTy | _recTy `seq` (True) ->
                                                                                                                                                                                                                                                     (case (fitsIn (_lhsIfiOpts) _fe _lUniq2 _lhsIpatTyVarMp _lhsIknTy _recTy) of
                                                                                                                                                                                                                                                      { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                                                                      (case (foVarMp _fo_ `varUpd` _lhsIpatTyVarMp) of
                                                                                                                                                                                                                                                       { _recPatExprOpatTyVarMp | _recPatExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                       (case (_extTy) of
                                                                                                                                                                                                                                                        { _recPatExprOknTy | _recPatExprOknTy `seq` (True) ->
                                                                                                                                                                                                                                                        (case (recPatExpr_5 _recPatExprOfiOpts _recPatExprOknTy _recPatExprOmatchOnFld _recPatExprOpatTyVarMp _recPatExprOtySigGam _recPatExprOvalGam ) of
                                                                                                                                                                                                                                                         { ( _recPatExprIpatTyVarMp,_recPatExprIvalGam,recPatExpr_6) | True ->
                                                                                                                                                                                                                                                             (case (_recPatExprIvalGam) of
                                                                                                                                                                                                                                                              { _patExprOvalGam | _patExprOvalGam `seq` (True) ->
                                                                                                                                                                                                                                                              (case (_lhsItySigGam) of
                                                                                                                                                                                                                                                               { _patExprOtySigGam | _patExprOtySigGam `seq` (True) ->
                                                                                                                                                                                                                                                               (case (_recPatExprIpatTyVarMp) of
                                                                                                                                                                                                                                                                { _patExprOpatTyVarMp | _patExprOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                (case (_lhsIfiOpts) of
                                                                                                                                                                                                                                                                 { _patExprOfiOpts | _patExprOfiOpts `seq` (True) ->
                                                                                                                                                                                                                                                                 (case (_patTy) of
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
                                                                                                                                                                                                                                                                                (case ((let sem_RecPatExpr_Ext_6 :: T_RecPatExpr_6 
                                                                                                                                                                                                                                                                                            sem_RecPatExpr_Ext_6  =
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
                                                                                                                                                                                                                                                                                                             { _recPatExprOvalTyGlobFreeTvarS | _recPatExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case (_lhsItyVarMp) of
                                                                                                                                                                                                                                                                                                              { _recPatExprOtyVarMp | _recPatExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                               { _recPatExprOtyTyGlobFreeTvarS | _recPatExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                                { _recPatExprOtvKiVarMp | _recPatExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                                 { _recPatExprOclDfGam | _recPatExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                                  { _recPatExprOchrStore | _recPatExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (recPatExpr_6 _recPatExprOchrStore _recPatExprOclDfGam _recPatExprOtvKiVarMp _recPatExprOtyTyGlobFreeTvarS _recPatExprOtyVarMp _recPatExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                   { ( _recPatExprIgathCnstrMp,_recPatExprIgathRangeMp,_recPatExprIscopeGam,_recPatExprIty,_recPatExprItyVarMp,recPatExpr_7) | True ->
                                                                                                                                                                                                                                                                                                                       (case (_recPatExprItyVarMp) of
                                                                                                                                                                                                                                                                                                                        { _patExprOtyVarMp | _patExprOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                         { _patExprOtyTyGlobFreeTvarS | _patExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                                          { _patExprOtvKiVarMp | _patExprOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                          (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                                           { _patExprOclDfGam | _patExprOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                           (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                                            { _patExprOchrStore | _patExprOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                                            (case (__tup211) of
                                                                                                                                                                                                                                                                                                                             { (_,_,_,_lUniq3) | _lUniq3 `seq` (True) ->
                                                                                                                                                                                                                                                                                                                             (case (mkPrIdCHR _lUniq3) of
                                                                                                                                                                                                                                                                                                                              { _prUid | _prUid `seq` (True) ->
                                                                                                                                                                                                                                                                                                                              (case ([rngLift _range mkPredOccRng (Pred_Lacks _rowTy (Label_Lab _nm)) _prUid _lhsIpredScope]) of
                                                                                                                                                                                                                                                                                                                               { _prOccL | _prOccL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                               (case (gathPredLToProveCnstrMp _prOccL) of
                                                                                                                                                                                                                                                                                                                                { _hereCnstrMp | _hereCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                (case (patExpr_7 _patExprOchrStore _patExprOclDfGam _patExprOtvKiVarMp _patExprOtyTyGlobFreeTvarS _patExprOtyVarMp _patExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                                 { ( _patExprIcpNm,_patExprIgathCnstrMp,_patExprIgathRangeMp,_patExprIscopeGam,_patExprIty,_patExprItyVarMp,patExpr_8) | True ->
                                                                                                                                                                                                                                                                                                                                     (case (cnstrMpUnions [_hereCnstrMp, _recPatExprIgathCnstrMp, _patExprIgathCnstrMp]) of
                                                                                                                                                                                                                                                                                                                                      { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_recPatExprIgathRangeMp `Map.union` _patExprIgathRangeMp) of
                                                                                                                                                                                                                                                                                                                                       { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       (case (_recPatExprIscopeGam `gamUnion` _patExprIscopeGam) of
                                                                                                                                                                                                                                                                                                                                        { _lhsOscopeGam | _lhsOscopeGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                        (case (foTy _fo_) of
                                                                                                                                                                                                                                                                                                                                         { _ty | _ty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                         (case (_ty) of
                                                                                                                                                                                                                                                                                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                          (case (_patExprItyVarMp) of
                                                                                                                                                                                                                                                                                                                                           { _lhsOtyVarMp | _lhsOtyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                           (case ((let sem_RecPatExpr_Ext_7 :: T_RecPatExpr_7 
                                                                                                                                                                                                                                                                                                                                                       sem_RecPatExpr_Ext_7  =
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
                                                                                                                                                                                                                                                                                                                                                                            { _recPatExprOrangeMp | _recPatExprOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                            (case (_lhsIfinValGam) of
                                                                                                                                                                                                                                                                                                                                                                             { _recPatExprOfinValGam | _recPatExprOfinValGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                             (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                                              { _recPatExprOfinTyVarMp | _recPatExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                              (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                               { _patExprOmoduleNm | _patExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                               (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                { _patExprOchrScopeBindMp | _patExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                 { _patExprOchrEvidBindMp | _patExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                  { _recPatExprOchrScopeBindMp | _recPatExprOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                                                                                   { _recPatExprOchrEvidBindMp | _recPatExprOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                   (case (_lhsIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                    { _recPatExprOcSubst | _recPatExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                    (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                                                                     { _recPatExprOmoduleNm | _recPatExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                     (case (_lhsIceParentNm) of
                                                                                                                                                                                                                                                                                                                                                                                      { _recPatExprOceParentNm | _recPatExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                      (case (recPatExpr_7 _recPatExprOcSubst _recPatExprOceParentNm _recPatExprOchrEvidBindMp _recPatExprOchrScopeBindMp _recPatExprOfinTyVarMp _recPatExprOfinValGam _recPatExprOmoduleNm _recPatExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                                       { ( _recPatExprIallErrSq,_recPatExprIcSubst,_recPatExprIcbindL,_recPatExprIerrSq,_recPatExprIextNm,_recPatExprIfsRPatL,_recPatExprIgathMentrelFilterMp,_recPatExprIgathTvKiVarMp,_recPatExprIpatCRest,_recPatExprIpp,_recPatExprIppL) | True ->
                                                                                                                                                                                                                                                                                                                                                                                           (case (_recPatExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                            { _patExprOcSubst | _patExprOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                            (case (rpatNmNm _patExprIcpNm) of
                                                                                                                                                                                                                                                                                                                                                                                             { _patExprOceParentNm | _patExprOceParentNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                             (case (patExpr_8 _patExprOcSubst _patExprOceParentNm _patExprOchrEvidBindMp _patExprOchrScopeBindMp _patExprOfinTyVarMp _patExprOfinValGam _patExprOmoduleNm _patExprOrangeMp ) of
                                                                                                                                                                                                                                                                                                                                                                                              { ( _patExprIallErrSq,_patExprIappArgPPL,_patExprIappFunNm,_patExprIappFunPP,_patExprIcSubst,_patExprIcbindL,_patExprIerrSq,_patExprIfsRPatL,_patExprIgathMentrelFilterMp,_patExprIgathTvKiVarMp,_patExprIisBang,_patExprIpatCRest,_patExprIpp,_patExprIrpat) | True ->
                                                                                                                                                                                                                                                                                                                                                                                                  (case (_recPatExprIallErrSq `Seq.union` _patExprIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                   (case (_patExprIcSubst) of
                                                                                                                                                                                                                                                                                                                                                                                                    { _lhsOcSubst | _lhsOcSubst `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                    (case (_recPatExprIcbindL ++ _patExprIcbindL) of
                                                                                                                                                                                                                                                                                                                                                                                                     { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                     (case (_recPatExprIpp) of
                                                                                                                                                                                                                                                                                                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                      (case (Seq.unions [ _recPatExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                                        , rngLift _range mkNestErr' _pp [_patExprIerrSq, foErrSq _fo_]
                                                                                                                                                                                                                                                                                                                                                                                                                        ]) of
                                                                                                                                                                                                                                                                                                                                                                                                       { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                       (case (_recPatExprIextNm) of
                                                                                                                                                                                                                                                                                                                                                                                                        { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                        (case (cSubstApp _patExprIcSubst $ acoreNmHolePred _prUid) of
                                                                                                                                                                                                                                                                                                                                                                                                         { _offset | _offset `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                         (case ((FldComputeOffset _nm _offset,_patExprIrpat) : _recPatExprIfsRPatL) of
                                                                                                                                                                                                                                                                                                                                                                                                          { _lhsOfsRPatL | _lhsOfsRPatL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                          (case (_recPatExprIgathMentrelFilterMp `mentrelFilterMpUnion` _patExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                                                           { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                           (case (_recPatExprIgathTvKiVarMp `varmpUnion` _patExprIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                                                                            { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                            (case (_recPatExprIpatCRest) of
                                                                                                                                                                                                                                                                                                                                                                                                             { _lhsOpatCRest | _lhsOpatCRest `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                             (case (_pp) of
                                                                                                                                                                                                                                                                                                                                                                                                              { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                              (case (ppFld "=" (Just _positionalNm) _nm (pp _nm) _patExprIpp : _recPatExprIppL) of
                                                                                                                                                                                                                                                                                                                                                                                                               { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                                                               ( _lhsOallErrSq,_lhsOcSubst,_lhsOcbindL,_lhsOerrSq,_lhsOextNm,_lhsOfsRPatL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpatCRest,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                                                                                                                                                                                                                                                                   in  sem_RecPatExpr_Ext_7)) of
                                                                                                                                                                                                                                                                                                                                            { ( sem_RecPatExpr_7) | True ->
                                                                                                                                                                                                                                                                                                                                            ( _lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOscopeGam,_lhsOty,_lhsOtyVarMp,sem_RecPatExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                                                                                                                                        in  sem_RecPatExpr_Ext_6)) of
                                                                                                                                                                                                                                                                                 { ( sem_RecPatExpr_6) | True ->
                                                                                                                                                                                                                                                                                 ( _lhsOpatTyVarMp,_lhsOvalGam,sem_RecPatExpr_6) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                                                                    in  sem_RecPatExpr_Ext_5)) of
                                                                                                                                                                                                             { ( sem_RecPatExpr_5) | True ->
                                                                                                                                                                                                             ( _lhsOchrInstDeclSq,_lhsOhasAFldRef,_lhsOpositionalFldNmL,sem_RecPatExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))
                                                                                                                                                                in  sem_RecPatExpr_Ext_4)) of
                                                                                                                                                         { ( sem_RecPatExpr_4) | True ->
                                                                                                                                                         ( _lhsOkiVarMp,_lhsOpolVarMp,_lhsOtyGam,_lhsOtyKiGam,sem_RecPatExpr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))
                                                                                                     in  sem_RecPatExpr_Ext_3)) of
                                                                                              { ( sem_RecPatExpr_3) | True ->
                                                                                              ( _lhsOpredSameScopeCounter,sem_RecPatExpr_3) }) }) }) }) }) }) }) }) }) })))))
                                                            in  sem_RecPatExpr_Ext_2)) of
                                                     { ( sem_RecPatExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_RecPatExpr_2) }) }) }) }) }) }) })))
                        in  sem_RecPatExpr_Ext_1)) of
                 { ( sem_RecPatExpr_1) | True ->
                 ( _lhsOrange,sem_RecPatExpr_1) }) }) }) }) })


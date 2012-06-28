


module EH101.EH.MainAG_PrExpr where

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

-- PrExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      chained attribute:
         tyGam                : TyGam
      synthesized attributes:
         prTy                 : Ty
         ty                   : Ty
   visit 3:
      inherited attribute:
         knPolCtx             : Polarity
      synthesized attribute:
         polVarL              : [Polarity]
   visit 4:
      inherited attribute:
         polGam               : PolGam
      chained attribute:
         polVarMp             : VarMp
   visit 5:
      chained attribute:
         tyKiGam              : TyKiGam
      synthesized attribute:
         intlTyKiGam          : TyKiGam
   visit 6:
      chained attribute:
         kiVarMp              : VarMp
      synthesized attributes:
         gathTyVarPolGam      : PolGam
         ki                   : Ty
         tyVarWildMp          : TyVarWildMp
   visit 7:
      inherited attribute:
         clGam                : ClGam
      synthesized attribute:
         evTy                 : Ty
   visit 8:
      inherited attributes:
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         finTyVarMp           : VarMp
         kiGam                : KiGam
         moduleNm             : HsName
         opts                 : EHCOpts
         tyKiGlobFreeTvarS    : TyVarIdS
         tyTyGlobFreeTvarS    : TyVarIdS
         tyTyTySigFreeTvarS   : TyVarIdS
         valTyGlobFreeTvarS   : TyVarIdS
      synthesized attributes:
         allErrSq             : ErrSq
         clMissNmS            : HsNameS
         clNmS                : HsNameS
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         pp                   : PP_Doc
         pr                   : Pred
   alternatives:
      alternative Arrow:
         child hsrange        : {Range}
         child arg            : PrExpr 
         child res            : PrExpr 
         visit 0:
            local range       : {Range}
         visit 2:
            local prTy        : _
            local pr          : {Pred}
            local ty          : {Ty}
         visit 3:
            intra pr          : {Pred}
         visit 4:
            intra pr          : {Pred}
         visit 5:
            intra pr          : {Pred}
         visit 6:
            intra pr          : {Pred}
         visit 7:
            intra pr          : {Pred}
         visit 8:
            local pp          : _
            intra pr          : {Pred}
      alternative Class:
         child hsrange        : {Range}
         child nm             : {HsName}
         child tyExprs        : TyExprs 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup196     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            local pr          : {Pred}
            local ty          : {Ty}
            local prTy        : _
            intra _tup196     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup196     : {(UID,UID,UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
            intra pr          : {Pred}
         visit 4:
            intra _tup196     : {(UID,UID,UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
            intra pr          : {Pred}
         visit 5:
            intra _tup196     : {(UID,UID,UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
            intra pr          : {Pred}
         visit 6:
            local lUniq       : {UID}
            local _tup194     : {(TyKiGamInfo,ErrL)}
            local tkgi_       : {TyKiGamInfo}
            local fo_         : {FIOut}
            intra _tup196     : {(UID,UID,UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
            intra pr          : {Pred}
         visit 7:
            local lUniq3      : {UID}
            local lUniq2      : {UID}
            local _tup195     : {(ClGamInfo,Set HsName,Set HsName)}
            local clgi        : {ClGamInfo}
            local fe2         : _
            intra _tup196     : {(UID,UID,UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
            intra _tup194     : {(TyKiGamInfo,ErrL)}
            intra fo_         : {FIOut}
            intra pr          : {Pred}
         visit 8:
            local clMissNmS   : {Set HsName}
            local clNmS       : {Set HsName}
            local pp          : _
            local nmErrs      : {ErrL}
            local clKiNmErrs  : {ErrL}
            local gathMentrelFilterMp : _
            intra _tup195     : {(ClGamInfo,Set HsName,Set HsName)}
            intra range       : {Range}
            intra _tup194     : {(TyKiGamInfo,ErrL)}
            intra fo_         : {FIOut}
            intra pr          : {Pred}
      alternative DynVar:
         child hsrange        : {Range}
         child nm             : {HsName}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 2:
            local pr          : {Pred}
            local ty          : {Ty}
            local prTy        : _
         visit 3:
            intra pr          : {Pred}
         visit 4:
            intra pr          : {Pred}
         visit 5:
            intra pr          : {Pred}
         visit 6:
            intra pr          : {Pred}
         visit 7:
            intra pr          : {Pred}
         visit 8:
            local pp          : _
            intra pr          : {Pred}
      alternative Eq:
         child hsrange        : {Range}
         child tyExpr1        : TyExpr 
         child tyExpr2        : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup197     : {(UID,UID)}
         visit 2:
            local pr          : {Pred}
            local ty          : {Ty}
            local prTy        : _
            intra _tup197     : {(UID,UID)}
         visit 3:
            intra _tup197     : {(UID,UID)}
            intra pr          : {Pred}
         visit 4:
            intra _tup197     : {(UID,UID)}
            intra pr          : {Pred}
         visit 5:
            intra _tup197     : {(UID,UID)}
            intra pr          : {Pred}
         visit 6:
            local lUniq       : {UID}
            local fo_         : {FIOut}
            intra _tup197     : {(UID,UID)}
            intra pr          : {Pred}
         visit 7:
            intra pr          : {Pred}
         visit 8:
            local pp          : _
            intra pr          : {Pred}
      alternative Forall:
         child hsrange        : {Range}
         child tyVar          : {HsName}
         child prExpr         : PrExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup199     : {(UID,UID,UID)}
         visit 2:
            local lUniq_ki    : {UID}
            local lUniq       : {UID}
            local _tup198     : {(UID,TyGamInfo,TyKiGamInfo)}
            local tgi_        : {TyGamInfo}
            local tv          : {UID}
            local prTy        : _
            local pr          : {Pred}
            local ty          : {Ty}
            intra _tup199     : {(UID,UID,UID)}
         visit 3:
            intra _tup198     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra pr          : {Pred}
         visit 4:
            intra _tup198     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra pr          : {Pred}
         visit 5:
            local tkgi_       : {TyKiGamInfo}
            local tyKiGamNew  : _
            intra _tup198     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra pr          : {Pred}
         visit 6:
            intra pr          : {Pred}
         visit 7:
            intra pr          : {Pred}
         visit 8:
            local pp          : _
            intra pr          : {Pred}
      alternative Lacks:
         child hsrange        : {Range}
         child rowTyExpr      : RowTyExpr 
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 2:
            local pr          : {Pred}
            local ty          : {Ty}
            local prTy        : _
         visit 3:
            intra pr          : {Pred}
         visit 4:
            intra pr          : {Pred}
         visit 5:
            intra pr          : {Pred}
         visit 6:
            intra pr          : {Pred}
         visit 7:
            intra pr          : {Pred}
         visit 8:
            local pp          : _
            intra pr          : {Pred}
-}
sem_PrExpr_Arrow :: Range ->
                    T_PrExpr  ->
                    T_PrExpr  ->
                    T_PrExpr 

sem_PrExpr_Arrow hsrange_ arg_ res_  | hsrange_ `seq` (arg_ `seq` (res_ `seq` (True))) =
    (case (res_ ) of
     { ( _resIrange,res_1) | True ->
         (case (arg_ ) of
          { ( _argIrange,arg_1) | True ->
              (case (rangeUnions [hsrange_, _argIrange    , _resIrange   ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_PrExpr_Arrow_1 :: T_PrExpr_1 
                            sem_PrExpr_Arrow_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (_lhsIgUniq) of
                                       { _argOgUniq | _argOgUniq `seq` (True) ->
                                       (case (arg_1 _argOgUniq ) of
                                        { ( _argIgUniq,arg_2) | True ->
                                            (case (_argIgUniq) of
                                             { _resOgUniq | _resOgUniq `seq` (True) ->
                                             (case (res_1 _resOgUniq ) of
                                              { ( _resIgUniq,res_2) | True ->
                                                  (case (_resIgUniq) of
                                                   { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                   (case ((let sem_PrExpr_Arrow_2 :: T_PrExpr_2 
                                                               sem_PrExpr_Arrow_2  =
                                                                   (\ _lhsItyGam ->
                                                                        _lhsItyGam `seq`
                                                                        ((case (_lhsItyGam) of
                                                                          { _argOtyGam | _argOtyGam `seq` (True) ->
                                                                          (case (arg_2 _argOtyGam ) of
                                                                           { ( _argIprTy,_argIty,_argItyGam,arg_3) | True ->
                                                                               (case (_argItyGam) of
                                                                                { _resOtyGam | _resOtyGam `seq` (True) ->
                                                                                (case (res_2 _resOtyGam ) of
                                                                                 { ( _resIprTy,_resIty,_resItyGam,res_3) | True ->
                                                                                     (case ([_argIprTy] `mkArrow` _resIprTy) of
                                                                                      { _prTy | _prTy `seq` (True) ->
                                                                                      (case (_prTy) of
                                                                                       { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                                       (case (Pred_Pred _prTy) of
                                                                                        { _pr | _pr `seq` (True) ->
                                                                                        (case (mkTyPr _pr) of
                                                                                         { _ty | _ty `seq` (True) ->
                                                                                         (case (_ty) of
                                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                                          (case (_resItyGam) of
                                                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                           (case ((let sem_PrExpr_Arrow_3 :: T_PrExpr_3 
                                                                                                       sem_PrExpr_Arrow_3  =
                                                                                                           (\ _lhsIknPolCtx ->
                                                                                                                _lhsIknPolCtx `seq`
                                                                                                                ((case (_lhsIknPolCtx) of
                                                                                                                  { _resOknPolCtx | _resOknPolCtx `seq` (True) ->
                                                                                                                  (case (res_3 _resOknPolCtx ) of
                                                                                                                   { ( _resIpolVarL,res_4) | True ->
                                                                                                                       (case (_resIpolVarL) of
                                                                                                                        { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                                        (case ((let sem_PrExpr_Arrow_4 :: T_PrExpr_4 
                                                                                                                                    sem_PrExpr_Arrow_4  =
                                                                                                                                        (\ _lhsIpolGam
                                                                                                                                           _lhsIpolVarMp ->
                                                                                                                                             _lhsIpolGam `seq`
                                                                                                                                             (_lhsIpolVarMp `seq`
                                                                                                                                              ((case (_lhsIpolVarMp) of
                                                                                                                                                { _argOpolVarMp | _argOpolVarMp `seq` (True) ->
                                                                                                                                                (case (_lhsIpolGam) of
                                                                                                                                                 { _argOpolGam | _argOpolGam `seq` (True) ->
                                                                                                                                                 (case (mkPolNegate _lhsIknPolCtx) of
                                                                                                                                                  { _argOknPolCtx | _argOknPolCtx `seq` (True) ->
                                                                                                                                                  (case (arg_3 _argOknPolCtx ) of
                                                                                                                                                   { ( _argIpolVarL,arg_4) | True ->
                                                                                                                                                       (case (arg_4 _argOpolGam _argOpolVarMp ) of
                                                                                                                                                        { ( _argIpolVarMp,arg_5) | True ->
                                                                                                                                                            (case (_argIpolVarMp) of
                                                                                                                                                             { _resOpolVarMp | _resOpolVarMp `seq` (True) ->
                                                                                                                                                             (case (_lhsIpolGam) of
                                                                                                                                                              { _resOpolGam | _resOpolGam `seq` (True) ->
                                                                                                                                                              (case (res_4 _resOpolGam _resOpolVarMp ) of
                                                                                                                                                               { ( _resIpolVarMp,res_5) | True ->
                                                                                                                                                                   (case (_resIpolVarMp) of
                                                                                                                                                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                                    (case ((let sem_PrExpr_Arrow_5 :: T_PrExpr_5 
                                                                                                                                                                                sem_PrExpr_Arrow_5  =
                                                                                                                                                                                    (\ _lhsItyKiGam ->
                                                                                                                                                                                         _lhsItyKiGam `seq`
                                                                                                                                                                                         ((case (_lhsItyKiGam) of
                                                                                                                                                                                           { _argOtyKiGam | _argOtyKiGam `seq` (True) ->
                                                                                                                                                                                           (case (arg_5 _argOtyKiGam ) of
                                                                                                                                                                                            { ( _argIintlTyKiGam,_argItyKiGam,arg_6) | True ->
                                                                                                                                                                                                (case (_argItyKiGam) of
                                                                                                                                                                                                 { _resOtyKiGam | _resOtyKiGam `seq` (True) ->
                                                                                                                                                                                                 (case (res_5 _resOtyKiGam ) of
                                                                                                                                                                                                  { ( _resIintlTyKiGam,_resItyKiGam,res_6) | True ->
                                                                                                                                                                                                      (case (_argIintlTyKiGam `gamUnion` _resIintlTyKiGam) of
                                                                                                                                                                                                       { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                                                       (case (_resItyKiGam) of
                                                                                                                                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                                        (case ((let sem_PrExpr_Arrow_6 :: T_PrExpr_6 
                                                                                                                                                                                                                    sem_PrExpr_Arrow_6  =
                                                                                                                                                                                                                        (\ _lhsIkiVarMp ->
                                                                                                                                                                                                                             _lhsIkiVarMp `seq`
                                                                                                                                                                                                                             ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                                               { _argOkiVarMp | _argOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                               (case (arg_6 _argOkiVarMp ) of
                                                                                                                                                                                                                                { ( _argIgathTyVarPolGam,_argIki,_argIkiVarMp,_argItyVarWildMp,arg_7) | True ->
                                                                                                                                                                                                                                    (case (_argIkiVarMp) of
                                                                                                                                                                                                                                     { _resOkiVarMp | _resOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                     (case (res_6 _resOkiVarMp ) of
                                                                                                                                                                                                                                      { ( _resIgathTyVarPolGam,_resIki,_resIkiVarMp,_resItyVarWildMp,res_7) | True ->
                                                                                                                                                                                                                                          (case (_argIgathTyVarPolGam `gamUnion` _resIgathTyVarPolGam) of
                                                                                                                                                                                                                                           { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                                                           (case (kiStar) of
                                                                                                                                                                                                                                            { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                                                            (case (_resIkiVarMp) of
                                                                                                                                                                                                                                             { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                             (case (_argItyVarWildMp `Map.union` _resItyVarWildMp) of
                                                                                                                                                                                                                                              { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                                              (case ((let sem_PrExpr_Arrow_7 :: T_PrExpr_7 
                                                                                                                                                                                                                                                          sem_PrExpr_Arrow_7  =
                                                                                                                                                                                                                                                              (\ _lhsIclGam ->
                                                                                                                                                                                                                                                                   _lhsIclGam `seq`
                                                                                                                                                                                                                                                                   ((case (_lhsIclGam) of
                                                                                                                                                                                                                                                                     { _resOclGam | _resOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                     (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                      { _argOclGam | _argOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                      (case (res_7 _resOclGam ) of
                                                                                                                                                                                                                                                                       { ( _resIevTy,res_8) | True ->
                                                                                                                                                                                                                                                                           (case (arg_7 _argOclGam ) of
                                                                                                                                                                                                                                                                            { ( _argIevTy,arg_8) | True ->
                                                                                                                                                                                                                                                                                (case ([_argIevTy] `mkArrow` _resIevTy) of
                                                                                                                                                                                                                                                                                 { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case ((let sem_PrExpr_Arrow_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                                                                             sem_PrExpr_Arrow_8  =
                                                                                                                                                                                                                                                                                                 (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                                                                                    _lhsIfinTyKiGam
                                                                                                                                                                                                                                                                                                    _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                                    _lhsIkiGam
                                                                                                                                                                                                                                                                                                    _lhsImoduleNm
                                                                                                                                                                                                                                                                                                    _lhsIopts
                                                                                                                                                                                                                                                                                                    _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                                                                    _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                                                    _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                                                                    _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                                                      _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                                                                      (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                                                                                       (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                                                        (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                                                                         (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                                                          (_lhsIopts `seq`
                                                                                                                                                                                                                                                                                                           (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                            (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                             (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                              (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                               ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                 { _resOvalTyGlobFreeTvarS | _resOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                                  { _resOtyTyTySigFreeTvarS | _resOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                   { _resOtyTyGlobFreeTvarS | _resOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                    { _resOtyKiGlobFreeTvarS | _resOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                                     { _resOopts | _resOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                      { _resOmoduleNm | _resOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                      (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                       { _resOkiGam | _resOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                        { _resOfinTyVarMp | _resOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                         { _resOfinTyKiGam | _resOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                         (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                          { _resOfinKiVarMp | _resOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                          (case (res_8 _resOfinKiVarMp _resOfinTyKiGam _resOfinTyVarMp _resOkiGam _resOmoduleNm _resOopts _resOtyKiGlobFreeTvarS _resOtyTyGlobFreeTvarS _resOtyTyTySigFreeTvarS _resOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                           { ( _resIallErrSq,_resIclMissNmS,_resIclNmS,_resIerrSq,_resIgathMentrelFilterMp,_resIpp,_resIpr) | True ->
                                                                                                                                                                                                                                                                                                                               (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                { _argOvalTyGlobFreeTvarS | _argOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                 { _argOtyTyTySigFreeTvarS | _argOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                 (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                  { _argOtyTyGlobFreeTvarS | _argOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                  (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                   { _argOtyKiGlobFreeTvarS | _argOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                   (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                                                    { _argOopts | _argOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                     { _argOmoduleNm | _argOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                                      { _argOkiGam | _argOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                       { _argOfinTyVarMp | _argOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                                        { _argOfinTyKiGam | _argOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                        (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                                         { _argOfinKiVarMp | _argOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                         (case (arg_8 _argOfinKiVarMp _argOfinTyKiGam _argOfinTyVarMp _argOkiGam _argOmoduleNm _argOopts _argOtyKiGlobFreeTvarS _argOtyTyGlobFreeTvarS _argOtyTyTySigFreeTvarS _argOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                                          { ( _argIallErrSq,_argIclMissNmS,_argIclNmS,_argIerrSq,_argIgathMentrelFilterMp,_argIpp,_argIpr) | True ->
                                                                                                                                                                                                                                                                                                                                              (case (_argIallErrSq `Seq.union` _resIallErrSq) of
                                                                                                                                                                                                                                                                                                                                               { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                               (case (_argIclMissNmS `Set.union` _resIclMissNmS) of
                                                                                                                                                                                                                                                                                                                                                { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                (case (_argIclNmS `Set.union` _resIclNmS) of
                                                                                                                                                                                                                                                                                                                                                 { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                 (case (_argIerrSq `Seq.union` _resIerrSq) of
                                                                                                                                                                                                                                                                                                                                                  { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                  (case (_argIgathMentrelFilterMp `mentrelFilterMpUnion` _resIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                   { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                   (case (_argIpp >#< hsnPrArrow >#< _resIpp) of
                                                                                                                                                                                                                                                                                                                                                    { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                    (case (_pp) of
                                                                                                                                                                                                                                                                                                                                                     { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                     (case (_pr) of
                                                                                                                                                                                                                                                                                                                                                      { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                      ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                                                                         in  sem_PrExpr_Arrow_8)) of
                                                                                                                                                                                                                                                                                  { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                                                                                  ( _lhsOevTy,sem_PrExpr_8) }) }) }) }) }) })))
                                                                                                                                                                                                                                                      in  sem_PrExpr_Arrow_7)) of
                                                                                                                                                                                                                                               { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                                                               ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                                                in  sem_PrExpr_Arrow_6)) of
                                                                                                                                                                                                         { ( sem_PrExpr_6) | True ->
                                                                                                                                                                                                         ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) }) }) })))
                                                                                                                                                                            in  sem_PrExpr_Arrow_5)) of
                                                                                                                                                                     { ( sem_PrExpr_5) | True ->
                                                                                                                                                                     ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                                in  sem_PrExpr_Arrow_4)) of
                                                                                                                         { ( sem_PrExpr_4) | True ->
                                                                                                                         ( _lhsOpolVarL,sem_PrExpr_4) }) }) }) })))
                                                                                                   in  sem_PrExpr_Arrow_3)) of
                                                                                            { ( sem_PrExpr_3) | True ->
                                                                                            ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) }) }) })))
                                                           in  sem_PrExpr_Arrow_2)) of
                                                    { ( sem_PrExpr_2) | True ->
                                                    ( _lhsOgUniq,sem_PrExpr_2) }) }) }) }) }) })))
                        in  sem_PrExpr_Arrow_1)) of
                 { ( sem_PrExpr_1) | True ->
                 ( _lhsOrange,sem_PrExpr_1) }) }) }) }) })

sem_PrExpr_Class :: Range ->
                    HsName ->
                    T_TyExprs  ->
                    T_PrExpr 

sem_PrExpr_Class hsrange_ nm_ tyExprs_  | hsrange_ `seq` (nm_ `seq` (tyExprs_ `seq` (True))) =
    (case (tyExprs_ ) of
     { ( _tyExprsIrange,tyExprs_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprsIrange, _tyExprsIrange
                                                                    ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_PrExpr_Class_1 :: T_PrExpr_1 
                       sem_PrExpr_Class_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq3) -> (__cont, lUniq,lUniq2,lUniq3)}}} )) of
                                  { __tup196 | __tup196 `seq` (True) ->
                                  (case (__tup196) of
                                   { (_tyExprsOgUniq,_,_,_) | _tyExprsOgUniq `seq` (True) ->
                                   (case (tyExprs_1 _tyExprsOgUniq ) of
                                    { ( _tyExprsIgUniq,tyExprs_2) | True ->
                                        (case (_tyExprsIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_PrExpr_Class_2 :: T_PrExpr_2 
                                                     sem_PrExpr_Class_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (_lhsItyGam) of
                                                                { _tyExprsOtyGam | _tyExprsOtyGam `seq` (True) ->
                                                                (case (tyExprs_2 _tyExprsOtyGam ) of
                                                                 { ( _tyExprsIpolVarL,_tyExprsItyGam,_tyExprsItyL,tyExprs_3) | True ->
                                                                     (case (Pred_Class (nm_ `mkConApp` _tyExprsItyL)) of
                                                                      { _pr | _pr `seq` (True) ->
                                                                      (case (mkTyPr _pr) of
                                                                       { _ty | _ty `seq` (True) ->
                                                                       (case (_ty) of
                                                                        { _prTy | _prTy `seq` (True) ->
                                                                        (case (_prTy) of
                                                                         { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                         (case (_ty) of
                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                          (case (_tyExprsItyGam) of
                                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                           (case ((let sem_PrExpr_Class_3 :: T_PrExpr_3 
                                                                                       sem_PrExpr_Class_3  =
                                                                                           (\ _lhsIknPolCtx ->
                                                                                                _lhsIknPolCtx `seq`
                                                                                                ((case (_tyExprsIpolVarL) of
                                                                                                  { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                  (case ((let sem_PrExpr_Class_4 :: T_PrExpr_4 
                                                                                                              sem_PrExpr_Class_4  =
                                                                                                                  (\ _lhsIpolGam
                                                                                                                     _lhsIpolVarMp ->
                                                                                                                       _lhsIpolGam `seq`
                                                                                                                       (_lhsIpolVarMp `seq`
                                                                                                                        ((case (_lhsIpolVarMp) of
                                                                                                                          { _tyExprsOpolVarMp | _tyExprsOpolVarMp `seq` (True) ->
                                                                                                                          (case (_lhsIpolGam) of
                                                                                                                           { _tyExprsOpolGam | _tyExprsOpolGam `seq` (True) ->
                                                                                                                           (case (tyExprs_3 _tyExprsOpolGam _tyExprsOpolVarMp ) of
                                                                                                                            { ( _tyExprsIpolVarMp,tyExprs_4) | True ->
                                                                                                                                (case (_tyExprsIpolVarMp) of
                                                                                                                                 { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                 (case ((let sem_PrExpr_Class_5 :: T_PrExpr_5 
                                                                                                                                             sem_PrExpr_Class_5  =
                                                                                                                                                 (\ _lhsItyKiGam ->
                                                                                                                                                      _lhsItyKiGam `seq`
                                                                                                                                                      ((case (_lhsItyKiGam) of
                                                                                                                                                        { _tyExprsOtyKiGam | _tyExprsOtyKiGam `seq` (True) ->
                                                                                                                                                        (case (tyExprs_4 _tyExprsOtyKiGam ) of
                                                                                                                                                         { ( _tyExprsIintlTyKiGam,_tyExprsItyKiGam,tyExprs_5) | True ->
                                                                                                                                                             (case (_tyExprsIintlTyKiGam) of
                                                                                                                                                              { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                              (case (_tyExprsItyKiGam) of
                                                                                                                                                               { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                               (case ((let sem_PrExpr_Class_6 :: T_PrExpr_6 
                                                                                                                                                                           sem_PrExpr_Class_6  =
                                                                                                                                                                               (\ _lhsIkiVarMp ->
                                                                                                                                                                                    _lhsIkiVarMp `seq`
                                                                                                                                                                                    ((case (_lhsIkiVarMp) of
                                                                                                                                                                                      { _tyExprsOkiVarMp | _tyExprsOkiVarMp `seq` (True) ->
                                                                                                                                                                                      (case (tyExprs_5 _tyExprsOkiVarMp ) of
                                                                                                                                                                                       { ( _tyExprsIgathTyVarPolGam,_tyExprsIkiL,_tyExprsIkiVarMp,_tyExprsItyVarWildMp,tyExprs_6) | True ->
                                                                                                                                                                                           (case (_tyExprsIgathTyVarPolGam) of
                                                                                                                                                                                            { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                            (case (kiStar) of
                                                                                                                                                                                             { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                             (case (__tup196) of
                                                                                                                                                                                              { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                              (case (tyKiGamLookupByNameErr (hsnClass2Kind nm_) _lhsItyKiGam) of
                                                                                                                                                                                               { __tup194 | __tup194 `seq` (True) ->
                                                                                                                                                                                               (case (__tup194) of
                                                                                                                                                                                                { (_tkgi_,_) | _tkgi_ `seq` (True) ->
                                                                                                                                                                                                (case (fitsIn weakFIOpts defaultFIEnv _lUniq _tyExprsIkiVarMp
                                                                                                                                                                                                         (_tyExprsIkiL `mkArrow` kiStar) (tkgiKi _tkgi_)) of
                                                                                                                                                                                                 { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                 (case (foVarMp _fo_ `varUpd` _tyExprsIkiVarMp) of
                                                                                                                                                                                                  { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                  (case (_tyExprsItyVarWildMp) of
                                                                                                                                                                                                   { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                   (case ((let sem_PrExpr_Class_7 :: T_PrExpr_7 
                                                                                                                                                                                                               sem_PrExpr_Class_7  =
                                                                                                                                                                                                                   (\ _lhsIclGam ->
                                                                                                                                                                                                                        _lhsIclGam `seq`
                                                                                                                                                                                                                        ((case (__tup196) of
                                                                                                                                                                                                                          { (_,_,_,_lUniq3) | _lUniq3 `seq` (True) ->
                                                                                                                                                                                                                          (case (__tup196) of
                                                                                                                                                                                                                           { (_,_,_lUniq2,_) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                           (case (case gamLookup nm_ _lhsIclGam of
                                                                                                                                                                                                                                      Just p   -> (p          ,Set.singleton nm_  ,Set.empty          )
                                                                                                                                                                                                                                      Nothing  -> (emptyCLGI  ,Set.empty          ,Set.singleton nm_  )) of
                                                                                                                                                                                                                            { __tup195 | __tup195 `seq` (True) ->
                                                                                                                                                                                                                            (case (__tup195) of
                                                                                                                                                                                                                             { (_clgi,_,_) | _clgi `seq` (True) ->
                                                                                                                                                                                                                             (case (defaultFIEnv
                                                                                                                                                                                                                                        { feTyGam = _lhsItyGam
                                                                                                                                                                                                                                        , fePolGam = _lhsIpolGam
                                                                                                                                                                                                                                        }) of
                                                                                                                                                                                                                              { _fe2 | _fe2 `seq` (True) ->
                                                                                                                                                                                                                              (case (let  fo = fitsIn  (predFIOpts {fioDontBind = varFreeSet _ty}) _fe2 _lUniq3 (emptyVarMp :: VarMp)
                                                                                                                                                                                                                                                       (clgiPrToEvidTy _clgi) ([_ty] `mkArrow` mkNewTyVar _lUniq2)
                                                                                                                                                                                                                                     in   snd $ tyArrowArgRes $ foVarMp fo `varUpd` foTy fo) of
                                                                                                                                                                                                                               { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                               (case ((let sem_PrExpr_Class_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                           sem_PrExpr_Class_8  =
                                                                                                                                                                                                                                               (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                                  _lhsIfinTyKiGam
                                                                                                                                                                                                                                                  _lhsIfinTyVarMp
                                                                                                                                                                                                                                                  _lhsIkiGam
                                                                                                                                                                                                                                                  _lhsImoduleNm
                                                                                                                                                                                                                                                  _lhsIopts
                                                                                                                                                                                                                                                  _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                  _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                  _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                  _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                    _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                    (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                                     (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                      (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                       (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                        (_lhsIopts `seq`
                                                                                                                                                                                                                                                         (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                          (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                           (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                            (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                             ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                               { _tyExprsOvalTyGlobFreeTvarS | _tyExprsOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                               (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                { _tyExprsOtyTyTySigFreeTvarS | _tyExprsOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                 { _tyExprsOtyTyGlobFreeTvarS | _tyExprsOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                 (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                  { _tyExprsOtyKiGlobFreeTvarS | _tyExprsOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                  (case (_lhsIopts) of
                                                                                                                                                                                                                                                                   { _tyExprsOopts | _tyExprsOopts `seq` (True) ->
                                                                                                                                                                                                                                                                   (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                    { _tyExprsOmoduleNm | _tyExprsOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                    (case (_lhsIknPolCtx) of
                                                                                                                                                                                                                                                                     { _tyExprsOknPolCtx | _tyExprsOknPolCtx `seq` (True) ->
                                                                                                                                                                                                                                                                     (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                      { _tyExprsOkiGam | _tyExprsOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                      (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                       { _tyExprsOfinTyVarMp | _tyExprsOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                       (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                        { _tyExprsOfinTyKiGam | _tyExprsOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                        (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                         { _tyExprsOfinKiVarMp | _tyExprsOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                         (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                          { _tyExprsOclGam | _tyExprsOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                          (case (tyExprs_6 _tyExprsOclGam _tyExprsOfinKiVarMp _tyExprsOfinTyKiGam _tyExprsOfinTyVarMp _tyExprsOkiGam _tyExprsOknPolCtx _tyExprsOmoduleNm _tyExprsOopts _tyExprsOtyKiGlobFreeTvarS _tyExprsOtyTyGlobFreeTvarS _tyExprsOtyTyTySigFreeTvarS _tyExprsOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                           { ( _tyExprsIallErrSq,_tyExprsIclMissNmS,_tyExprsIclNmS,_tyExprsIerrSq,_tyExprsIgathMentrelFilterMp,_tyExprsIpp,_tyExprsIppL) | True ->
                                                                                                                                                                                                                                                                               (case (_tyExprsIallErrSq) of
                                                                                                                                                                                                                                                                                { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (__tup195) of
                                                                                                                                                                                                                                                                                 { (_,_,_clMissNmS) | _clMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_clMissNmS  `Set.union`  _tyExprsIclMissNmS) of
                                                                                                                                                                                                                                                                                  { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (__tup195) of
                                                                                                                                                                                                                                                                                   { (_,_clNmS,_) | _clNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_clNmS      `Set.union`  _tyExprsIclNmS) of
                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (ppSpaced (pp nm_ : _tyExprsIppL)) of
                                                                                                                                                                                                                                                                                     { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (if ehcOptHsChecksInEH _lhsIopts
                                                                                                                                                                                                                                                                                            then rngLift _range checkClNms _clMissNmS
                                                                                                                                                                                                                                                                                            else []) of
                                                                                                                                                                                                                                                                                      { _nmErrs | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (__tup194) of
                                                                                                                                                                                                                                                                                       { (_,_clKiNmErrs) | _clKiNmErrs `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (rngLift _range mkNestErr' _pp [ _tyExprsIerrSq
                                                                                                                                                                                                                                                                                                                            , Seq.fromList $ firstNotEmpty [_nmErrs,_clKiNmErrs], foErrSq _fo_
                                                                                                                                                                                                                                                                                                                            ]) of
                                                                                                                                                                                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Type                   nm_
                                                                                                                                                                                                                                                                                               `mentrelFilterMpUnion`
                                                                                                                                                                                                                                                                                               _tyExprsIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                         { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                          { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_pp) of
                                                                                                                                                                                                                                                                                           { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_pr) of
                                                                                                                                                                                                                                                                                            { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                                            ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                       in  sem_PrExpr_Class_8)) of
                                                                                                                                                                                                                                { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                                ( _lhsOevTy,sem_PrExpr_8) }) }) }) }) }) }) })))
                                                                                                                                                                                                           in  sem_PrExpr_Class_7)) of
                                                                                                                                                                                                    { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                    ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                       in  sem_PrExpr_Class_6)) of
                                                                                                                                                                { ( sem_PrExpr_6) | True ->
                                                                                                                                                                ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) })))
                                                                                                                                         in  sem_PrExpr_Class_5)) of
                                                                                                                                  { ( sem_PrExpr_5) | True ->
                                                                                                                                  ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }))))
                                                                                                          in  sem_PrExpr_Class_4)) of
                                                                                                   { ( sem_PrExpr_4) | True ->
                                                                                                   ( _lhsOpolVarL,sem_PrExpr_4) }) })))
                                                                                   in  sem_PrExpr_Class_3)) of
                                                                            { ( sem_PrExpr_3) | True ->
                                                                            ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) })))
                                                 in  sem_PrExpr_Class_2)) of
                                          { ( sem_PrExpr_2) | True ->
                                          ( _lhsOgUniq,sem_PrExpr_2) }) }) }) }) })))
                   in  sem_PrExpr_Class_1)) of
            { ( sem_PrExpr_1) | True ->
            ( _lhsOrange,sem_PrExpr_1) }) }) }) })

sem_PrExpr_DynVar :: Range ->
                     HsName ->
                     T_TyExpr  ->
                     T_PrExpr 

sem_PrExpr_DynVar hsrange_ nm_ tyExpr_  | hsrange_ `seq` (nm_ `seq` (tyExpr_ `seq` (True))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_PrExpr_DynVar_1 :: T_PrExpr_1 
                       sem_PrExpr_DynVar_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                  (case (tyExpr_1 _tyExprOgUniq ) of
                                   { ( _tyExprIgUniq,tyExpr_2) | True ->
                                       (case (_tyExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_PrExpr_DynVar_2 :: T_PrExpr_2 
                                                    sem_PrExpr_DynVar_2  =
                                                        (\ _lhsItyGam ->
                                                             _lhsItyGam `seq`
                                                             ((case (_lhsItyGam) of
                                                               { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                               (case (tyExpr_2 _tyExprOtyGam ) of
                                                                { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                    (case (Pred_Class ((hsnDynVar `hsnConcat` nm_) `mkConApp` [_tyExprIty])) of
                                                                     { _pr | _pr `seq` (True) ->
                                                                     (case (mkTyPr _pr) of
                                                                      { _ty | _ty `seq` (True) ->
                                                                      (case (_ty) of
                                                                       { _prTy | _prTy `seq` (True) ->
                                                                       (case (_prTy) of
                                                                        { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                        (case (_ty) of
                                                                         { _lhsOty | _lhsOty `seq` (True) ->
                                                                         (case (_tyExprItyGam) of
                                                                          { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                          (case ((let sem_PrExpr_DynVar_3 :: T_PrExpr_3 
                                                                                      sem_PrExpr_DynVar_3  =
                                                                                          (\ _lhsIknPolCtx ->
                                                                                               _lhsIknPolCtx `seq`
                                                                                               ((case (_lhsIknPolCtx) of
                                                                                                 { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                 (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                  { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                      (case (_tyExprIpolVarL) of
                                                                                                       { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                       (case ((let sem_PrExpr_DynVar_4 :: T_PrExpr_4 
                                                                                                                   sem_PrExpr_DynVar_4  =
                                                                                                                       (\ _lhsIpolGam
                                                                                                                          _lhsIpolVarMp ->
                                                                                                                            _lhsIpolGam `seq`
                                                                                                                            (_lhsIpolVarMp `seq`
                                                                                                                             ((case (_lhsIpolVarMp) of
                                                                                                                               { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                               (case (_lhsIpolGam) of
                                                                                                                                { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                                (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                                 { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                     (case (_tyExprIpolVarMp) of
                                                                                                                                      { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                      (case ((let sem_PrExpr_DynVar_5 :: T_PrExpr_5 
                                                                                                                                                  sem_PrExpr_DynVar_5  =
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
                                                                                                                                                                    (case ((let sem_PrExpr_DynVar_6 :: T_PrExpr_6 
                                                                                                                                                                                sem_PrExpr_DynVar_6  =
                                                                                                                                                                                    (\ _lhsIkiVarMp ->
                                                                                                                                                                                         _lhsIkiVarMp `seq`
                                                                                                                                                                                         ((case (_lhsIkiVarMp) of
                                                                                                                                                                                           { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                           (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                            { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                                (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                                 { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                 (case (kiStar) of
                                                                                                                                                                                                  { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                  (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                   { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                   (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                    { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                    (case ((let sem_PrExpr_DynVar_7 :: T_PrExpr_7 
                                                                                                                                                                                                                sem_PrExpr_DynVar_7  =
                                                                                                                                                                                                                    (\ _lhsIclGam ->
                                                                                                                                                                                                                         _lhsIclGam `seq`
                                                                                                                                                                                                                         ((case (_lhsIclGam) of
                                                                                                                                                                                                                           { _tyExprOclGam | _tyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                           (case (tyExpr_7 _tyExprOclGam ) of
                                                                                                                                                                                                                            { ( _tyExprIevTy,tyExpr_8) | True ->
                                                                                                                                                                                                                                (case (_tyExprIevTy) of
                                                                                                                                                                                                                                 { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                 (case ((let sem_PrExpr_DynVar_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                             sem_PrExpr_DynVar_8  =
                                                                                                                                                                                                                                                 (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                                    _lhsIfinTyKiGam
                                                                                                                                                                                                                                                    _lhsIfinTyVarMp
                                                                                                                                                                                                                                                    _lhsIkiGam
                                                                                                                                                                                                                                                    _lhsImoduleNm
                                                                                                                                                                                                                                                    _lhsIopts
                                                                                                                                                                                                                                                    _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                    _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                    _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                    _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                      _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                      (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                                       (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                        (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                         (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                          (_lhsIopts `seq`
                                                                                                                                                                                                                                                           (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                            (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                             (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                              (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                               ((case (_lhsIvalTyGlobFreeTvarS) of
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
                                                                                                                                                                                                                                                                                (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                 { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                  { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_tyExprIerrSq) of
                                                                                                                                                                                                                                                                                   { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                    { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (hsnDynVar >|< nm_ >#< "::" >#< _tyExprIpp) of
                                                                                                                                                                                                                                                                                     { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_pp) of
                                                                                                                                                                                                                                                                                      { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_pr) of
                                                                                                                                                                                                                                                                                       { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                                       ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                         in  sem_PrExpr_DynVar_8)) of
                                                                                                                                                                                                                                  { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                                  ( _lhsOevTy,sem_PrExpr_8) }) }) }) })))
                                                                                                                                                                                                            in  sem_PrExpr_DynVar_7)) of
                                                                                                                                                                                                     { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                     ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) })))
                                                                                                                                                                            in  sem_PrExpr_DynVar_6)) of
                                                                                                                                                                     { ( sem_PrExpr_6) | True ->
                                                                                                                                                                     ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) })))
                                                                                                                                              in  sem_PrExpr_DynVar_5)) of
                                                                                                                                       { ( sem_PrExpr_5) | True ->
                                                                                                                                       ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }))))
                                                                                                               in  sem_PrExpr_DynVar_4)) of
                                                                                                        { ( sem_PrExpr_4) | True ->
                                                                                                        ( _lhsOpolVarL,sem_PrExpr_4) }) }) }) })))
                                                                                  in  sem_PrExpr_DynVar_3)) of
                                                                           { ( sem_PrExpr_3) | True ->
                                                                           ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) })))
                                                in  sem_PrExpr_DynVar_2)) of
                                         { ( sem_PrExpr_2) | True ->
                                         ( _lhsOgUniq,sem_PrExpr_2) }) }) }) })))
                   in  sem_PrExpr_DynVar_1)) of
            { ( sem_PrExpr_1) | True ->
            ( _lhsOrange,sem_PrExpr_1) }) }) }) })

sem_PrExpr_Eq :: Range ->
                 T_TyExpr  ->
                 T_TyExpr  ->
                 T_PrExpr 

sem_PrExpr_Eq hsrange_ tyExpr1_ tyExpr2_  | hsrange_ `seq` (tyExpr1_ `seq` (tyExpr2_ `seq` (True))) =
    (case (tyExpr2_ ) of
     { ( _tyExpr2Irange,tyExpr2_1) | True ->
         (case (tyExpr1_ ) of
          { ( _tyExpr1Irange,tyExpr1_1) | True ->
              (case (rangeUnions [hsrange_, _tyExpr1Irange, _tyExpr2Irange
                                                                         ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_PrExpr_Eq_1 :: T_PrExpr_1 
                            sem_PrExpr_Eq_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                                       { __tup197 | __tup197 `seq` (True) ->
                                       (case (__tup197) of
                                        { (_tyExpr1OgUniq,_) | _tyExpr1OgUniq `seq` (True) ->
                                        (case (tyExpr1_1 _tyExpr1OgUniq ) of
                                         { ( _tyExpr1IgUniq,tyExpr1_2) | True ->
                                             (case (_tyExpr1IgUniq) of
                                              { _tyExpr2OgUniq | _tyExpr2OgUniq `seq` (True) ->
                                              (case (tyExpr2_1 _tyExpr2OgUniq ) of
                                               { ( _tyExpr2IgUniq,tyExpr2_2) | True ->
                                                   (case (_tyExpr2IgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_PrExpr_Eq_2 :: T_PrExpr_2 
                                                                sem_PrExpr_Eq_2  =
                                                                    (\ _lhsItyGam ->
                                                                         _lhsItyGam `seq`
                                                                         ((case (_lhsItyGam) of
                                                                           { _tyExpr1OtyGam | _tyExpr1OtyGam `seq` (True) ->
                                                                           (case (tyExpr1_2 _tyExpr1OtyGam ) of
                                                                            { ( _tyExpr1Ity,_tyExpr1ItyGam,tyExpr1_3) | True ->
                                                                                (case (_tyExpr1ItyGam) of
                                                                                 { _tyExpr2OtyGam | _tyExpr2OtyGam `seq` (True) ->
                                                                                 (case (tyExpr2_2 _tyExpr2OtyGam ) of
                                                                                  { ( _tyExpr2Ity,_tyExpr2ItyGam,tyExpr2_3) | True ->
                                                                                      (case (Pred_Eq _tyExpr1Ity _tyExpr2Ity) of
                                                                                       { _pr | _pr `seq` (True) ->
                                                                                       (case (mkTyPr _pr) of
                                                                                        { _ty | _ty `seq` (True) ->
                                                                                        (case (_ty) of
                                                                                         { _prTy | _prTy `seq` (True) ->
                                                                                         (case (_prTy) of
                                                                                          { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                                          (case (_ty) of
                                                                                           { _lhsOty | _lhsOty `seq` (True) ->
                                                                                           (case (_tyExpr2ItyGam) of
                                                                                            { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                            (case ((let sem_PrExpr_Eq_3 :: T_PrExpr_3 
                                                                                                        sem_PrExpr_Eq_3  =
                                                                                                            (\ _lhsIknPolCtx ->
                                                                                                                 _lhsIknPolCtx `seq`
                                                                                                                 ((case (_lhsIknPolCtx) of
                                                                                                                   { _tyExpr2OknPolCtx | _tyExpr2OknPolCtx `seq` (True) ->
                                                                                                                   (case (tyExpr2_3 _tyExpr2OknPolCtx ) of
                                                                                                                    { ( _tyExpr2IpolVarL,tyExpr2_4) | True ->
                                                                                                                        (case (_tyExpr2IpolVarL) of
                                                                                                                         { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                                         (case ((let sem_PrExpr_Eq_4 :: T_PrExpr_4 
                                                                                                                                     sem_PrExpr_Eq_4  =
                                                                                                                                         (\ _lhsIpolGam
                                                                                                                                            _lhsIpolVarMp ->
                                                                                                                                              _lhsIpolGam `seq`
                                                                                                                                              (_lhsIpolVarMp `seq`
                                                                                                                                               ((case (_lhsIpolVarMp) of
                                                                                                                                                 { _tyExpr1OpolVarMp | _tyExpr1OpolVarMp `seq` (True) ->
                                                                                                                                                 (case (_lhsIpolGam) of
                                                                                                                                                  { _tyExpr1OpolGam | _tyExpr1OpolGam `seq` (True) ->
                                                                                                                                                  (case (_lhsIknPolCtx) of
                                                                                                                                                   { _tyExpr1OknPolCtx | _tyExpr1OknPolCtx `seq` (True) ->
                                                                                                                                                   (case (tyExpr1_3 _tyExpr1OknPolCtx ) of
                                                                                                                                                    { ( _tyExpr1IpolVarL,tyExpr1_4) | True ->
                                                                                                                                                        (case (tyExpr1_4 _tyExpr1OpolGam _tyExpr1OpolVarMp ) of
                                                                                                                                                         { ( _tyExpr1ImbStrictness,_tyExpr1IpolVarMp,tyExpr1_5) | True ->
                                                                                                                                                             (case (_tyExpr1IpolVarMp) of
                                                                                                                                                              { _tyExpr2OpolVarMp | _tyExpr2OpolVarMp `seq` (True) ->
                                                                                                                                                              (case (_lhsIpolGam) of
                                                                                                                                                               { _tyExpr2OpolGam | _tyExpr2OpolGam `seq` (True) ->
                                                                                                                                                               (case (tyExpr2_4 _tyExpr2OpolGam _tyExpr2OpolVarMp ) of
                                                                                                                                                                { ( _tyExpr2ImbStrictness,_tyExpr2IpolVarMp,tyExpr2_5) | True ->
                                                                                                                                                                    (case (_tyExpr2IpolVarMp) of
                                                                                                                                                                     { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                                     (case ((let sem_PrExpr_Eq_5 :: T_PrExpr_5 
                                                                                                                                                                                 sem_PrExpr_Eq_5  =
                                                                                                                                                                                     (\ _lhsItyKiGam ->
                                                                                                                                                                                          _lhsItyKiGam `seq`
                                                                                                                                                                                          ((case (_lhsItyKiGam) of
                                                                                                                                                                                            { _tyExpr1OtyKiGam | _tyExpr1OtyKiGam `seq` (True) ->
                                                                                                                                                                                            (case (tyExpr1_5 _tyExpr1OtyKiGam ) of
                                                                                                                                                                                             { ( _tyExpr1IintlTyKiGam,_tyExpr1ItyKiGam,tyExpr1_6) | True ->
                                                                                                                                                                                                 (case (_tyExpr1ItyKiGam) of
                                                                                                                                                                                                  { _tyExpr2OtyKiGam | _tyExpr2OtyKiGam `seq` (True) ->
                                                                                                                                                                                                  (case (tyExpr2_5 _tyExpr2OtyKiGam ) of
                                                                                                                                                                                                   { ( _tyExpr2IintlTyKiGam,_tyExpr2ItyKiGam,tyExpr2_6) | True ->
                                                                                                                                                                                                       (case (_tyExpr1IintlTyKiGam `gamUnion` _tyExpr2IintlTyKiGam) of
                                                                                                                                                                                                        { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                                                        (case (_tyExpr2ItyKiGam) of
                                                                                                                                                                                                         { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                                         (case ((let sem_PrExpr_Eq_6 :: T_PrExpr_6 
                                                                                                                                                                                                                     sem_PrExpr_Eq_6  =
                                                                                                                                                                                                                         (\ _lhsIkiVarMp ->
                                                                                                                                                                                                                              _lhsIkiVarMp `seq`
                                                                                                                                                                                                                              ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                                                { _tyExpr1OkiVarMp | _tyExpr1OkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                (case (tyExpr1_6 _tyExpr1OkiVarMp ) of
                                                                                                                                                                                                                                 { ( _tyExpr1IgathTyVarPolGam,_tyExpr1Iki,_tyExpr1IkiVarMp,_tyExpr1Ipol,_tyExpr1ItyVarWildMp,tyExpr1_7) | True ->
                                                                                                                                                                                                                                     (case (_tyExpr1IkiVarMp) of
                                                                                                                                                                                                                                      { _tyExpr2OkiVarMp | _tyExpr2OkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                      (case (tyExpr2_6 _tyExpr2OkiVarMp ) of
                                                                                                                                                                                                                                       { ( _tyExpr2IgathTyVarPolGam,_tyExpr2Iki,_tyExpr2IkiVarMp,_tyExpr2Ipol,_tyExpr2ItyVarWildMp,tyExpr2_7) | True ->
                                                                                                                                                                                                                                           (case (_tyExpr1IgathTyVarPolGam `gamUnion` _tyExpr2IgathTyVarPolGam) of
                                                                                                                                                                                                                                            { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                                                            (case (_tyExpr2Iki) of
                                                                                                                                                                                                                                             { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                                                             (case (__tup197) of
                                                                                                                                                                                                                                              { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                              (case (fitsIn unifyFIOpts defaultFIEnv _lUniq _tyExpr2IkiVarMp _tyExpr1Iki _tyExpr2Iki) of
                                                                                                                                                                                                                                               { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                                                               (case (foVarMp _fo_ `varUpd` _tyExpr2IkiVarMp) of
                                                                                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                (case (_tyExpr1ItyVarWildMp `Map.union` _tyExpr2ItyVarWildMp) of
                                                                                                                                                                                                                                                 { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                                                 (case ((let sem_PrExpr_Eq_7 :: T_PrExpr_7 
                                                                                                                                                                                                                                                             sem_PrExpr_Eq_7  =
                                                                                                                                                                                                                                                                 (\ _lhsIclGam ->
                                                                                                                                                                                                                                                                      _lhsIclGam `seq`
                                                                                                                                                                                                                                                                      ((case (_lhsIclGam) of
                                                                                                                                                                                                                                                                        { _tyExpr2OclGam | _tyExpr2OclGam `seq` (True) ->
                                                                                                                                                                                                                                                                        (case (tyExpr2_7 _tyExpr2OclGam ) of
                                                                                                                                                                                                                                                                         { ( _tyExpr2IevTy,tyExpr2_8) | True ->
                                                                                                                                                                                                                                                                             (case (_tyExpr2IevTy) of
                                                                                                                                                                                                                                                                              { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                                                              (case ((let sem_PrExpr_Eq_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                                                                          sem_PrExpr_Eq_8  =
                                                                                                                                                                                                                                                                                              (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                                                                                 _lhsIfinTyKiGam
                                                                                                                                                                                                                                                                                                 _lhsIfinTyVarMp
                                                                                                                                                                                                                                                                                                 _lhsIkiGam
                                                                                                                                                                                                                                                                                                 _lhsImoduleNm
                                                                                                                                                                                                                                                                                                 _lhsIopts
                                                                                                                                                                                                                                                                                                 _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                                                                 _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                                                                 _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                                                                 _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                                                   _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                                                                   (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                                                                                    (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                                                     (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                                                                      (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                                                       (_lhsIopts `seq`
                                                                                                                                                                                                                                                                                                        (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                         (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                          (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                           (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                                            ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                              { _tyExpr2OvalTyGlobFreeTvarS | _tyExpr2OvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                               { _tyExpr2OtyTyTySigFreeTvarS | _tyExpr2OtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                { _tyExpr2OtyTyGlobFreeTvarS | _tyExpr2OtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                 { _tyExpr2OtyKiGlobFreeTvarS | _tyExpr2OtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                                  { _tyExpr2Oopts | _tyExpr2Oopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                   { _tyExpr2OmoduleNm | _tyExpr2OmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                    { _tyExpr2OkiGam | _tyExpr2OkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                     { _tyExpr2OfinTyVarMp | _tyExpr2OfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                      { _tyExpr2OfinTyKiGam | _tyExpr2OfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                      (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                       { _tyExpr2OfinKiVarMp | _tyExpr2OfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (tyExpr2_8 _tyExpr2OfinKiVarMp _tyExpr2OfinTyKiGam _tyExpr2OfinTyVarMp _tyExpr2OkiGam _tyExpr2OmoduleNm _tyExpr2Oopts _tyExpr2OtyKiGlobFreeTvarS _tyExpr2OtyTyGlobFreeTvarS _tyExpr2OtyTyTySigFreeTvarS _tyExpr2OvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                        { ( _tyExpr2IallErrSq,_tyExpr2IappArgPPL,_tyExpr2IappFunNm,_tyExpr2IappFunPP,_tyExpr2IclMissNmS,_tyExpr2IclNmS,_tyExpr2IerrSq,_tyExpr2IgathMentrelFilterMp,_tyExpr2Ipp,_tyExpr2ItyWildL) | True ->
                                                                                                                                                                                                                                                                                                                            (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                                                                             { _tyExpr1OclGam | _tyExpr1OclGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                             (case (tyExpr1_7 _tyExpr1OclGam ) of
                                                                                                                                                                                                                                                                                                                              { ( _tyExpr1IevTy,tyExpr1_8) | True ->
                                                                                                                                                                                                                                                                                                                                  (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                   { _tyExpr1OvalTyGlobFreeTvarS | _tyExpr1OvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                   (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                    { _tyExpr1OtyTyTySigFreeTvarS | _tyExpr1OtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                     { _tyExpr1OtyTyGlobFreeTvarS | _tyExpr1OtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                      { _tyExpr1OtyKiGlobFreeTvarS | _tyExpr1OtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                                                       { _tyExpr1Oopts | _tyExpr1Oopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                        { _tyExpr1OmoduleNm | _tyExpr1OmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                        (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                                         { _tyExpr1OkiGam | _tyExpr1OkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                         (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                          { _tyExpr1OfinTyVarMp | _tyExpr1OfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                          (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                                           { _tyExpr1OfinTyKiGam | _tyExpr1OfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                           (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                                            { _tyExpr1OfinKiVarMp | _tyExpr1OfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                            (case (tyExpr1_8 _tyExpr1OfinKiVarMp _tyExpr1OfinTyKiGam _tyExpr1OfinTyVarMp _tyExpr1OkiGam _tyExpr1OmoduleNm _tyExpr1Oopts _tyExpr1OtyKiGlobFreeTvarS _tyExpr1OtyTyGlobFreeTvarS _tyExpr1OtyTyTySigFreeTvarS _tyExpr1OvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                                             { ( _tyExpr1IallErrSq,_tyExpr1IappArgPPL,_tyExpr1IappFunNm,_tyExpr1IappFunPP,_tyExpr1IclMissNmS,_tyExpr1IclNmS,_tyExpr1IerrSq,_tyExpr1IgathMentrelFilterMp,_tyExpr1Ipp,_tyExpr1ItyWildL) | True ->
                                                                                                                                                                                                                                                                                                                                                 (case (_tyExpr1IallErrSq `Seq.union` _tyExpr2IallErrSq) of
                                                                                                                                                                                                                                                                                                                                                  { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                  (case (_tyExpr1IclMissNmS `Set.union` _tyExpr2IclMissNmS) of
                                                                                                                                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                   (case (_tyExpr1IclNmS `Set.union` _tyExpr2IclNmS) of
                                                                                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                    (case (_tyExpr1IerrSq `Seq.union` _tyExpr2IerrSq) of
                                                                                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                     (case (_tyExpr1IgathMentrelFilterMp `mentrelFilterMpUnion` _tyExpr2IgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                      (case (_tyExpr1Ipp >#< hsnEqTilde >#< _tyExpr2Ipp) of
                                                                                                                                                                                                                                                                                                                                                       { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                       (case (_pp) of
                                                                                                                                                                                                                                                                                                                                                        { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                        (case (_pr) of
                                                                                                                                                                                                                                                                                                                                                         { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                         ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                                                                      in  sem_PrExpr_Eq_8)) of
                                                                                                                                                                                                                                                                               { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                                                                               ( _lhsOevTy,sem_PrExpr_8) }) }) }) })))
                                                                                                                                                                                                                                                         in  sem_PrExpr_Eq_7)) of
                                                                                                                                                                                                                                                  { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                                                                  ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                                                 in  sem_PrExpr_Eq_6)) of
                                                                                                                                                                                                          { ( sem_PrExpr_6) | True ->
                                                                                                                                                                                                          ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) }) }) })))
                                                                                                                                                                             in  sem_PrExpr_Eq_5)) of
                                                                                                                                                                      { ( sem_PrExpr_5) | True ->
                                                                                                                                                                      ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                                 in  sem_PrExpr_Eq_4)) of
                                                                                                                          { ( sem_PrExpr_4) | True ->
                                                                                                                          ( _lhsOpolVarL,sem_PrExpr_4) }) }) }) })))
                                                                                                    in  sem_PrExpr_Eq_3)) of
                                                                                             { ( sem_PrExpr_3) | True ->
                                                                                             ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) }) }) })))
                                                            in  sem_PrExpr_Eq_2)) of
                                                     { ( sem_PrExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_PrExpr_2) }) }) }) }) }) }) })))
                        in  sem_PrExpr_Eq_1)) of
                 { ( sem_PrExpr_1) | True ->
                 ( _lhsOrange,sem_PrExpr_1) }) }) }) }) })

sem_PrExpr_Forall :: Range ->
                     HsName ->
                     T_PrExpr  ->
                     T_PrExpr 

sem_PrExpr_Forall hsrange_ tyVar_ prExpr_  | hsrange_ `seq` (tyVar_ `seq` (prExpr_ `seq` (True))) =
    (case (prExpr_ ) of
     { ( _prExprIrange,prExpr_1) | True ->
         (case (rangeUnions [hsrange_, _prExprIrange , _prExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_PrExpr_Forall_1 :: T_PrExpr_1 
                       sem_PrExpr_Forall_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_ki)}} )) of
                                  { __tup199 | __tup199 `seq` (True) ->
                                  (case (__tup199) of
                                   { (_prExprOgUniq,_,_) | _prExprOgUniq `seq` (True) ->
                                   (case (prExpr_1 _prExprOgUniq ) of
                                    { ( _prExprIgUniq,prExpr_2) | True ->
                                        (case (_prExprIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_PrExpr_Forall_2 :: T_PrExpr_2 
                                                     sem_PrExpr_Forall_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (__tup199) of
                                                                { (_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                (case (__tup199) of
                                                                 { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                                 (case (let  t = mkTyVar _lUniq
                                                                        in   (_lUniq,mkTGI t,TyKiGamInfo (mkNewTyVar _lUniq_ki))) of
                                                                  { __tup198 | __tup198 `seq` (True) ->
                                                                  (case (__tup198) of
                                                                   { (_,_tgi_,_) | _tgi_ `seq` (True) ->
                                                                   (case (tvGathFlowIn  (tyVar_ `gamSingleton` _tgi_) _lhsItyGam) of
                                                                    { _prExprOtyGam | _prExprOtyGam `seq` (True) ->
                                                                    (case (__tup198) of
                                                                     { (_tv,_,_) | _tv `seq` (True) ->
                                                                     (case (prExpr_2 _prExprOtyGam ) of
                                                                      { ( _prExprIprTy,_prExprIty,_prExprItyGam,prExpr_3) | True ->
                                                                          (case (mkTyQu tyQu_Forall [(_tv,kiStar)] _prExprIprTy) of
                                                                           { _prTy | _prTy `seq` (True) ->
                                                                           (case (_prTy) of
                                                                            { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                            (case (Pred_Pred _prTy) of
                                                                             { _pr | _pr `seq` (True) ->
                                                                             (case (mkTyPr _pr) of
                                                                              { _ty | _ty `seq` (True) ->
                                                                              (case (_ty) of
                                                                               { _lhsOty | _lhsOty `seq` (True) ->
                                                                               (case (tvGathFlowOut _lhsItyGam _prExprItyGam) of
                                                                                { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                (case ((let sem_PrExpr_Forall_3 :: T_PrExpr_3 
                                                                                            sem_PrExpr_Forall_3  =
                                                                                                (\ _lhsIknPolCtx ->
                                                                                                     _lhsIknPolCtx `seq`
                                                                                                     ((case (_lhsIknPolCtx) of
                                                                                                       { _prExprOknPolCtx | _prExprOknPolCtx `seq` (True) ->
                                                                                                       (case (prExpr_3 _prExprOknPolCtx ) of
                                                                                                        { ( _prExprIpolVarL,prExpr_4) | True ->
                                                                                                            (case (_prExprIpolVarL) of
                                                                                                             { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                             (case ((let sem_PrExpr_Forall_4 :: T_PrExpr_4 
                                                                                                                         sem_PrExpr_Forall_4  =
                                                                                                                             (\ _lhsIpolGam
                                                                                                                                _lhsIpolVarMp ->
                                                                                                                                  _lhsIpolGam `seq`
                                                                                                                                  (_lhsIpolVarMp `seq`
                                                                                                                                   ((case (_lhsIpolVarMp) of
                                                                                                                                     { _prExprOpolVarMp | _prExprOpolVarMp `seq` (True) ->
                                                                                                                                     (case (_lhsIpolGam) of
                                                                                                                                      { _prExprOpolGam | _prExprOpolGam `seq` (True) ->
                                                                                                                                      (case (prExpr_4 _prExprOpolGam _prExprOpolVarMp ) of
                                                                                                                                       { ( _prExprIpolVarMp,prExpr_5) | True ->
                                                                                                                                           (case (_prExprIpolVarMp) of
                                                                                                                                            { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                            (case ((let sem_PrExpr_Forall_5 :: T_PrExpr_5 
                                                                                                                                                        sem_PrExpr_Forall_5  =
                                                                                                                                                            (\ _lhsItyKiGam ->
                                                                                                                                                                 _lhsItyKiGam `seq`
                                                                                                                                                                 ((case (__tup198) of
                                                                                                                                                                   { (_,_,_tkgi_) | _tkgi_ `seq` (True) ->
                                                                                                                                                                   (case (tgiTy _tgi_ `tyKiGamSingleton` _tkgi_) of
                                                                                                                                                                    { _tyKiGamNew | _tyKiGamNew `seq` (True) ->
                                                                                                                                                                    (case (tvGathFlowIn  _tyKiGamNew _lhsItyKiGam) of
                                                                                                                                                                     { _prExprOtyKiGam | _prExprOtyKiGam `seq` (True) ->
                                                                                                                                                                     (case (prExpr_5 _prExprOtyKiGam ) of
                                                                                                                                                                      { ( _prExprIintlTyKiGam,_prExprItyKiGam,prExpr_6) | True ->
                                                                                                                                                                          (case (gamUnions [_tyKiGamNew,_prExprIintlTyKiGam]) of
                                                                                                                                                                           { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                           (case (tvGathFlowOut _lhsItyKiGam _prExprItyKiGam) of
                                                                                                                                                                            { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                            (case ((let sem_PrExpr_Forall_6 :: T_PrExpr_6 
                                                                                                                                                                                        sem_PrExpr_Forall_6  =
                                                                                                                                                                                            (\ _lhsIkiVarMp ->
                                                                                                                                                                                                 _lhsIkiVarMp `seq`
                                                                                                                                                                                                 ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                   { _prExprOkiVarMp | _prExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                                   (case (prExpr_6 _prExprOkiVarMp ) of
                                                                                                                                                                                                    { ( _prExprIgathTyVarPolGam,_prExprIki,_prExprIkiVarMp,_prExprItyVarWildMp,prExpr_7) | True ->
                                                                                                                                                                                                        (case (_prExprIgathTyVarPolGam) of
                                                                                                                                                                                                         { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                         (case (kiStar) of
                                                                                                                                                                                                          { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                          (case (_prExprIkiVarMp) of
                                                                                                                                                                                                           { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                           (case (_prExprItyVarWildMp) of
                                                                                                                                                                                                            { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                            (case ((let sem_PrExpr_Forall_7 :: T_PrExpr_7 
                                                                                                                                                                                                                        sem_PrExpr_Forall_7  =
                                                                                                                                                                                                                            (\ _lhsIclGam ->
                                                                                                                                                                                                                                 _lhsIclGam `seq`
                                                                                                                                                                                                                                 ((case (_lhsIclGam) of
                                                                                                                                                                                                                                   { _prExprOclGam | _prExprOclGam `seq` (True) ->
                                                                                                                                                                                                                                   (case (prExpr_7 _prExprOclGam ) of
                                                                                                                                                                                                                                    { ( _prExprIevTy,prExpr_8) | True ->
                                                                                                                                                                                                                                        (case (_prExprIevTy) of
                                                                                                                                                                                                                                         { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                         (case ((let sem_PrExpr_Forall_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                                     sem_PrExpr_Forall_8  =
                                                                                                                                                                                                                                                         (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                                            _lhsIfinTyKiGam
                                                                                                                                                                                                                                                            _lhsIfinTyVarMp
                                                                                                                                                                                                                                                            _lhsIkiGam
                                                                                                                                                                                                                                                            _lhsImoduleNm
                                                                                                                                                                                                                                                            _lhsIopts
                                                                                                                                                                                                                                                            _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                            _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                            _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                            _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                              _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                              (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                                               (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                                 (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                  (_lhsIopts `seq`
                                                                                                                                                                                                                                                                   (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                    (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                     (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                                      (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                       ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                         { _prExprOvalTyGlobFreeTvarS | _prExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                         (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                          { _prExprOtyTyTySigFreeTvarS | _prExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                          (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                           { _prExprOtyTyGlobFreeTvarS | _prExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                           (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                            { _prExprOtyKiGlobFreeTvarS | _prExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_lhsIopts) of
                                                                                                                                                                                                                                                                             { _prExprOopts | _prExprOopts `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                              { _prExprOmoduleNm | _prExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                               { _prExprOkiGam | _prExprOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                { _prExprOfinTyVarMp | _prExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                 { _prExprOfinTyKiGam | _prExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                  { _prExprOfinKiVarMp | _prExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (prExpr_8 _prExprOfinKiVarMp _prExprOfinTyKiGam _prExprOfinTyVarMp _prExprOkiGam _prExprOmoduleNm _prExprOopts _prExprOtyKiGlobFreeTvarS _prExprOtyTyGlobFreeTvarS _prExprOtyTyTySigFreeTvarS _prExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                   { ( _prExprIallErrSq,_prExprIclMissNmS,_prExprIclNmS,_prExprIerrSq,_prExprIgathMentrelFilterMp,_prExprIpp,_prExprIpr) | True ->
                                                                                                                                                                                                                                                                                       (case (_prExprIallErrSq) of
                                                                                                                                                                                                                                                                                        { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_prExprIclMissNmS) of
                                                                                                                                                                                                                                                                                         { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_prExprIclNmS) of
                                                                                                                                                                                                                                                                                          { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_prExprIerrSq) of
                                                                                                                                                                                                                                                                                           { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_prExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                            { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case ("forall" >#< tyVar_ >#< "." >#< _prExprIpp) of
                                                                                                                                                                                                                                                                                             { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_pp) of
                                                                                                                                                                                                                                                                                              { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_pr) of
                                                                                                                                                                                                                                                                                               { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                                               ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                                 in  sem_PrExpr_Forall_8)) of
                                                                                                                                                                                                                                          { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                                          ( _lhsOevTy,sem_PrExpr_8) }) }) }) })))
                                                                                                                                                                                                                    in  sem_PrExpr_Forall_7)) of
                                                                                                                                                                                                             { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                             ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) })))
                                                                                                                                                                                    in  sem_PrExpr_Forall_6)) of
                                                                                                                                                                             { ( sem_PrExpr_6) | True ->
                                                                                                                                                                             ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) }) }) })))
                                                                                                                                                    in  sem_PrExpr_Forall_5)) of
                                                                                                                                             { ( sem_PrExpr_5) | True ->
                                                                                                                                             ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }))))
                                                                                                                     in  sem_PrExpr_Forall_4)) of
                                                                                                              { ( sem_PrExpr_4) | True ->
                                                                                                              ( _lhsOpolVarL,sem_PrExpr_4) }) }) }) })))
                                                                                        in  sem_PrExpr_Forall_3)) of
                                                                                 { ( sem_PrExpr_3) | True ->
                                                                                 ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                                 in  sem_PrExpr_Forall_2)) of
                                          { ( sem_PrExpr_2) | True ->
                                          ( _lhsOgUniq,sem_PrExpr_2) }) }) }) }) })))
                   in  sem_PrExpr_Forall_1)) of
            { ( sem_PrExpr_1) | True ->
            ( _lhsOrange,sem_PrExpr_1) }) }) }) })

sem_PrExpr_Lacks :: Range ->
                    T_RowTyExpr  ->
                    HsName ->
                    T_PrExpr 

sem_PrExpr_Lacks hsrange_ rowTyExpr_ nm_  | hsrange_ `seq` (rowTyExpr_ `seq` (nm_ `seq` (True))) =
    (case (rowTyExpr_ ) of
     { ( _rowTyExprIrange,rowTyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _rowTyExprIrange
                                                     , _rowTyExprIrange
                                                                    ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_PrExpr_Lacks_1 :: T_PrExpr_1 
                       sem_PrExpr_Lacks_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _rowTyExprOgUniq | _rowTyExprOgUniq `seq` (True) ->
                                  (case (positionalFldNames) of
                                   { _rowTyExprOpositionalFldNmL | _rowTyExprOpositionalFldNmL `seq` (True) ->
                                   (case (rowTyExpr_1 _rowTyExprOgUniq _rowTyExprOpositionalFldNmL ) of
                                    { ( _rowTyExprIgUniq,_rowTyExprIpositionalFldNmL,rowTyExpr_2) | True ->
                                        (case (_rowTyExprIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_PrExpr_Lacks_2 :: T_PrExpr_2 
                                                     sem_PrExpr_Lacks_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (_lhsItyGam) of
                                                                { _rowTyExprOtyGam | _rowTyExprOtyGam `seq` (True) ->
                                                                (case (rowTyExpr_2 _rowTyExprOtyGam ) of
                                                                 { ( _rowTyExprItyGam,_rowTyExprItyRow,rowTyExpr_3) | True ->
                                                                     (case (Pred_Lacks _rowTyExprItyRow (Label_Lab nm_)) of
                                                                      { _pr | _pr `seq` (True) ->
                                                                      (case (mkTyPr _pr) of
                                                                       { _ty | _ty `seq` (True) ->
                                                                       (case (_ty) of
                                                                        { _prTy | _prTy `seq` (True) ->
                                                                        (case (_prTy) of
                                                                         { _lhsOprTy | _lhsOprTy `seq` (True) ->
                                                                         (case (_ty) of
                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                          (case (_rowTyExprItyGam) of
                                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                           (case ((let sem_PrExpr_Lacks_3 :: T_PrExpr_3 
                                                                                       sem_PrExpr_Lacks_3  =
                                                                                           (\ _lhsIknPolCtx ->
                                                                                                _lhsIknPolCtx `seq`
                                                                                                ((case ([]) of
                                                                                                  { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                  (case ((let sem_PrExpr_Lacks_4 :: T_PrExpr_4 
                                                                                                              sem_PrExpr_Lacks_4  =
                                                                                                                  (\ _lhsIpolGam
                                                                                                                     _lhsIpolVarMp ->
                                                                                                                       _lhsIpolGam `seq`
                                                                                                                       (_lhsIpolVarMp `seq`
                                                                                                                        ((case (_lhsIpolVarMp) of
                                                                                                                          { _rowTyExprOpolVarMp | _rowTyExprOpolVarMp `seq` (True) ->
                                                                                                                          (case (_lhsIpolGam) of
                                                                                                                           { _rowTyExprOpolGam | _rowTyExprOpolGam `seq` (True) ->
                                                                                                                           (case (_lhsIknPolCtx) of
                                                                                                                            { _rowTyExprOknPolCtx | _rowTyExprOknPolCtx `seq` (True) ->
                                                                                                                            (case (rowTyExpr_3 _rowTyExprOknPolCtx _rowTyExprOpolGam _rowTyExprOpolVarMp ) of
                                                                                                                             { ( _rowTyExprIpolVarMp,rowTyExpr_4) | True ->
                                                                                                                                 (case (_rowTyExprIpolVarMp) of
                                                                                                                                  { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                  (case ((let sem_PrExpr_Lacks_5 :: T_PrExpr_5 
                                                                                                                                              sem_PrExpr_Lacks_5  =
                                                                                                                                                  (\ _lhsItyKiGam ->
                                                                                                                                                       _lhsItyKiGam `seq`
                                                                                                                                                       ((case (_lhsItyKiGam) of
                                                                                                                                                         { _rowTyExprOtyKiGam | _rowTyExprOtyKiGam `seq` (True) ->
                                                                                                                                                         (case (rowTyExpr_4 _rowTyExprOtyKiGam ) of
                                                                                                                                                          { ( _rowTyExprIintlTyKiGam,_rowTyExprItyKiGam,rowTyExpr_5) | True ->
                                                                                                                                                              (case (_rowTyExprIintlTyKiGam) of
                                                                                                                                                               { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                               (case (_rowTyExprItyKiGam) of
                                                                                                                                                                { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                (case ((let sem_PrExpr_Lacks_6 :: T_PrExpr_6 
                                                                                                                                                                            sem_PrExpr_Lacks_6  =
                                                                                                                                                                                (\ _lhsIkiVarMp ->
                                                                                                                                                                                     _lhsIkiVarMp `seq`
                                                                                                                                                                                     ((case (emptyGam) of
                                                                                                                                                                                       { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                       (case (kiStar) of
                                                                                                                                                                                        { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                        (case (_lhsIkiVarMp) of
                                                                                                                                                                                         { _rowTyExprOkiVarMp | _rowTyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                         (case (rowTyExpr_5 _rowTyExprOkiVarMp ) of
                                                                                                                                                                                          { ( _rowTyExprIkiVarMp,_rowTyExprItyVarWildMp,rowTyExpr_6) | True ->
                                                                                                                                                                                              (case (_rowTyExprIkiVarMp) of
                                                                                                                                                                                               { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                               (case (_rowTyExprItyVarWildMp) of
                                                                                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                (case ((let sem_PrExpr_Lacks_7 :: T_PrExpr_7 
                                                                                                                                                                                                            sem_PrExpr_Lacks_7  =
                                                                                                                                                                                                                (\ _lhsIclGam ->
                                                                                                                                                                                                                     _lhsIclGam `seq`
                                                                                                                                                                                                                     ((case (tyInt) of
                                                                                                                                                                                                                       { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                       (case ((let sem_PrExpr_Lacks_8 :: T_PrExpr_8 
                                                                                                                                                                                                                                   sem_PrExpr_Lacks_8  =
                                                                                                                                                                                                                                       (\ _lhsIfinKiVarMp
                                                                                                                                                                                                                                          _lhsIfinTyKiGam
                                                                                                                                                                                                                                          _lhsIfinTyVarMp
                                                                                                                                                                                                                                          _lhsIkiGam
                                                                                                                                                                                                                                          _lhsImoduleNm
                                                                                                                                                                                                                                          _lhsIopts
                                                                                                                                                                                                                                          _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                          _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                          _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                            _lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                            (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                             (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                              (_lhsIkiGam `seq`
                                                                                                                                                                                                                                               (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                (_lhsIopts `seq`
                                                                                                                                                                                                                                                 (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                  (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                   (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                    (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                     ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                       { _rowTyExprOvalTyGlobFreeTvarS | _rowTyExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                       (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                        { _rowTyExprOtyTyTySigFreeTvarS | _rowTyExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                        (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                         { _rowTyExprOtyTyGlobFreeTvarS | _rowTyExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                         (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                          { _rowTyExprOtyKiGlobFreeTvarS | _rowTyExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                          (case (_lhsIopts) of
                                                                                                                                                                                                                                                           { _rowTyExprOopts | _rowTyExprOopts `seq` (True) ->
                                                                                                                                                                                                                                                           (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                            { _rowTyExprOmoduleNm | _rowTyExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                            (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                             { _rowTyExprOkiGam | _rowTyExprOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                             (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                              { _rowTyExprOfinTyVarMp | _rowTyExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                              (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                               { _rowTyExprOfinTyKiGam | _rowTyExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                               (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                { _rowTyExprOfinKiVarMp | _rowTyExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                 { _rowTyExprOclGam | _rowTyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                 (case (rowTyExpr_6 _rowTyExprOclGam _rowTyExprOfinKiVarMp _rowTyExprOfinTyKiGam _rowTyExprOfinTyVarMp _rowTyExprOkiGam _rowTyExprOmoduleNm _rowTyExprOopts _rowTyExprOtyKiGlobFreeTvarS _rowTyExprOtyTyGlobFreeTvarS _rowTyExprOtyTyTySigFreeTvarS _rowTyExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                  { ( _rowTyExprIallErrSq,_rowTyExprIclMissNmS,_rowTyExprIclNmS,_rowTyExprIerrSq,_rowTyExprIextNm,_rowTyExprIgathMentrelFilterMp,_rowTyExprIpp,_rowTyExprIppL) | True ->
                                                                                                                                                                                                                                                                      (case (_rowTyExprIallErrSq) of
                                                                                                                                                                                                                                                                       { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                       (case (_rowTyExprIclMissNmS) of
                                                                                                                                                                                                                                                                        { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                        (case (_rowTyExprIclNmS) of
                                                                                                                                                                                                                                                                         { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                         (case (_rowTyExprIerrSq) of
                                                                                                                                                                                                                                                                          { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                          (case (_rowTyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                           { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                           (case (_rowTyExprIpp >|< "\\" >|< pp nm_) of
                                                                                                                                                                                                                                                                            { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_pp) of
                                                                                                                                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_pr) of
                                                                                                                                                                                                                                                                              { _lhsOpr | _lhsOpr `seq` (True) ->
                                                                                                                                                                                                                                                                              ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOpr) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                               in  sem_PrExpr_Lacks_8)) of
                                                                                                                                                                                                                        { ( sem_PrExpr_8) | True ->
                                                                                                                                                                                                                        ( _lhsOevTy,sem_PrExpr_8) }) })))
                                                                                                                                                                                                        in  sem_PrExpr_Lacks_7)) of
                                                                                                                                                                                                 { ( sem_PrExpr_7) | True ->
                                                                                                                                                                                                 ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_PrExpr_7) }) }) }) }) }) }) })))
                                                                                                                                                                        in  sem_PrExpr_Lacks_6)) of
                                                                                                                                                                 { ( sem_PrExpr_6) | True ->
                                                                                                                                                                 ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_PrExpr_6) }) }) }) }) })))
                                                                                                                                          in  sem_PrExpr_Lacks_5)) of
                                                                                                                                   { ( sem_PrExpr_5) | True ->
                                                                                                                                   ( _lhsOpolVarMp,sem_PrExpr_5) }) }) }) }) }) }))))
                                                                                                          in  sem_PrExpr_Lacks_4)) of
                                                                                                   { ( sem_PrExpr_4) | True ->
                                                                                                   ( _lhsOpolVarL,sem_PrExpr_4) }) })))
                                                                                   in  sem_PrExpr_Lacks_3)) of
                                                                            { ( sem_PrExpr_3) | True ->
                                                                            ( _lhsOprTy,_lhsOty,_lhsOtyGam,sem_PrExpr_3) }) }) }) }) }) }) }) }) })))
                                                 in  sem_PrExpr_Lacks_2)) of
                                          { ( sem_PrExpr_2) | True ->
                                          ( _lhsOgUniq,sem_PrExpr_2) }) }) }) }) })))
                   in  sem_PrExpr_Lacks_1)) of
            { ( sem_PrExpr_1) | True ->
            ( _lhsOrange,sem_PrExpr_1) }) }) }) })


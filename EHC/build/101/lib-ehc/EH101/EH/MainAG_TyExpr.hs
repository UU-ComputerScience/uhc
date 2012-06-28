


module EH101.EH.MainAG_TyExpr where

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

-- TyExpr ------------------------------------------------------
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
      synthesized attribute:
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
      synthesized attribute:
         mbStrictness         : Maybe Strictness
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
         pol                  : Polarity
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
         appArgPPL            : [PP_Doc]
         appFunNm             : HsName
         appFunPP             : PP_Doc
         clMissNmS            : HsNameS
         clNmS                : HsNameS
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         pp                   : PP_Doc
         tyWildL              : TyL
   alternatives:
      alternative Ann:
         child hsrange        : {Range}
         child ann            : TyExprAnn 
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 2:
            local ty          : {Ty}
         visit 8:
            local pp          : _
      alternative App:
         child hsrange        : {Range}
         child func           : TyExpr 
         child arg            : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            local ty          : {Ty}
            intra _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 4:
            local lUniq_17_polArg : {UID}
            local polArgVar   : {Ty}
            local knFuncPol   : _
            intra _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 5:
            intra _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 6:
            local lUniq       : {UID}
            local knResKi     : _
            local ki          : {Ty}
            local lUniq2      : {UID}
            local knFunKi     : {Ty}
            local fo_         : {FIOut}
            intra _tup218     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 7:
            local evTy        : _
            intra fo_         : {FIOut}
            intra range       : {Range}
         visit 8:
            local _tup217     : {(PP_Doc,[PP_Doc])}
            local pp          : _
            intra fo_         : {FIOut}
            intra range       : {Range}
      alternative AppTop:
         child hsrange        : {Range}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            intra range       : {Range}
         visit 2:
            intra range       : {Range}
         visit 3:
            intra range       : {Range}
         visit 4:
            intra range       : {Range}
         visit 5:
            intra range       : {Range}
         visit 6:
            intra range       : {Range}
         visit 7:
            intra range       : {Range}
         visit 8:
            local pp          : _
            intra range       : {Range}
      alternative Con:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup223     : {(UID,UID)}
            intra range       : {Range}
         visit 2:
            local ty          : {Ty}
            intra _tup223     : {(UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup223     : {(UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 4:
            local lUniq_17_fitsIn_pol : {UID}
            local _tup222     : {(PolGamInfo,ErrL)}
            local pgi_        : {PolGamInfo}
            local polFromEnv  : _
            local foPol       : {FIOut}
            intra _tup223     : {(UID,UID)}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 5:
            intra foPol       : {FIOut}
            intra ty          : {Ty}
            intra _tup222     : {(PolGamInfo,ErrL)}
            intra range       : {Range}
         visit 6:
            local _tup221     : {(TyKiGamInfo,ErrL)}
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra foPol       : {FIOut}
            intra ty          : {Ty}
            intra _tup222     : {(PolGamInfo,ErrL)}
            intra range       : {Range}
         visit 7:
            intra ty          : {Ty}
            intra _tup222     : {(PolGamInfo,ErrL)}
            intra _tup221     : {(TyKiGamInfo,ErrL)}
            intra foPol       : {FIOut}
            intra range       : {Range}
         visit 8:
            local pp          : _
            local nmPolErrs   : {ErrL}
            local nmErrs2     : {ErrL}
            local _tup220     : {(TyGamInfo,ErrL)}
            local nmErrs      : {ErrL}
            local gathMentrelFilterMp : _
            intra _tup222     : {(PolGamInfo,ErrL)}
            intra _tup221     : {(TyKiGamInfo,ErrL)}
            intra foPol       : {FIOut}
            intra range       : {Range}
      alternative Impls:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup224     : {(UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local implsVarId  : {UID}
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
            intra _tup224     : {(UID,UID)}
         visit 3:
            intra implsVarId  : {UID}
            intra ty          : {Ty}
         visit 4:
            intra implsVarId  : {UID}
            intra ty          : {Ty}
         visit 5:
            intra implsVarId  : {UID}
            intra ty          : {Ty}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra implsVarId  : {UID}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Lam:
         child hsrange        : {Range}
         child tyVar          : {HsName}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup226     : {(UID,UID,UID,UID,UID,UID)}
         visit 2:
            local lUniq_ki    : {UID}
            local lUniq       : {UID}
            local _tup225     : {(UID,TyGamInfo,TyKiGamInfo)}
            local tgi_        : {TyGamInfo}
            local tv          : {UID}
            local ty          : {Ty}
            intra _tup226     : {(UID,UID,UID,UID,UID,UID)}
         visit 3:
            intra _tup226     : {(UID,UID,UID,UID,UID,UID)}
            intra _tup225     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
         visit 4:
            local lUniq_17_polRes : {UID}
            local lUniq_17_polArg : {UID}
            local lUniq_17_fitsIn_pol : {UID}
            local polResVar   : {Ty}
            local polArgVar   : {Ty}
            local foPol       : {FIOut}
            intra _tup226     : {(UID,UID,UID,UID,UID,UID)}
            intra _tup225     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
         visit 5:
            local tkgi_       : {TyKiGamInfo}
            local tyKiGamNew  : _
            intra _tup225     : {(UID,TyGamInfo,TyKiGamInfo)}
            intra tgi_        : {TyGamInfo}
            intra polArgVar   : {Ty}
            intra ty          : {Ty}
         visit 6:
            local ki          : {Ty}
            intra tkgi_       : {TyKiGamInfo}
            intra polArgVar   : {Ty}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Mono:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup227     : {(UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local tyVarId     : {UID}
            local tvarv       : {Ty}
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
            intra _tup227     : {(UID,UID)}
         visit 3:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 4:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 5:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative NoImpls:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 2:
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
         visit 3:
            intra ty          : {Ty}
         visit 4:
            intra ty          : {Ty}
         visit 5:
            intra ty          : {Ty}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Parens:
         child hsrange        : {Range}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 8:
            local pp          : _
      alternative Pred:
         child hsrange        : {Range}
         child prExpr         : PrExpr 
         visit 0:
            local range       : {Range}
         visit 8:
            local pp          : _
      alternative Quant:
         child hsrange        : {Range}
         child qu             : {TyQu}
         child tyVar          : {HsName}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup230     : {(UID,UID,UID)}
         visit 2:
            local lUniq_ki    : {UID}
            local lUniq       : {UID}
            local _tup229     : {(UID,TyGamInfo,TyKiGamInfo)}
            local tkgi_       : {TyKiGamInfo}
            local tgi_        : {TyGamInfo}
            local tv          : {UID}
            local ty          : {Ty}
            intra _tup230     : {(UID,UID,UID)}
         visit 3:
            intra tkgi_       : {TyKiGamInfo}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
         visit 4:
            intra tkgi_       : {TyKiGamInfo}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
         visit 5:
            local tyKiGamNew  : _
            intra tkgi_       : {TyKiGamInfo}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
         visit 6:
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Row:
         child hsrange        : {Range}
         child rowTyExpr      : RowTyExpr 
         visit 0:
            local range       : {Range}
         visit 2:
            local ty          : {Ty}
         visit 3:
            intra ty          : {Ty}
         visit 4:
            intra ty          : {Ty}
         visit 5:
            intra ty          : {Ty}
         visit 6:
            local ki          : {Ty}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Var:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup235     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            local lUniq       : {UID}
            local _tup232     : {(TyGamInfo,TyGam)}
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
            intra _tup235     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup235     : {(UID,UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 4:
            local lUniq_17_fitsIn_pol : {UID}
            local _tup234     : {(PolGamInfo,ErrL)}
            local pgi_        : {PolGamInfo}
            local polFromEnv  : _
            local foPol       : {FIOut}
            intra _tup235     : {(UID,UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 5:
            local lUniq_ki    : {UID}
            local _tup233     : _
            local tyKiGamNew  : _
            intra _tup235     : {(UID,UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra foPol       : {FIOut}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra _tup233     : _
            intra foPol       : {FIOut}
            intra ty          : {Ty}
            intra range       : {Range}
         visit 7:
            intra ty          : {Ty}
            intra foPol       : {FIOut}
            intra range       : {Range}
         visit 8:
            local pp          : _
            intra foPol       : {FIOut}
            intra range       : {Range}
      alternative VarWild:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup239     : {(UID,UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local _tup237     : {(TyGamInfo,TyGam)}
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
            intra _tup239     : {(UID,UID,UID)}
         visit 3:
            intra _tup239     : {(UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra lUniq       : {UID}
            intra ty          : {Ty}
         visit 4:
            intra _tup239     : {(UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra lUniq       : {UID}
            intra ty          : {Ty}
         visit 5:
            local lUniq_ki    : {UID}
            local _tup238     : _
            local tyKiGamNew  : _
            intra _tup239     : {(UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra lUniq       : {UID}
            intra ty          : {Ty}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            local tyVarId     : {UID}
            intra _tup238     : _
            intra lUniq       : {UID}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
      alternative Wild:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup240     : {(UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local tyVarId     : {UID}
            local tvarv       : {Ty}
            local tgi_        : {TyGamInfo}
            local ty          : {Ty}
            intra _tup240     : {(UID,UID)}
         visit 3:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 4:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 5:
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 6:
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra tvarv       : {Ty}
            intra tyVarId     : {UID}
            intra ty          : {Ty}
         visit 7:
            intra ty          : {Ty}
         visit 8:
            local pp          : _
-}
sem_TyExpr_Ann :: Range ->
                  T_TyExprAnn  ->
                  T_TyExpr  ->
                  T_TyExpr 

sem_TyExpr_Ann hsrange_ ann_ tyExpr_  | hsrange_ `seq` (ann_ `seq` (tyExpr_ `seq` (True))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Ann_1 :: T_TyExpr_1 
                       sem_TyExpr_Ann_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                  (case (tyExpr_1 _tyExprOgUniq ) of
                                   { ( _tyExprIgUniq,tyExpr_2) | True ->
                                       (case (_tyExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_TyExpr_Ann_2 :: T_TyExpr_2 
                                                    sem_TyExpr_Ann_2  =
                                                        (\ _lhsItyGam ->
                                                             _lhsItyGam `seq`
                                                             ((case (_lhsItyGam) of
                                                               { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                               (case (tyExpr_2 _tyExprOtyGam ) of
                                                                { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                    (case (ann_ ) of
                                                                     { ( _annIann,_annIisEmpty,_annImbStrictness,_annIpp) | True ->
                                                                         (case (if _annIisEmpty
                                                                                then _tyExprIty
                                                                                else Ty_Ann _annIann _tyExprIty) of
                                                                          { _ty | _ty `seq` (True) ->
                                                                          (case (_ty) of
                                                                           { _lhsOty | _lhsOty `seq` (True) ->
                                                                           (case (_tyExprItyGam) of
                                                                            { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                            (case ((let sem_TyExpr_Ann_3 :: T_TyExpr_3 
                                                                                        sem_TyExpr_Ann_3  =
                                                                                            (\ _lhsIknPolCtx ->
                                                                                                 _lhsIknPolCtx `seq`
                                                                                                 ((case ([]) of
                                                                                                   { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                   (case ((let sem_TyExpr_Ann_4 :: T_TyExpr_4 
                                                                                                               sem_TyExpr_Ann_4  =
                                                                                                                   (\ _lhsIpolGam
                                                                                                                      _lhsIpolVarMp ->
                                                                                                                        _lhsIpolGam `seq`
                                                                                                                        (_lhsIpolVarMp `seq`
                                                                                                                         ((case (_lhsIknPolCtx) of
                                                                                                                           { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                                           (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                                            { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                                                (case (_lhsIpolVarMp) of
                                                                                                                                 { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                                 (case (_lhsIpolGam) of
                                                                                                                                  { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                                  (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                                   { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                       (case (_annImbStrictness <|> _tyExprImbStrictness) of
                                                                                                                                        { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                                        (case (_tyExprIpolVarMp) of
                                                                                                                                         { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                         (case ((let sem_TyExpr_Ann_5 :: T_TyExpr_5 
                                                                                                                                                     sem_TyExpr_Ann_5  =
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
                                                                                                                                                                       (case ((let sem_TyExpr_Ann_6 :: T_TyExpr_6 
                                                                                                                                                                                   sem_TyExpr_Ann_6  =
                                                                                                                                                                                       (\ _lhsIkiVarMp ->
                                                                                                                                                                                            _lhsIkiVarMp `seq`
                                                                                                                                                                                            ((case (_lhsIkiVarMp) of
                                                                                                                                                                                              { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                              (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                               { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                                   (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                                    { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                    (case (_tyExprIki) of
                                                                                                                                                                                                     { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                     (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                      { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                      (case (_tyExprIpol) of
                                                                                                                                                                                                       { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                       (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                        { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                        (case ((let sem_TyExpr_Ann_7 :: T_TyExpr_7 
                                                                                                                                                                                                                    sem_TyExpr_Ann_7  =
                                                                                                                                                                                                                        (\ _lhsIclGam ->
                                                                                                                                                                                                                             _lhsIclGam `seq`
                                                                                                                                                                                                                             ((case (_lhsIclGam) of
                                                                                                                                                                                                                               { _tyExprOclGam | _tyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                               (case (tyExpr_7 _tyExprOclGam ) of
                                                                                                                                                                                                                                { ( _tyExprIevTy,tyExpr_8) | True ->
                                                                                                                                                                                                                                    (case (_tyExprIevTy) of
                                                                                                                                                                                                                                     { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                     (case ((let sem_TyExpr_Ann_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                                 sem_TyExpr_Ann_8  =
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
                                                                                                                                                                                                                                                                                    (case ([]) of
                                                                                                                                                                                                                                                                                     { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                      { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_annIpp >#< _tyExprIpp) of
                                                                                                                                                                                                                                                                                       { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_pp) of
                                                                                                                                                                                                                                                                                        { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                         { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                          { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_tyExprIerrSq) of
                                                                                                                                                                                                                                                                                           { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                            { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_pp) of
                                                                                                                                                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_tyExprItyWildL) of
                                                                                                                                                                                                                                                                                              { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                              ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                             in  sem_TyExpr_Ann_8)) of
                                                                                                                                                                                                                                      { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                      ( _lhsOevTy,sem_TyExpr_8) }) }) }) })))
                                                                                                                                                                                                                in  sem_TyExpr_Ann_7)) of
                                                                                                                                                                                                         { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                         ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                                               in  sem_TyExpr_Ann_6)) of
                                                                                                                                                                        { ( sem_TyExpr_6) | True ->
                                                                                                                                                                        ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) })))
                                                                                                                                                 in  sem_TyExpr_Ann_5)) of
                                                                                                                                          { ( sem_TyExpr_5) | True ->
                                                                                                                                          ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                                           in  sem_TyExpr_Ann_4)) of
                                                                                                    { ( sem_TyExpr_4) | True ->
                                                                                                    ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                                    in  sem_TyExpr_Ann_3)) of
                                                                             { ( sem_TyExpr_3) | True ->
                                                                             ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) })))
                                                in  sem_TyExpr_Ann_2)) of
                                         { ( sem_TyExpr_2) | True ->
                                         ( _lhsOgUniq,sem_TyExpr_2) }) }) }) })))
                   in  sem_TyExpr_Ann_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_App :: Range ->
                  T_TyExpr  ->
                  T_TyExpr  ->
                  T_TyExpr 

sem_TyExpr_App hsrange_ func_ arg_  | hsrange_ `seq` (func_ `seq` (arg_ `seq` (True))) =
    (case (arg_ ) of
     { ( _argIrange,arg_1) | True ->
         (case (func_ ) of
          { ( _funcIrange,func_1) | True ->
              (case (rangeUnions [hsrange_, _funcIrange   , _argIrange   ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_TyExpr_App_1 :: T_TyExpr_1 
                            sem_TyExpr_App_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq_17_polArg) -> (__cont, lUniq,lUniq2,lUniq_17_polArg)}}} )) of
                                       { __tup218 | __tup218 `seq` (True) ->
                                       (case (__tup218) of
                                        { (_funcOgUniq,_,_,_) | _funcOgUniq `seq` (True) ->
                                        (case (func_1 _funcOgUniq ) of
                                         { ( _funcIgUniq,func_2) | True ->
                                             (case (_funcIgUniq) of
                                              { _argOgUniq | _argOgUniq `seq` (True) ->
                                              (case (arg_1 _argOgUniq ) of
                                               { ( _argIgUniq,arg_2) | True ->
                                                   (case (_argIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_TyExpr_App_2 :: T_TyExpr_2 
                                                                sem_TyExpr_App_2  =
                                                                    (\ _lhsItyGam ->
                                                                         _lhsItyGam `seq`
                                                                         ((case (_lhsItyGam) of
                                                                           { _funcOtyGam | _funcOtyGam `seq` (True) ->
                                                                           (case (func_2 _funcOtyGam ) of
                                                                            { ( _funcIty,_funcItyGam,func_3) | True ->
                                                                                (case (_funcItyGam) of
                                                                                 { _argOtyGam | _argOtyGam `seq` (True) ->
                                                                                 (case (arg_2 _argOtyGam ) of
                                                                                  { ( _argIty,_argItyGam,arg_3) | True ->
                                                                                      (case (Ty_App _funcIty _argIty) of
                                                                                       { _ty | _ty `seq` (True) ->
                                                                                       (case (_ty) of
                                                                                        { _lhsOty | _lhsOty `seq` (True) ->
                                                                                        (case (_argItyGam) of
                                                                                         { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                         (case ((let sem_TyExpr_App_3 :: T_TyExpr_3 
                                                                                                     sem_TyExpr_App_3  =
                                                                                                         (\ _lhsIknPolCtx ->
                                                                                                              _lhsIknPolCtx `seq`
                                                                                                              ((case ([]) of
                                                                                                                { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                                (case ((let sem_TyExpr_App_4 :: T_TyExpr_4 
                                                                                                                            sem_TyExpr_App_4  =
                                                                                                                                (\ _lhsIpolGam
                                                                                                                                   _lhsIpolVarMp ->
                                                                                                                                     _lhsIpolGam `seq`
                                                                                                                                     (_lhsIpolVarMp `seq`
                                                                                                                                      ((case (Nothing) of
                                                                                                                                        { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                                        (case (_lhsIpolVarMp) of
                                                                                                                                         { _funcOpolVarMp | _funcOpolVarMp `seq` (True) ->
                                                                                                                                         (case (_lhsIpolGam) of
                                                                                                                                          { _funcOpolGam | _funcOpolGam `seq` (True) ->
                                                                                                                                          (case (__tup218) of
                                                                                                                                           { (_,_,_,_lUniq_17_polArg) | _lUniq_17_polArg `seq` (True) ->
                                                                                                                                           (case (mkPolVar _lUniq_17_polArg) of
                                                                                                                                            { _polArgVar | _polArgVar `seq` (True) ->
                                                                                                                                            (case ([_polArgVar] `mkArrow` _lhsIknPolCtx) of
                                                                                                                                             { _knFuncPol | _knFuncPol `seq` (True) ->
                                                                                                                                             (case (_knFuncPol) of
                                                                                                                                              { _funcOknPolCtx | _funcOknPolCtx `seq` (True) ->
                                                                                                                                              (case (func_3 _funcOknPolCtx ) of
                                                                                                                                               { ( _funcIpolVarL,func_4) | True ->
                                                                                                                                                   (case (func_4 _funcOpolGam _funcOpolVarMp ) of
                                                                                                                                                    { ( _funcImbStrictness,_funcIpolVarMp,func_5) | True ->
                                                                                                                                                        (case (_funcIpolVarMp) of
                                                                                                                                                         { _argOpolVarMp | _argOpolVarMp `seq` (True) ->
                                                                                                                                                         (case (_lhsIpolGam) of
                                                                                                                                                          { _argOpolGam | _argOpolGam `seq` (True) ->
                                                                                                                                                          (case (_polArgVar) of
                                                                                                                                                           { _argOknPolCtx | _argOknPolCtx `seq` (True) ->
                                                                                                                                                           (case (arg_3 _argOknPolCtx ) of
                                                                                                                                                            { ( _argIpolVarL,arg_4) | True ->
                                                                                                                                                                (case (arg_4 _argOpolGam _argOpolVarMp ) of
                                                                                                                                                                 { ( _argImbStrictness,_argIpolVarMp,arg_5) | True ->
                                                                                                                                                                     (case (_argIpolVarMp) of
                                                                                                                                                                      { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                                      (case ((let sem_TyExpr_App_5 :: T_TyExpr_5 
                                                                                                                                                                                  sem_TyExpr_App_5  =
                                                                                                                                                                                      (\ _lhsItyKiGam ->
                                                                                                                                                                                           _lhsItyKiGam `seq`
                                                                                                                                                                                           ((case (_lhsItyKiGam) of
                                                                                                                                                                                             { _funcOtyKiGam | _funcOtyKiGam `seq` (True) ->
                                                                                                                                                                                             (case (func_5 _funcOtyKiGam ) of
                                                                                                                                                                                              { ( _funcIintlTyKiGam,_funcItyKiGam,func_6) | True ->
                                                                                                                                                                                                  (case (_funcItyKiGam) of
                                                                                                                                                                                                   { _argOtyKiGam | _argOtyKiGam `seq` (True) ->
                                                                                                                                                                                                   (case (arg_5 _argOtyKiGam ) of
                                                                                                                                                                                                    { ( _argIintlTyKiGam,_argItyKiGam,arg_6) | True ->
                                                                                                                                                                                                        (case (_funcIintlTyKiGam `gamUnion` _argIintlTyKiGam) of
                                                                                                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                                                         (case (_argItyKiGam) of
                                                                                                                                                                                                          { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                                          (case ((let sem_TyExpr_App_6 :: T_TyExpr_6 
                                                                                                                                                                                                                      sem_TyExpr_App_6  =
                                                                                                                                                                                                                          (\ _lhsIkiVarMp ->
                                                                                                                                                                                                                               _lhsIkiVarMp `seq`
                                                                                                                                                                                                                               ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                                                 { _funcOkiVarMp | _funcOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                 (case (func_6 _funcOkiVarMp ) of
                                                                                                                                                                                                                                  { ( _funcIgathTyVarPolGam,_funcIki,_funcIkiVarMp,_funcIpol,_funcItyVarWildMp,func_7) | True ->
                                                                                                                                                                                                                                      (case (_funcIkiVarMp) of
                                                                                                                                                                                                                                       { _argOkiVarMp | _argOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                       (case (arg_6 _argOkiVarMp ) of
                                                                                                                                                                                                                                        { ( _argIgathTyVarPolGam,_argIki,_argIkiVarMp,_argIpol,_argItyVarWildMp,arg_7) | True ->
                                                                                                                                                                                                                                            (case (_funcIgathTyVarPolGam `gamUnion` _argIgathTyVarPolGam) of
                                                                                                                                                                                                                                             { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                                                             (case (__tup218) of
                                                                                                                                                                                                                                              { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                                                                                                                                                                                                              (case (mkNewTyVar _lUniq) of
                                                                                                                                                                                                                                               { _knResKi | _knResKi `seq` (True) ->
                                                                                                                                                                                                                                               (case (_knResKi) of
                                                                                                                                                                                                                                                { _ki | _ki `seq` (True) ->
                                                                                                                                                                                                                                                (case (_ki) of
                                                                                                                                                                                                                                                 { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                                                                 (case (__tup218) of
                                                                                                                                                                                                                                                  { (_,_,_lUniq2,_) | _lUniq2 `seq` (True) ->
                                                                                                                                                                                                                                                  (case ([_argIki] `mkArrow` _knResKi) of
                                                                                                                                                                                                                                                   { _knFunKi | _knFunKi `seq` (True) ->
                                                                                                                                                                                                                                                   (case (fitsIn weakFIOpts defaultFIEnv _lUniq2 _argIkiVarMp _funcIki _knFunKi) of
                                                                                                                                                                                                                                                    { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                                                                    (case (foVarMp _fo_ `varUpd` _argIkiVarMp) of
                                                                                                                                                                                                                                                     { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                     (case (_lhsIknPolCtx) of
                                                                                                                                                                                                                                                      { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                                                                      (case (_funcItyVarWildMp `Map.union` _argItyVarWildMp) of
                                                                                                                                                                                                                                                       { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                                                       (case ((let sem_TyExpr_App_7 :: T_TyExpr_7 
                                                                                                                                                                                                                                                                   sem_TyExpr_App_7  =
                                                                                                                                                                                                                                                                       (\ _lhsIclGam ->
                                                                                                                                                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                                                                                                                                                            ((case (_lhsIclGam) of
                                                                                                                                                                                                                                                                              { _argOclGam | _argOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                               { _funcOclGam | _funcOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (arg_7 _argOclGam ) of
                                                                                                                                                                                                                                                                                { ( _argIevTy,arg_8) | True ->
                                                                                                                                                                                                                                                                                    (case (func_7 _funcOclGam ) of
                                                                                                                                                                                                                                                                                     { ( _funcIevTy,func_8) | True ->
                                                                                                                                                                                                                                                                                         (case (Ty_App _funcIevTy _argIevTy) of
                                                                                                                                                                                                                                                                                          { _evTy | _evTy `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_evTy) of
                                                                                                                                                                                                                                                                                           { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case ((let sem_TyExpr_App_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                                                                                       sem_TyExpr_App_8  =
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
                                                                                                                                                                                                                                                                                                                                     { ( _argIallErrSq,_argIappArgPPL,_argIappFunNm,_argIappFunPP,_argIclMissNmS,_argIclNmS,_argIerrSq,_argIgathMentrelFilterMp,_argIpp,_argItyWildL) | True ->
                                                                                                                                                                                                                                                                                                                                         (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                          { _funcOvalTyGlobFreeTvarS | _funcOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                          (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                           { _funcOtyTyTySigFreeTvarS | _funcOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                           (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                            { _funcOtyTyGlobFreeTvarS | _funcOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                            (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                                                             { _funcOtyKiGlobFreeTvarS | _funcOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                             (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                                                              { _funcOopts | _funcOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                              (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                                                               { _funcOmoduleNm | _funcOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                               (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                                                { _funcOkiGam | _funcOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                                                 { _funcOfinTyVarMp | _funcOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                 (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                                                  { _funcOfinTyKiGam | _funcOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                  (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                                                   { _funcOfinKiVarMp | _funcOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                   (case (func_8 _funcOfinKiVarMp _funcOfinTyKiGam _funcOfinTyVarMp _funcOkiGam _funcOmoduleNm _funcOopts _funcOtyKiGlobFreeTvarS _funcOtyTyGlobFreeTvarS _funcOtyTyTySigFreeTvarS _funcOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                                                    { ( _funcIallErrSq,_funcIappArgPPL,_funcIappFunNm,_funcIappFunPP,_funcIclMissNmS,_funcIclNmS,_funcIerrSq,_funcIgathMentrelFilterMp,_funcIpp,_funcItyWildL) | True ->
                                                                                                                                                                                                                                                                                                                                                        (case (_funcIallErrSq `Seq.union` _argIallErrSq) of
                                                                                                                                                                                                                                                                                                                                                         { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                         (case (mkExtAppPP  (_funcIappFunNm,_funcIappFunPP,_funcIappArgPPL)
                                                                                                                                                                                                                                                                                                                                                                            (_argIappFunNm,_argIappFunPP,_argIappArgPPL,_argIpp)) of
                                                                                                                                                                                                                                                                                                                                                          { __tup217 | __tup217 `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                          (case (__tup217) of
                                                                                                                                                                                                                                                                                                                                                           { (_,_lhsOappArgPPL) | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                           (case (_funcIappFunNm) of
                                                                                                                                                                                                                                                                                                                                                            { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                            (case (__tup217) of
                                                                                                                                                                                                                                                                                                                                                             { (_lhsOappFunPP,_) | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                             (case (_funcIclMissNmS `Set.union` _argIclMissNmS) of
                                                                                                                                                                                                                                                                                                                                                              { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                              (case (_funcIclNmS `Set.union` _argIclNmS) of
                                                                                                                                                                                                                                                                                                                                                               { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                               (case (_funcIpp >#< _argIpp) of
                                                                                                                                                                                                                                                                                                                                                                { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                (case (rngLift _range mkNestErr' _pp [ _funcIerrSq, _argIerrSq
                                                                                                                                                                                                                                                                                                                                                                                                     , foErrSq _fo_
                                                                                                                                                                                                                                                                                                                                                                                                     ]) of
                                                                                                                                                                                                                                                                                                                                                                 { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                 (case (_funcIgathMentrelFilterMp `mentrelFilterMpUnion` _argIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                                                  { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                  (case (_pp) of
                                                                                                                                                                                                                                                                                                                                                                   { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                   (case (_funcItyWildL ++ _argItyWildL) of
                                                                                                                                                                                                                                                                                                                                                                    { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                                                    ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                                                                                   in  sem_TyExpr_App_8)) of
                                                                                                                                                                                                                                                                                            { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                                                                            ( _lhsOevTy,sem_TyExpr_8) }) }) }) }) }) }) })))
                                                                                                                                                                                                                                                               in  sem_TyExpr_App_7)) of
                                                                                                                                                                                                                                                        { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                                                                        ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                                                  in  sem_TyExpr_App_6)) of
                                                                                                                                                                                                           { ( sem_TyExpr_6) | True ->
                                                                                                                                                                                                           ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) }) }) })))
                                                                                                                                                                              in  sem_TyExpr_App_5)) of
                                                                                                                                                                       { ( sem_TyExpr_5) | True ->
                                                                                                                                                                       ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                        in  sem_TyExpr_App_4)) of
                                                                                                                 { ( sem_TyExpr_4) | True ->
                                                                                                                 ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                                                 in  sem_TyExpr_App_3)) of
                                                                                          { ( sem_TyExpr_3) | True ->
                                                                                          ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) }) })))
                                                            in  sem_TyExpr_App_2)) of
                                                     { ( sem_TyExpr_2) | True ->
                                                     ( _lhsOgUniq,sem_TyExpr_2) }) }) }) }) }) }) })))
                        in  sem_TyExpr_App_1)) of
                 { ( sem_TyExpr_1) | True ->
                 ( _lhsOrange,sem_TyExpr_1) }) }) }) }) })

sem_TyExpr_AppTop :: Range ->
                     T_TyExpr  ->
                     T_TyExpr 

sem_TyExpr_AppTop hsrange_ tyExpr_  | hsrange_ `seq` (tyExpr_ `seq` (True)) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_AppTop_1 :: T_TyExpr_1 
                       sem_TyExpr_AppTop_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                  (case (tyExpr_1 _tyExprOgUniq ) of
                                   { ( _tyExprIgUniq,tyExpr_2) | True ->
                                       (case (_tyExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_TyExpr_AppTop_2 :: T_TyExpr_2 
                                                    sem_TyExpr_AppTop_2  =
                                                        (\ _lhsItyGam ->
                                                             _lhsItyGam `seq`
                                                             ((case (_lhsItyGam) of
                                                               { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                               (case (tyExpr_2 _tyExprOtyGam ) of
                                                                { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                    (case (_tyExprIty) of
                                                                     { _lhsOty | _lhsOty `seq` (True) ->
                                                                     (case (_tyExprItyGam) of
                                                                      { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                      (case ((let sem_TyExpr_AppTop_3 :: T_TyExpr_3 
                                                                                  sem_TyExpr_AppTop_3  =
                                                                                      (\ _lhsIknPolCtx ->
                                                                                           _lhsIknPolCtx `seq`
                                                                                           ((case ([]) of
                                                                                             { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                             (case ((let sem_TyExpr_AppTop_4 :: T_TyExpr_4 
                                                                                                         sem_TyExpr_AppTop_4  =
                                                                                                             (\ _lhsIpolGam
                                                                                                                _lhsIpolVarMp ->
                                                                                                                  _lhsIpolGam `seq`
                                                                                                                  (_lhsIpolVarMp `seq`
                                                                                                                   ((case (Nothing) of
                                                                                                                     { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                     (case (_lhsIpolVarMp) of
                                                                                                                      { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                      (case (_lhsIpolGam) of
                                                                                                                       { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                       (case (_lhsIknPolCtx) of
                                                                                                                        { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                                        (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                                         { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                                             (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                              { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                  (case (_tyExprIpolVarMp) of
                                                                                                                                   { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                   (case ((let sem_TyExpr_AppTop_5 :: T_TyExpr_5 
                                                                                                                                               sem_TyExpr_AppTop_5  =
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
                                                                                                                                                                 (case ((let sem_TyExpr_AppTop_6 :: T_TyExpr_6 
                                                                                                                                                                             sem_TyExpr_AppTop_6  =
                                                                                                                                                                                 (\ _lhsIkiVarMp ->
                                                                                                                                                                                      _lhsIkiVarMp `seq`
                                                                                                                                                                                      ((case (_lhsIkiVarMp) of
                                                                                                                                                                                        { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                        (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                         { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                             (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                              { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                              (case (_tyExprIki) of
                                                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                               (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                (case (_tyExprIpol) of
                                                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                 (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                  (case ((let sem_TyExpr_AppTop_7 :: T_TyExpr_7 
                                                                                                                                                                                                              sem_TyExpr_AppTop_7  =
                                                                                                                                                                                                                  (\ _lhsIclGam ->
                                                                                                                                                                                                                       _lhsIclGam `seq`
                                                                                                                                                                                                                       ((case (_lhsIclGam) of
                                                                                                                                                                                                                         { _tyExprOclGam | _tyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                         (case (tyExpr_7 _tyExprOclGam ) of
                                                                                                                                                                                                                          { ( _tyExprIevTy,tyExpr_8) | True ->
                                                                                                                                                                                                                              (case (_tyExprIevTy) of
                                                                                                                                                                                                                               { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                               (case ((let sem_TyExpr_AppTop_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                           sem_TyExpr_AppTop_8  =
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
                                                                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                                                                               { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (ppAppTop  (_tyExprIappFunNm,_tyExprIappFunPP)
                                                                                                                                                                                                                                                                                                 _tyExprIappArgPPL _tyExprIpp) of
                                                                                                                                                                                                                                                                                 { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                                                                                                                  { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (rngLift _range mkNestErr' _pp [_tyExprIerrSq]) of
                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_tyExprItyWildL) of
                                                                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                       in  sem_TyExpr_AppTop_8)) of
                                                                                                                                                                                                                                { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                ( _lhsOevTy,sem_TyExpr_8) }) }) }) })))
                                                                                                                                                                                                          in  sem_TyExpr_AppTop_7)) of
                                                                                                                                                                                                   { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                   ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                                         in  sem_TyExpr_AppTop_6)) of
                                                                                                                                                                  { ( sem_TyExpr_6) | True ->
                                                                                                                                                                  ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) })))
                                                                                                                                           in  sem_TyExpr_AppTop_5)) of
                                                                                                                                    { ( sem_TyExpr_5) | True ->
                                                                                                                                    ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                                     in  sem_TyExpr_AppTop_4)) of
                                                                                              { ( sem_TyExpr_4) | True ->
                                                                                              ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                              in  sem_TyExpr_AppTop_3)) of
                                                                       { ( sem_TyExpr_3) | True ->
                                                                       ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) })))
                                                in  sem_TyExpr_AppTop_2)) of
                                         { ( sem_TyExpr_2) | True ->
                                         ( _lhsOgUniq,sem_TyExpr_2) }) }) }) })))
                   in  sem_TyExpr_AppTop_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Con :: Range ->
                  HsName ->
                  T_TyExpr 

sem_TyExpr_Con hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_Con_1 :: T_TyExpr_1 
                  sem_TyExpr_Con_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq_17_fitsIn_pol) -> (__cont, lUniq_17_fitsIn_pol)} )) of
                             { __tup223 | __tup223 `seq` (True) ->
                             (case (__tup223) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_Con_2 :: T_TyExpr_2 
                                          sem_TyExpr_Con_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (Ty_Con nm_) of
                                                     { _ty | _ty `seq` (True) ->
                                                     (case (_ty) of
                                                      { _lhsOty | _lhsOty `seq` (True) ->
                                                      (case (_lhsItyGam) of
                                                       { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                       (case ((let sem_TyExpr_Con_3 :: T_TyExpr_3 
                                                                   sem_TyExpr_Con_3  =
                                                                       (\ _lhsIknPolCtx ->
                                                                            _lhsIknPolCtx `seq`
                                                                            ((case ([]) of
                                                                              { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                              (case ((let sem_TyExpr_Con_4 :: T_TyExpr_4 
                                                                                          sem_TyExpr_Con_4  =
                                                                                              (\ _lhsIpolGam
                                                                                                 _lhsIpolVarMp ->
                                                                                                   _lhsIpolGam `seq`
                                                                                                   (_lhsIpolVarMp `seq`
                                                                                                    ((case (Nothing) of
                                                                                                      { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                      (case (__tup223) of
                                                                                                       { (_,_lUniq_17_fitsIn_pol) | _lUniq_17_fitsIn_pol `seq` (True) ->
                                                                                                       (case (polGamLookupErr nm_ _lhsIpolGam) of
                                                                                                        { __tup222 | __tup222 `seq` (True) ->
                                                                                                        (case (__tup222) of
                                                                                                         { (_pgi_,_) | _pgi_ `seq` (True) ->
                                                                                                         (case (pgiPol _pgi_) of
                                                                                                          { _polFromEnv | _polFromEnv `seq` (True) ->
                                                                                                          (case (fitsIn weakFIOpts defaultFIEnv _lUniq_17_fitsIn_pol _lhsIpolVarMp _polFromEnv _lhsIknPolCtx) of
                                                                                                           { _foPol | _foPol `seq` (True) ->
                                                                                                           (case (foVarMp _foPol `varUpd` _lhsIpolVarMp) of
                                                                                                            { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                            (case ((let sem_TyExpr_Con_5 :: T_TyExpr_5 
                                                                                                                        sem_TyExpr_Con_5  =
                                                                                                                            (\ _lhsItyKiGam ->
                                                                                                                                 _lhsItyKiGam `seq`
                                                                                                                                 ((case (emptyGam) of
                                                                                                                                   { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                   (case (_lhsItyKiGam) of
                                                                                                                                    { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                    (case ((let sem_TyExpr_Con_6 :: T_TyExpr_6 
                                                                                                                                                sem_TyExpr_Con_6  =
                                                                                                                                                    (\ _lhsIkiVarMp ->
                                                                                                                                                         _lhsIkiVarMp `seq`
                                                                                                                                                         ((case (emptyGam) of
                                                                                                                                                           { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                           (case (tyKiGamLookupByNameErr nm_ _lhsItyKiGam) of
                                                                                                                                                            { __tup221 | __tup221 `seq` (True) ->
                                                                                                                                                            (case (__tup221) of
                                                                                                                                                             { (_tkgi_,_) | _tkgi_ `seq` (True) ->
                                                                                                                                                             (case (tkgiKi _tkgi_) of
                                                                                                                                                              { _ki | _ki `seq` (True) ->
                                                                                                                                                              (case (_ki) of
                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                               (case (_lhsIkiVarMp) of
                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                (case (foTy _foPol) of
                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                 (case (Map.empty) of
                                                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                  (case ((let sem_TyExpr_Con_7 :: T_TyExpr_7 
                                                                                                                                                                              sem_TyExpr_Con_7  =
                                                                                                                                                                                  (\ _lhsIclGam ->
                                                                                                                                                                                       _lhsIclGam `seq`
                                                                                                                                                                                       ((case (_ty) of
                                                                                                                                                                                         { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                         (case ((let sem_TyExpr_Con_8 :: T_TyExpr_8 
                                                                                                                                                                                                     sem_TyExpr_Con_8  =
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
                                                                                                                                                                                                                       ((case (Seq.empty) of
                                                                                                                                                                                                                         { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                         (case ([]) of
                                                                                                                                                                                                                          { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                          (case (nm_) of
                                                                                                                                                                                                                           { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                           (case (ppCon nm_) of
                                                                                                                                                                                                                            { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                            (case (_pp) of
                                                                                                                                                                                                                             { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                             (case (Set.empty) of
                                                                                                                                                                                                                              { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                              (case (Set.empty) of
                                                                                                                                                                                                                               { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                               (case (__tup222) of
                                                                                                                                                                                                                                { (_,_nmPolErrs) | _nmPolErrs `seq` (True) ->
                                                                                                                                                                                                                                (case (__tup221) of
                                                                                                                                                                                                                                 { (_,_nmErrs2) | _nmErrs2 `seq` (True) ->
                                                                                                                                                                                                                                 (case (tyGamLookupErr nm_ _lhsItyGam) of
                                                                                                                                                                                                                                  { __tup220 | __tup220 `seq` (True) ->
                                                                                                                                                                                                                                  (case (__tup220) of
                                                                                                                                                                                                                                   { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                                                                                                                                                                   (case (rngLift _range mkNestErr' _pp [Seq.fromList $ firstNotEmpty [_nmErrs,_nmErrs2,_nmPolErrs], foErrSq _foPol    ]) of
                                                                                                                                                                                                                                    { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                    (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Type nm_) of
                                                                                                                                                                                                                                     { _gathMentrelFilterMp | _gathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                     (case (_gathMentrelFilterMp) of
                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                 in  sem_TyExpr_Con_8)) of
                                                                                                                                                                                          { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                          ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                          in  sem_TyExpr_Con_7)) of
                                                                                                                                                                   { ( sem_TyExpr_7) | True ->
                                                                                                                                                                   ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) }) })))
                                                                                                                                            in  sem_TyExpr_Con_6)) of
                                                                                                                                     { ( sem_TyExpr_6) | True ->
                                                                                                                                     ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) })))
                                                                                                                    in  sem_TyExpr_Con_5)) of
                                                                                                             { ( sem_TyExpr_5) | True ->
                                                                                                             ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                      in  sem_TyExpr_Con_4)) of
                                                                               { ( sem_TyExpr_4) | True ->
                                                                               ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                               in  sem_TyExpr_Con_3)) of
                                                        { ( sem_TyExpr_3) | True ->
                                                        ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) })))
                                      in  sem_TyExpr_Con_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_Con_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_Impls :: Range ->
                    T_TyExpr 

sem_TyExpr_Impls hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_Impls_1 :: T_TyExpr_1 
                  sem_TyExpr_Impls_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup224 | __tup224 `seq` (True) ->
                             (case (__tup224) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_Impls_2 :: T_TyExpr_2 
                                          sem_TyExpr_Impls_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup224) of
                                                     { (_,_lUniq) | _lUniq `seq` (True) ->
                                                     (case (_lUniq) of
                                                      { _implsVarId | _implsVarId `seq` (True) ->
                                                      (case (mkTGI (Ty_Impls (Impls_Tail _implsVarId []))) of
                                                       { _tgi_ | _tgi_ `seq` (True) ->
                                                       (case (tgiTy _tgi_) of
                                                        { _ty | _ty `seq` (True) ->
                                                        (case (_ty) of
                                                         { _lhsOty | _lhsOty `seq` (True) ->
                                                         (case (_lhsItyGam) of
                                                          { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                          (case ((let sem_TyExpr_Impls_3 :: T_TyExpr_3 
                                                                      sem_TyExpr_Impls_3  =
                                                                          (\ _lhsIknPolCtx ->
                                                                               _lhsIknPolCtx `seq`
                                                                               ((case ([]) of
                                                                                 { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                 (case ((let sem_TyExpr_Impls_4 :: T_TyExpr_4 
                                                                                             sem_TyExpr_Impls_4  =
                                                                                                 (\ _lhsIpolGam
                                                                                                    _lhsIpolVarMp ->
                                                                                                      _lhsIpolGam `seq`
                                                                                                      (_lhsIpolVarMp `seq`
                                                                                                       ((case (Nothing) of
                                                                                                         { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                         (case (_lhsIpolVarMp) of
                                                                                                          { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                          (case ((let sem_TyExpr_Impls_5 :: T_TyExpr_5 
                                                                                                                      sem_TyExpr_Impls_5  =
                                                                                                                          (\ _lhsItyKiGam ->
                                                                                                                               _lhsItyKiGam `seq`
                                                                                                                               ((case (emptyGam) of
                                                                                                                                 { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                 (case (_lhsItyKiGam) of
                                                                                                                                  { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                  (case ((let sem_TyExpr_Impls_6 :: T_TyExpr_6 
                                                                                                                                              sem_TyExpr_Impls_6  =
                                                                                                                                                  (\ _lhsIkiVarMp ->
                                                                                                                                                       _lhsIkiVarMp `seq`
                                                                                                                                                       ((case (emptyGam) of
                                                                                                                                                         { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                         (case (TyKiGamInfo kiStar) of
                                                                                                                                                          { _tkgi_ | _tkgi_ `seq` (True) ->
                                                                                                                                                          (case (tkgiKi _tkgi_) of
                                                                                                                                                           { _ki | _ki `seq` (True) ->
                                                                                                                                                           (case (_ki) of
                                                                                                                                                            { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                            (case (_lhsIkiVarMp) of
                                                                                                                                                             { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                             (case (_lhsIknPolCtx) of
                                                                                                                                                              { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                              (case (Map.singleton _implsVarId TyVarWild_NoQuantTyExpr_YesQuantLetBinding) of
                                                                                                                                                               { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                               (case ((let sem_TyExpr_Impls_7 :: T_TyExpr_7 
                                                                                                                                                                           sem_TyExpr_Impls_7  =
                                                                                                                                                                               (\ _lhsIclGam ->
                                                                                                                                                                                    _lhsIclGam `seq`
                                                                                                                                                                                    ((case (_ty) of
                                                                                                                                                                                      { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                      (case ((let sem_TyExpr_Impls_8 :: T_TyExpr_8 
                                                                                                                                                                                                  sem_TyExpr_Impls_8  =
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
                                                                                                                                                                                                                    ((case (Seq.empty) of
                                                                                                                                                                                                                      { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                      (case ([]) of
                                                                                                                                                                                                                       { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                       (case (hsnUnknown) of
                                                                                                                                                                                                                        { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                        (case (hsnOImpl >#< "..." >#< hsnCImpl) of
                                                                                                                                                                                                                         { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                         (case (_pp) of
                                                                                                                                                                                                                          { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                          (case (Set.empty) of
                                                                                                                                                                                                                           { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                           (case (Set.empty) of
                                                                                                                                                                                                                            { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                            (case (Seq.empty) of
                                                                                                                                                                                                                             { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                             (case (Map.empty) of
                                                                                                                                                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                              (case (_pp) of
                                                                                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                               (case ([]) of
                                                                                                                                                                                                                                { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                              in  sem_TyExpr_Impls_8)) of
                                                                                                                                                                                       { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                       ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                       in  sem_TyExpr_Impls_7)) of
                                                                                                                                                                { ( sem_TyExpr_7) | True ->
                                                                                                                                                                ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                          in  sem_TyExpr_Impls_6)) of
                                                                                                                                   { ( sem_TyExpr_6) | True ->
                                                                                                                                   ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) })))
                                                                                                                  in  sem_TyExpr_Impls_5)) of
                                                                                                           { ( sem_TyExpr_5) | True ->
                                                                                                           ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }))))
                                                                                         in  sem_TyExpr_Impls_4)) of
                                                                                  { ( sem_TyExpr_4) | True ->
                                                                                  ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                  in  sem_TyExpr_Impls_3)) of
                                                           { ( sem_TyExpr_3) | True ->
                                                           ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) })))
                                      in  sem_TyExpr_Impls_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_Impls_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_Lam :: Range ->
                  HsName ->
                  T_TyExpr  ->
                  T_TyExpr 

sem_TyExpr_Lam hsrange_ tyVar_ tyExpr_  | hsrange_ `seq` (tyVar_ `seq` (tyExpr_ `seq` (True))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Lam_1 :: T_TyExpr_1 
                       sem_TyExpr_Lam_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_17_fitsIn_pol) -> case nextUnique __cont of { (__cont, lUniq_17_polArg) -> case nextUnique __cont of { (__cont, lUniq_17_polRes) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_17_fitsIn_pol,lUniq_17_polArg,lUniq_17_polRes,lUniq_ki)}}}}} )) of
                                  { __tup226 | __tup226 `seq` (True) ->
                                  (case (__tup226) of
                                   { (_tyExprOgUniq,_,_,_,_,_) | _tyExprOgUniq `seq` (True) ->
                                   (case (tyExpr_1 _tyExprOgUniq ) of
                                    { ( _tyExprIgUniq,tyExpr_2) | True ->
                                        (case (_tyExprIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_TyExpr_Lam_2 :: T_TyExpr_2 
                                                     sem_TyExpr_Lam_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (__tup226) of
                                                                { (_,_,_,_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                (case (__tup226) of
                                                                 { (_,_lUniq,_,_,_,_) | _lUniq `seq` (True) ->
                                                                 (case (let  t = mkTyVar _lUniq
                                                                        in   (_lUniq,mkTGI t,TyKiGamInfo (mkNewTyVar _lUniq_ki))) of
                                                                  { __tup225 | __tup225 `seq` (True) ->
                                                                  (case (__tup225) of
                                                                   { (_,_tgi_,_) | _tgi_ `seq` (True) ->
                                                                   (case (tvGathFlowIn  (tyVar_ `gamSingleton` _tgi_) _lhsItyGam) of
                                                                    { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                                    (case (__tup225) of
                                                                     { (_tv,_,_) | _tv `seq` (True) ->
                                                                     (case (tyExpr_2 _tyExprOtyGam ) of
                                                                      { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                          (case (mkTyLam [_tv] _tyExprIty) of
                                                                           { _ty | _ty `seq` (True) ->
                                                                           (case (_ty) of
                                                                            { _lhsOty | _lhsOty `seq` (True) ->
                                                                            (case (tvGathFlowOut _lhsItyGam _tyExprItyGam) of
                                                                             { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                             (case ((let sem_TyExpr_Lam_3 :: T_TyExpr_3 
                                                                                         sem_TyExpr_Lam_3  =
                                                                                             (\ _lhsIknPolCtx ->
                                                                                                  _lhsIknPolCtx `seq`
                                                                                                  ((case ([]) of
                                                                                                    { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                    (case ((let sem_TyExpr_Lam_4 :: T_TyExpr_4 
                                                                                                                sem_TyExpr_Lam_4  =
                                                                                                                    (\ _lhsIpolGam
                                                                                                                       _lhsIpolVarMp ->
                                                                                                                         _lhsIpolGam `seq`
                                                                                                                         (_lhsIpolVarMp `seq`
                                                                                                                          ((case (Nothing) of
                                                                                                                            { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                            (case (__tup226) of
                                                                                                                             { (_,_,_,_,_lUniq_17_polRes,_) | _lUniq_17_polRes `seq` (True) ->
                                                                                                                             (case (__tup226) of
                                                                                                                              { (_,_,_,_lUniq_17_polArg,_,_) | _lUniq_17_polArg `seq` (True) ->
                                                                                                                              (case (__tup226) of
                                                                                                                               { (_,_,_lUniq_17_fitsIn_pol,_,_,_) | _lUniq_17_fitsIn_pol `seq` (True) ->
                                                                                                                               (case (mkPolVar _lUniq_17_polRes) of
                                                                                                                                { _polResVar | _polResVar `seq` (True) ->
                                                                                                                                (case (mkPolVar _lUniq_17_polArg) of
                                                                                                                                 { _polArgVar | _polArgVar `seq` (True) ->
                                                                                                                                 (case (fitsIn weakFIOpts defaultFIEnv _lUniq_17_fitsIn_pol _lhsIpolVarMp ([_polArgVar] `mkArrow` _polResVar) _lhsIknPolCtx) of
                                                                                                                                  { _foPol | _foPol `seq` (True) ->
                                                                                                                                  (case (foVarMp _foPol `varUpd` _lhsIpolVarMp) of
                                                                                                                                   { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                                   (case (gamAdd tyVar_ (mkPGI _polArgVar) _lhsIpolGam) of
                                                                                                                                    { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                                    (case (_polResVar) of
                                                                                                                                     { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                                                     (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                                                      { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                                                          (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                                           { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                               (case (_tyExprIpolVarMp) of
                                                                                                                                                { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                                (case ((let sem_TyExpr_Lam_5 :: T_TyExpr_5 
                                                                                                                                                            sem_TyExpr_Lam_5  =
                                                                                                                                                                (\ _lhsItyKiGam ->
                                                                                                                                                                     _lhsItyKiGam `seq`
                                                                                                                                                                     ((case (__tup225) of
                                                                                                                                                                       { (_,_,_tkgi_) | _tkgi_ `seq` (True) ->
                                                                                                                                                                       (case (tgiTy _tgi_ `tyKiGamSingleton` _tkgi_) of
                                                                                                                                                                        { _tyKiGamNew | _tyKiGamNew `seq` (True) ->
                                                                                                                                                                        (case (tvGathFlowIn  _tyKiGamNew _lhsItyKiGam) of
                                                                                                                                                                         { _tyExprOtyKiGam | _tyExprOtyKiGam `seq` (True) ->
                                                                                                                                                                         (case (tyExpr_5 _tyExprOtyKiGam ) of
                                                                                                                                                                          { ( _tyExprIintlTyKiGam,_tyExprItyKiGam,tyExpr_6) | True ->
                                                                                                                                                                              (case (gamUnions [_tyKiGamNew,_tyExprIintlTyKiGam]) of
                                                                                                                                                                               { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                               (case (tvGathFlowOut _lhsItyKiGam _tyExprItyKiGam) of
                                                                                                                                                                                { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                (case ((let sem_TyExpr_Lam_6 :: T_TyExpr_6 
                                                                                                                                                                                            sem_TyExpr_Lam_6  =
                                                                                                                                                                                                (\ _lhsIkiVarMp ->
                                                                                                                                                                                                     _lhsIkiVarMp `seq`
                                                                                                                                                                                                     ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                       { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                                       (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                                        { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                                            (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                                             { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                             (case ([tkgiKi _tkgi_] `mkArrow` _tyExprIki) of
                                                                                                                                                                                                              { _ki | _ki `seq` (True) ->
                                                                                                                                                                                                              (case (_ki) of
                                                                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                               (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                (case ([_polArgVar] `mkArrow` _tyExprIpol) of
                                                                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                                 (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                  (case ((let sem_TyExpr_Lam_7 :: T_TyExpr_7 
                                                                                                                                                                                                                              sem_TyExpr_Lam_7  =
                                                                                                                                                                                                                                  (\ _lhsIclGam ->
                                                                                                                                                                                                                                       _lhsIclGam `seq`
                                                                                                                                                                                                                                       ((case (_ty) of
                                                                                                                                                                                                                                         { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                         (case ((let sem_TyExpr_Lam_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                                     sem_TyExpr_Lam_8  =
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
                                                                                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                                                                                               { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                                { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case ("\\" >|< tyVar_ >#< "->" >#< _tyExprIpp) of
                                                                                                                                                                                                                                                                                                 { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                                                                                                                                  { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                                  (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                   (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                                    (case (_tyExprIerrSq) of
                                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                     (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (_tyExprItyWildL) of
                                                                                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                                 in  sem_TyExpr_Lam_8)) of
                                                                                                                                                                                                                                          { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                          ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                                                                          in  sem_TyExpr_Lam_7)) of
                                                                                                                                                                                                                   { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                                   ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                        in  sem_TyExpr_Lam_6)) of
                                                                                                                                                                                 { ( sem_TyExpr_6) | True ->
                                                                                                                                                                                 ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) }) }) })))
                                                                                                                                                        in  sem_TyExpr_Lam_5)) of
                                                                                                                                                 { ( sem_TyExpr_5) | True ->
                                                                                                                                                 ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))
                                                                                                            in  sem_TyExpr_Lam_4)) of
                                                                                                     { ( sem_TyExpr_4) | True ->
                                                                                                     ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                                     in  sem_TyExpr_Lam_3)) of
                                                                              { ( sem_TyExpr_3) | True ->
                                                                              ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) }) }) }) }) })))
                                                 in  sem_TyExpr_Lam_2)) of
                                          { ( sem_TyExpr_2) | True ->
                                          ( _lhsOgUniq,sem_TyExpr_2) }) }) }) }) })))
                   in  sem_TyExpr_Lam_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Mono :: Range ->
                   T_TyExpr 

sem_TyExpr_Mono hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_Mono_1 :: T_TyExpr_1 
                  sem_TyExpr_Mono_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup227 | __tup227 `seq` (True) ->
                             (case (__tup227) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_Mono_2 :: T_TyExpr_2 
                                          sem_TyExpr_Mono_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup227) of
                                                     { (_,_lUniq) | _lUniq `seq` (True) ->
                                                     (case (_lUniq) of
                                                      { _tyVarId | _tyVarId `seq` (True) ->
                                                      (case (mkNewTyVar _tyVarId) of
                                                       { _tvarv | _tvarv `seq` (True) ->
                                                       (case (mkTGI (Ty_Ann TyAnn_Mono _tvarv)) of
                                                        { _tgi_ | _tgi_ `seq` (True) ->
                                                        (case (tgiTy _tgi_) of
                                                         { _ty | _ty `seq` (True) ->
                                                         (case (_ty) of
                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                          (case (_lhsItyGam) of
                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                           (case ((let sem_TyExpr_Mono_3 :: T_TyExpr_3 
                                                                       sem_TyExpr_Mono_3  =
                                                                           (\ _lhsIknPolCtx ->
                                                                                _lhsIknPolCtx `seq`
                                                                                ((case ([]) of
                                                                                  { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                  (case ((let sem_TyExpr_Mono_4 :: T_TyExpr_4 
                                                                                              sem_TyExpr_Mono_4  =
                                                                                                  (\ _lhsIpolGam
                                                                                                     _lhsIpolVarMp ->
                                                                                                       _lhsIpolGam `seq`
                                                                                                       (_lhsIpolVarMp `seq`
                                                                                                        ((case (Nothing) of
                                                                                                          { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                          (case (_lhsIpolVarMp) of
                                                                                                           { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                           (case ((let sem_TyExpr_Mono_5 :: T_TyExpr_5 
                                                                                                                       sem_TyExpr_Mono_5  =
                                                                                                                           (\ _lhsItyKiGam ->
                                                                                                                                _lhsItyKiGam `seq`
                                                                                                                                ((case (emptyGam) of
                                                                                                                                  { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                  (case (_lhsItyKiGam) of
                                                                                                                                   { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                   (case ((let sem_TyExpr_Mono_6 :: T_TyExpr_6 
                                                                                                                                               sem_TyExpr_Mono_6  =
                                                                                                                                                   (\ _lhsIkiVarMp ->
                                                                                                                                                        _lhsIkiVarMp `seq`
                                                                                                                                                        ((case (emptyGam) of
                                                                                                                                                          { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                          (case (TyKiGamInfo _tvarv) of
                                                                                                                                                           { _tkgi_ | _tkgi_ `seq` (True) ->
                                                                                                                                                           (case (tkgiKi _tkgi_) of
                                                                                                                                                            { _ki | _ki `seq` (True) ->
                                                                                                                                                            (case (_ki) of
                                                                                                                                                             { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                             (case (_lhsIkiVarMp) of
                                                                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                              (case (_lhsIknPolCtx) of
                                                                                                                                                               { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                               (case (Map.singleton _tyVarId TyVarWild_NoQuantTyExpr_NoQuantLetBinding) of
                                                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                (case ((let sem_TyExpr_Mono_7 :: T_TyExpr_7 
                                                                                                                                                                            sem_TyExpr_Mono_7  =
                                                                                                                                                                                (\ _lhsIclGam ->
                                                                                                                                                                                     _lhsIclGam `seq`
                                                                                                                                                                                     ((case (_ty) of
                                                                                                                                                                                       { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                       (case ((let sem_TyExpr_Mono_8 :: T_TyExpr_8 
                                                                                                                                                                                                   sem_TyExpr_Mono_8  =
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
                                                                                                                                                                                                                     ((case (Seq.empty) of
                                                                                                                                                                                                                       { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                        { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                        (case (hsnUnknown) of
                                                                                                                                                                                                                         { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                         (case (pp "%...") of
                                                                                                                                                                                                                          { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                          (case (_pp) of
                                                                                                                                                                                                                           { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                           (case (Set.empty) of
                                                                                                                                                                                                                            { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                            (case (Set.empty) of
                                                                                                                                                                                                                             { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                             (case (Seq.empty) of
                                                                                                                                                                                                                              { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                              (case (Map.empty) of
                                                                                                                                                                                                                               { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                               (case (_pp) of
                                                                                                                                                                                                                                { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                (case ([]) of
                                                                                                                                                                                                                                 { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                               in  sem_TyExpr_Mono_8)) of
                                                                                                                                                                                        { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                        ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                        in  sem_TyExpr_Mono_7)) of
                                                                                                                                                                 { ( sem_TyExpr_7) | True ->
                                                                                                                                                                 ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                           in  sem_TyExpr_Mono_6)) of
                                                                                                                                    { ( sem_TyExpr_6) | True ->
                                                                                                                                    ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) })))
                                                                                                                   in  sem_TyExpr_Mono_5)) of
                                                                                                            { ( sem_TyExpr_5) | True ->
                                                                                                            ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }))))
                                                                                          in  sem_TyExpr_Mono_4)) of
                                                                                   { ( sem_TyExpr_4) | True ->
                                                                                   ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                   in  sem_TyExpr_Mono_3)) of
                                                            { ( sem_TyExpr_3) | True ->
                                                            ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) }) })))
                                      in  sem_TyExpr_Mono_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_Mono_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_NoImpls :: Range ->
                      T_TyExpr 

sem_TyExpr_NoImpls hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_NoImpls_1 :: T_TyExpr_1 
                  sem_TyExpr_NoImpls_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (_lhsIgUniq) of
                             { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                             (case ((let sem_TyExpr_NoImpls_2 :: T_TyExpr_2 
                                         sem_TyExpr_NoImpls_2  =
                                             (\ _lhsItyGam ->
                                                  _lhsItyGam `seq`
                                                  ((case (mkTGI (Ty_Impls Impls_Nil)) of
                                                    { _tgi_ | _tgi_ `seq` (True) ->
                                                    (case (tgiTy _tgi_) of
                                                     { _ty | _ty `seq` (True) ->
                                                     (case (_ty) of
                                                      { _lhsOty | _lhsOty `seq` (True) ->
                                                      (case (_lhsItyGam) of
                                                       { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                       (case ((let sem_TyExpr_NoImpls_3 :: T_TyExpr_3 
                                                                   sem_TyExpr_NoImpls_3  =
                                                                       (\ _lhsIknPolCtx ->
                                                                            _lhsIknPolCtx `seq`
                                                                            ((case ([]) of
                                                                              { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                              (case ((let sem_TyExpr_NoImpls_4 :: T_TyExpr_4 
                                                                                          sem_TyExpr_NoImpls_4  =
                                                                                              (\ _lhsIpolGam
                                                                                                 _lhsIpolVarMp ->
                                                                                                   _lhsIpolGam `seq`
                                                                                                   (_lhsIpolVarMp `seq`
                                                                                                    ((case (Nothing) of
                                                                                                      { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                      (case (_lhsIpolVarMp) of
                                                                                                       { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                       (case ((let sem_TyExpr_NoImpls_5 :: T_TyExpr_5 
                                                                                                                   sem_TyExpr_NoImpls_5  =
                                                                                                                       (\ _lhsItyKiGam ->
                                                                                                                            _lhsItyKiGam `seq`
                                                                                                                            ((case (emptyGam) of
                                                                                                                              { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                              (case (_lhsItyKiGam) of
                                                                                                                               { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                               (case ((let sem_TyExpr_NoImpls_6 :: T_TyExpr_6 
                                                                                                                                           sem_TyExpr_NoImpls_6  =
                                                                                                                                               (\ _lhsIkiVarMp ->
                                                                                                                                                    _lhsIkiVarMp `seq`
                                                                                                                                                    ((case (emptyGam) of
                                                                                                                                                      { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                      (case (TyKiGamInfo kiStar) of
                                                                                                                                                       { _tkgi_ | _tkgi_ `seq` (True) ->
                                                                                                                                                       (case (tkgiKi _tkgi_) of
                                                                                                                                                        { _ki | _ki `seq` (True) ->
                                                                                                                                                        (case (_ki) of
                                                                                                                                                         { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                         (case (_lhsIkiVarMp) of
                                                                                                                                                          { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                          (case (_lhsIknPolCtx) of
                                                                                                                                                           { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                           (case (Map.empty) of
                                                                                                                                                            { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                            (case ((let sem_TyExpr_NoImpls_7 :: T_TyExpr_7 
                                                                                                                                                                        sem_TyExpr_NoImpls_7  =
                                                                                                                                                                            (\ _lhsIclGam ->
                                                                                                                                                                                 _lhsIclGam `seq`
                                                                                                                                                                                 ((case (_ty) of
                                                                                                                                                                                   { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                   (case ((let sem_TyExpr_NoImpls_8 :: T_TyExpr_8 
                                                                                                                                                                                               sem_TyExpr_NoImpls_8  =
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
                                                                                                                                                                                                                 ((case (Seq.empty) of
                                                                                                                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                   (case ([]) of
                                                                                                                                                                                                                    { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                    (case (hsnUnknown) of
                                                                                                                                                                                                                     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                     (case (hsnOImpl >#< "_" >#< hsnCImpl) of
                                                                                                                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                       { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                       (case (Set.empty) of
                                                                                                                                                                                                                        { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                        (case (Set.empty) of
                                                                                                                                                                                                                         { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                         (case (Seq.empty) of
                                                                                                                                                                                                                          { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                          (case (Map.empty) of
                                                                                                                                                                                                                           { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                           (case (_pp) of
                                                                                                                                                                                                                            { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                            (case ([]) of
                                                                                                                                                                                                                             { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                             ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                           in  sem_TyExpr_NoImpls_8)) of
                                                                                                                                                                                    { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                    ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                    in  sem_TyExpr_NoImpls_7)) of
                                                                                                                                                             { ( sem_TyExpr_7) | True ->
                                                                                                                                                             ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                       in  sem_TyExpr_NoImpls_6)) of
                                                                                                                                { ( sem_TyExpr_6) | True ->
                                                                                                                                ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) })))
                                                                                                               in  sem_TyExpr_NoImpls_5)) of
                                                                                                        { ( sem_TyExpr_5) | True ->
                                                                                                        ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }))))
                                                                                      in  sem_TyExpr_NoImpls_4)) of
                                                                               { ( sem_TyExpr_4) | True ->
                                                                               ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                               in  sem_TyExpr_NoImpls_3)) of
                                                        { ( sem_TyExpr_3) | True ->
                                                        ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) })))
                                     in  sem_TyExpr_NoImpls_2)) of
                              { ( sem_TyExpr_2) | True ->
                              ( _lhsOgUniq,sem_TyExpr_2) }) })))
              in  sem_TyExpr_NoImpls_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_Parens :: Range ->
                     T_TyExpr  ->
                     T_TyExpr 

sem_TyExpr_Parens hsrange_ tyExpr_  | hsrange_ `seq` (tyExpr_ `seq` (True)) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Parens_1 :: T_TyExpr_1 
                       sem_TyExpr_Parens_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                  (case (tyExpr_1 _tyExprOgUniq ) of
                                   { ( _tyExprIgUniq,tyExpr_2) | True ->
                                       (case (_tyExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_TyExpr_Parens_2 :: T_TyExpr_2 
                                                    sem_TyExpr_Parens_2  =
                                                        (\ _lhsItyGam ->
                                                             _lhsItyGam `seq`
                                                             ((case (_lhsItyGam) of
                                                               { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                               (case (tyExpr_2 _tyExprOtyGam ) of
                                                                { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                    (case (_tyExprIty) of
                                                                     { _lhsOty | _lhsOty `seq` (True) ->
                                                                     (case (_tyExprItyGam) of
                                                                      { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                      (case ((let sem_TyExpr_Parens_3 :: T_TyExpr_3 
                                                                                  sem_TyExpr_Parens_3  =
                                                                                      (\ _lhsIknPolCtx ->
                                                                                           _lhsIknPolCtx `seq`
                                                                                           ((case ([]) of
                                                                                             { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                             (case ((let sem_TyExpr_Parens_4 :: T_TyExpr_4 
                                                                                                         sem_TyExpr_Parens_4  =
                                                                                                             (\ _lhsIpolGam
                                                                                                                _lhsIpolVarMp ->
                                                                                                                  _lhsIpolGam `seq`
                                                                                                                  (_lhsIpolVarMp `seq`
                                                                                                                   ((case (Nothing) of
                                                                                                                     { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                     (case (_lhsIpolVarMp) of
                                                                                                                      { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                      (case (_lhsIpolGam) of
                                                                                                                       { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                       (case (_lhsIknPolCtx) of
                                                                                                                        { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                                        (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                                         { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                                             (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                              { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                  (case (_tyExprIpolVarMp) of
                                                                                                                                   { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                   (case ((let sem_TyExpr_Parens_5 :: T_TyExpr_5 
                                                                                                                                               sem_TyExpr_Parens_5  =
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
                                                                                                                                                                 (case ((let sem_TyExpr_Parens_6 :: T_TyExpr_6 
                                                                                                                                                                             sem_TyExpr_Parens_6  =
                                                                                                                                                                                 (\ _lhsIkiVarMp ->
                                                                                                                                                                                      _lhsIkiVarMp `seq`
                                                                                                                                                                                      ((case (_lhsIkiVarMp) of
                                                                                                                                                                                        { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                        (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                         { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                             (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                              { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                              (case (_tyExprIki) of
                                                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                               (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                (case (_tyExprIpol) of
                                                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                 (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                  (case ((let sem_TyExpr_Parens_7 :: T_TyExpr_7 
                                                                                                                                                                                                              sem_TyExpr_Parens_7  =
                                                                                                                                                                                                                  (\ _lhsIclGam ->
                                                                                                                                                                                                                       _lhsIclGam `seq`
                                                                                                                                                                                                                       ((case (_lhsIclGam) of
                                                                                                                                                                                                                         { _tyExprOclGam | _tyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                         (case (tyExpr_7 _tyExprOclGam ) of
                                                                                                                                                                                                                          { ( _tyExprIevTy,tyExpr_8) | True ->
                                                                                                                                                                                                                              (case (_tyExprIevTy) of
                                                                                                                                                                                                                               { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                               (case ((let sem_TyExpr_Parens_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                           sem_TyExpr_Parens_8  =
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
                                                                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                                                                               { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (ppParens _tyExprIpp) of
                                                                                                                                                                                                                                                                                 { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                                                                                                                  { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (_tyExprIerrSq) of
                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_tyExprItyWildL) of
                                                                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                       in  sem_TyExpr_Parens_8)) of
                                                                                                                                                                                                                                { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                ( _lhsOevTy,sem_TyExpr_8) }) }) }) })))
                                                                                                                                                                                                          in  sem_TyExpr_Parens_7)) of
                                                                                                                                                                                                   { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                   ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                                         in  sem_TyExpr_Parens_6)) of
                                                                                                                                                                  { ( sem_TyExpr_6) | True ->
                                                                                                                                                                  ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) })))
                                                                                                                                           in  sem_TyExpr_Parens_5)) of
                                                                                                                                    { ( sem_TyExpr_5) | True ->
                                                                                                                                    ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                                     in  sem_TyExpr_Parens_4)) of
                                                                                              { ( sem_TyExpr_4) | True ->
                                                                                              ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                              in  sem_TyExpr_Parens_3)) of
                                                                       { ( sem_TyExpr_3) | True ->
                                                                       ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) })))
                                                in  sem_TyExpr_Parens_2)) of
                                         { ( sem_TyExpr_2) | True ->
                                         ( _lhsOgUniq,sem_TyExpr_2) }) }) }) })))
                   in  sem_TyExpr_Parens_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Pred :: Range ->
                   T_PrExpr  ->
                   T_TyExpr 

sem_TyExpr_Pred hsrange_ prExpr_  | hsrange_ `seq` (prExpr_ `seq` (True)) =
    (case (prExpr_ ) of
     { ( _prExprIrange,prExpr_1) | True ->
         (case (rangeUnions [hsrange_, _prExprIrange , _prExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Pred_1 :: T_TyExpr_1 
                       sem_TyExpr_Pred_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _prExprOgUniq | _prExprOgUniq `seq` (True) ->
                                  (case (prExpr_1 _prExprOgUniq ) of
                                   { ( _prExprIgUniq,prExpr_2) | True ->
                                       (case (_prExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_TyExpr_Pred_2 :: T_TyExpr_2 
                                                    sem_TyExpr_Pred_2  =
                                                        (\ _lhsItyGam ->
                                                             _lhsItyGam `seq`
                                                             ((case (_lhsItyGam) of
                                                               { _prExprOtyGam | _prExprOtyGam `seq` (True) ->
                                                               (case (prExpr_2 _prExprOtyGam ) of
                                                                { ( _prExprIprTy,_prExprIty,_prExprItyGam,prExpr_3) | True ->
                                                                    (case (_prExprIty) of
                                                                     { _lhsOty | _lhsOty `seq` (True) ->
                                                                     (case (_prExprItyGam) of
                                                                      { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                      (case ((let sem_TyExpr_Pred_3 :: T_TyExpr_3 
                                                                                  sem_TyExpr_Pred_3  =
                                                                                      (\ _lhsIknPolCtx ->
                                                                                           _lhsIknPolCtx `seq`
                                                                                           ((case (_lhsIknPolCtx) of
                                                                                             { _prExprOknPolCtx | _prExprOknPolCtx `seq` (True) ->
                                                                                             (case (prExpr_3 _prExprOknPolCtx ) of
                                                                                              { ( _prExprIpolVarL,prExpr_4) | True ->
                                                                                                  (case (_prExprIpolVarL) of
                                                                                                   { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                   (case ((let sem_TyExpr_Pred_4 :: T_TyExpr_4 
                                                                                                               sem_TyExpr_Pred_4  =
                                                                                                                   (\ _lhsIpolGam
                                                                                                                      _lhsIpolVarMp ->
                                                                                                                        _lhsIpolGam `seq`
                                                                                                                        (_lhsIpolVarMp `seq`
                                                                                                                         ((case (Nothing) of
                                                                                                                           { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                           (case (_lhsIpolVarMp) of
                                                                                                                            { _prExprOpolVarMp | _prExprOpolVarMp `seq` (True) ->
                                                                                                                            (case (_lhsIpolGam) of
                                                                                                                             { _prExprOpolGam | _prExprOpolGam `seq` (True) ->
                                                                                                                             (case (prExpr_4 _prExprOpolGam _prExprOpolVarMp ) of
                                                                                                                              { ( _prExprIpolVarMp,prExpr_5) | True ->
                                                                                                                                  (case (_prExprIpolVarMp) of
                                                                                                                                   { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                   (case ((let sem_TyExpr_Pred_5 :: T_TyExpr_5 
                                                                                                                                               sem_TyExpr_Pred_5  =
                                                                                                                                                   (\ _lhsItyKiGam ->
                                                                                                                                                        _lhsItyKiGam `seq`
                                                                                                                                                        ((case (_lhsItyKiGam) of
                                                                                                                                                          { _prExprOtyKiGam | _prExprOtyKiGam `seq` (True) ->
                                                                                                                                                          (case (prExpr_5 _prExprOtyKiGam ) of
                                                                                                                                                           { ( _prExprIintlTyKiGam,_prExprItyKiGam,prExpr_6) | True ->
                                                                                                                                                               (case (_prExprIintlTyKiGam) of
                                                                                                                                                                { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                (case (_prExprItyKiGam) of
                                                                                                                                                                 { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                 (case ((let sem_TyExpr_Pred_6 :: T_TyExpr_6 
                                                                                                                                                                             sem_TyExpr_Pred_6  =
                                                                                                                                                                                 (\ _lhsIkiVarMp ->
                                                                                                                                                                                      _lhsIkiVarMp `seq`
                                                                                                                                                                                      ((case (_lhsIkiVarMp) of
                                                                                                                                                                                        { _prExprOkiVarMp | _prExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                        (case (prExpr_6 _prExprOkiVarMp ) of
                                                                                                                                                                                         { ( _prExprIgathTyVarPolGam,_prExprIki,_prExprIkiVarMp,_prExprItyVarWildMp,prExpr_7) | True ->
                                                                                                                                                                                             (case (_prExprIgathTyVarPolGam) of
                                                                                                                                                                                              { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                              (case (_prExprIki) of
                                                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                               (case (_prExprIkiVarMp) of
                                                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                (case (_lhsIknPolCtx) of
                                                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                 (case (_prExprItyVarWildMp) of
                                                                                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                  (case ((let sem_TyExpr_Pred_7 :: T_TyExpr_7 
                                                                                                                                                                                                              sem_TyExpr_Pred_7  =
                                                                                                                                                                                                                  (\ _lhsIclGam ->
                                                                                                                                                                                                                       _lhsIclGam `seq`
                                                                                                                                                                                                                       ((case (_lhsIclGam) of
                                                                                                                                                                                                                         { _prExprOclGam | _prExprOclGam `seq` (True) ->
                                                                                                                                                                                                                         (case (prExpr_7 _prExprOclGam ) of
                                                                                                                                                                                                                          { ( _prExprIevTy,prExpr_8) | True ->
                                                                                                                                                                                                                              (case (_prExprIevTy) of
                                                                                                                                                                                                                               { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                               (case ((let sem_TyExpr_Pred_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                           sem_TyExpr_Pred_8  =
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
                                                                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                                                                               { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (hsnOImpl >#< _prExprIpp >#< hsnCImpl) of
                                                                                                                                                                                                                                                                                 { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                                                                                                                  { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (_prExprIclMissNmS) of
                                                                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                   (case (_prExprIclNmS) of
                                                                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                    (case (_prExprIerrSq) of
                                                                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_prExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                       in  sem_TyExpr_Pred_8)) of
                                                                                                                                                                                                                                { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                ( _lhsOevTy,sem_TyExpr_8) }) }) }) })))
                                                                                                                                                                                                          in  sem_TyExpr_Pred_7)) of
                                                                                                                                                                                                   { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                   ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                                         in  sem_TyExpr_Pred_6)) of
                                                                                                                                                                  { ( sem_TyExpr_6) | True ->
                                                                                                                                                                  ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) })))
                                                                                                                                           in  sem_TyExpr_Pred_5)) of
                                                                                                                                    { ( sem_TyExpr_5) | True ->
                                                                                                                                    ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }))))
                                                                                                           in  sem_TyExpr_Pred_4)) of
                                                                                                    { ( sem_TyExpr_4) | True ->
                                                                                                    ( _lhsOpolVarL,sem_TyExpr_4) }) }) }) })))
                                                                              in  sem_TyExpr_Pred_3)) of
                                                                       { ( sem_TyExpr_3) | True ->
                                                                       ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) })))
                                                in  sem_TyExpr_Pred_2)) of
                                         { ( sem_TyExpr_2) | True ->
                                         ( _lhsOgUniq,sem_TyExpr_2) }) }) }) })))
                   in  sem_TyExpr_Pred_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Quant :: Range ->
                    TyQu ->
                    HsName ->
                    T_TyExpr  ->
                    T_TyExpr 

sem_TyExpr_Quant hsrange_ qu_ tyVar_ tyExpr_  | hsrange_ `seq` (qu_ `seq` (tyVar_ `seq` (tyExpr_ `seq` (True)))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _tyExprIrange , _tyExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Quant_1 :: T_TyExpr_1 
                       sem_TyExpr_Quant_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_ki)}} )) of
                                  { __tup230 | __tup230 `seq` (True) ->
                                  (case (__tup230) of
                                   { (_tyExprOgUniq,_,_) | _tyExprOgUniq `seq` (True) ->
                                   (case (tyExpr_1 _tyExprOgUniq ) of
                                    { ( _tyExprIgUniq,tyExpr_2) | True ->
                                        (case (_tyExprIgUniq) of
                                         { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                         (case ((let sem_TyExpr_Quant_2 :: T_TyExpr_2 
                                                     sem_TyExpr_Quant_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (__tup230) of
                                                                { (_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                (case (__tup230) of
                                                                 { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                                 (case (let  t = mkTyVar _lUniq
                                                                        in   (_lUniq,mkTGI t,TyKiGamInfo (mkNewTyVar _lUniq_ki))) of
                                                                  { __tup229 | __tup229 `seq` (True) ->
                                                                  (case (__tup229) of
                                                                   { (_,_,_tkgi_) | _tkgi_ `seq` (True) ->
                                                                   (case (__tup229) of
                                                                    { (_,_tgi_,_) | _tgi_ `seq` (True) ->
                                                                    (case (__tup229) of
                                                                     { (_tv,_,_) | _tv `seq` (True) ->
                                                                     (case (tvGathFlowIn  (tyVar_ `gamSingleton` _tgi_) _lhsItyGam) of
                                                                      { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                                      (case (tyExpr_2 _tyExprOtyGam ) of
                                                                       { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                           (case (Ty_TBind qu_ _tv
                                                                                           (tkgiKi _tkgi_)
                                                                                           _tyExprIty) of
                                                                            { _ty | _ty `seq` (True) ->
                                                                            (case (_ty) of
                                                                             { _lhsOty | _lhsOty `seq` (True) ->
                                                                             (case (tvGathFlowOut _lhsItyGam _tyExprItyGam) of
                                                                              { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                              (case ((let sem_TyExpr_Quant_3 :: T_TyExpr_3 
                                                                                          sem_TyExpr_Quant_3  =
                                                                                              (\ _lhsIknPolCtx ->
                                                                                                   _lhsIknPolCtx `seq`
                                                                                                   ((case ([]) of
                                                                                                     { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                     (case ((let sem_TyExpr_Quant_4 :: T_TyExpr_4 
                                                                                                                 sem_TyExpr_Quant_4  =
                                                                                                                     (\ _lhsIpolGam
                                                                                                                        _lhsIpolVarMp ->
                                                                                                                          _lhsIpolGam `seq`
                                                                                                                          (_lhsIpolVarMp `seq`
                                                                                                                           ((case (Nothing) of
                                                                                                                             { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                             (case (_lhsIpolVarMp) of
                                                                                                                              { _tyExprOpolVarMp | _tyExprOpolVarMp `seq` (True) ->
                                                                                                                              (case (_lhsIpolGam) of
                                                                                                                               { _tyExprOpolGam | _tyExprOpolGam `seq` (True) ->
                                                                                                                               (case (_lhsIknPolCtx) of
                                                                                                                                { _tyExprOknPolCtx | _tyExprOknPolCtx `seq` (True) ->
                                                                                                                                (case (tyExpr_3 _tyExprOknPolCtx ) of
                                                                                                                                 { ( _tyExprIpolVarL,tyExpr_4) | True ->
                                                                                                                                     (case (tyExpr_4 _tyExprOpolGam _tyExprOpolVarMp ) of
                                                                                                                                      { ( _tyExprImbStrictness,_tyExprIpolVarMp,tyExpr_5) | True ->
                                                                                                                                          (case (_tyExprIpolVarMp) of
                                                                                                                                           { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                           (case ((let sem_TyExpr_Quant_5 :: T_TyExpr_5 
                                                                                                                                                       sem_TyExpr_Quant_5  =
                                                                                                                                                           (\ _lhsItyKiGam ->
                                                                                                                                                                _lhsItyKiGam `seq`
                                                                                                                                                                ((case (tgiTy _tgi_ `tyKiGamSingleton` _tkgi_) of
                                                                                                                                                                  { _tyKiGamNew | _tyKiGamNew `seq` (True) ->
                                                                                                                                                                  (case (tvGathFlowIn  _tyKiGamNew _lhsItyKiGam) of
                                                                                                                                                                   { _tyExprOtyKiGam | _tyExprOtyKiGam `seq` (True) ->
                                                                                                                                                                   (case (tyExpr_5 _tyExprOtyKiGam ) of
                                                                                                                                                                    { ( _tyExprIintlTyKiGam,_tyExprItyKiGam,tyExpr_6) | True ->
                                                                                                                                                                        (case (gamUnions [_tyKiGamNew,_tyExprIintlTyKiGam]) of
                                                                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                         (case (tvGathFlowOut _lhsItyKiGam _tyExprItyKiGam) of
                                                                                                                                                                          { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                          (case ((let sem_TyExpr_Quant_6 :: T_TyExpr_6 
                                                                                                                                                                                      sem_TyExpr_Quant_6  =
                                                                                                                                                                                          (\ _lhsIkiVarMp ->
                                                                                                                                                                                               _lhsIkiVarMp `seq`
                                                                                                                                                                                               ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                 { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                                 (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                                  { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                                      (case (_tyExprIgathTyVarPolGam) of
                                                                                                                                                                                                       { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                       (case (_tyExprIki) of
                                                                                                                                                                                                        { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                                        (case (_tyExprIkiVarMp) of
                                                                                                                                                                                                         { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                         (case (_tyExprIpol) of
                                                                                                                                                                                                          { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                                          (case (_tyExprItyVarWildMp) of
                                                                                                                                                                                                           { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                           (case ((let sem_TyExpr_Quant_7 :: T_TyExpr_7 
                                                                                                                                                                                                                       sem_TyExpr_Quant_7  =
                                                                                                                                                                                                                           (\ _lhsIclGam ->
                                                                                                                                                                                                                                _lhsIclGam `seq`
                                                                                                                                                                                                                                ((case (_ty) of
                                                                                                                                                                                                                                  { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                                  (case ((let sem_TyExpr_Quant_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                              sem_TyExpr_Quant_8  =
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
                                                                                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                                                                                        { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (hsnUnknown) of
                                                                                                                                                                                                                                                                                         { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (showTyQu qu_ >#< tyVar_ >#< "." >#< _tyExprIpp) of
                                                                                                                                                                                                                                                                                          { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_pp) of
                                                                                                                                                                                                                                                                                           { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                            { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_tyExprIclNmS) of
                                                                                                                                                                                                                                                                                             { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_tyExprIerrSq) of
                                                                                                                                                                                                                                                                                              { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                               { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (_pp) of
                                                                                                                                                                                                                                                                                                { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (_tyExprItyWildL) of
                                                                                                                                                                                                                                                                                                 { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                                          in  sem_TyExpr_Quant_8)) of
                                                                                                                                                                                                                                   { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                                   ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                                                                   in  sem_TyExpr_Quant_7)) of
                                                                                                                                                                                                            { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                            ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                                                  in  sem_TyExpr_Quant_6)) of
                                                                                                                                                                           { ( sem_TyExpr_6) | True ->
                                                                                                                                                                           ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) }) })))
                                                                                                                                                   in  sem_TyExpr_Quant_5)) of
                                                                                                                                            { ( sem_TyExpr_5) | True ->
                                                                                                                                            ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                                             in  sem_TyExpr_Quant_4)) of
                                                                                                      { ( sem_TyExpr_4) | True ->
                                                                                                      ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                                      in  sem_TyExpr_Quant_3)) of
                                                                               { ( sem_TyExpr_3) | True ->
                                                                               ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) }) }) }) }) }) })))
                                                 in  sem_TyExpr_Quant_2)) of
                                          { ( sem_TyExpr_2) | True ->
                                          ( _lhsOgUniq,sem_TyExpr_2) }) }) }) }) })))
                   in  sem_TyExpr_Quant_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Row :: Range ->
                  T_RowTyExpr  ->
                  T_TyExpr 

sem_TyExpr_Row hsrange_ rowTyExpr_  | hsrange_ `seq` (rowTyExpr_ `seq` (True)) =
    (case (rowTyExpr_ ) of
     { ( _rowTyExprIrange,rowTyExpr_1) | True ->
         (case (rangeUnions [hsrange_, _rowTyExprIrange
                                                     , _rowTyExprIrange
                                                                    ]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_TyExpr_Row_1 :: T_TyExpr_1 
                       sem_TyExpr_Row_1  =
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
                                         (case ((let sem_TyExpr_Row_2 :: T_TyExpr_2 
                                                     sem_TyExpr_Row_2  =
                                                         (\ _lhsItyGam ->
                                                              _lhsItyGam `seq`
                                                              ((case (_lhsItyGam) of
                                                                { _rowTyExprOtyGam | _rowTyExprOtyGam `seq` (True) ->
                                                                (case (rowTyExpr_2 _rowTyExprOtyGam ) of
                                                                 { ( _rowTyExprItyGam,_rowTyExprItyRow,rowTyExpr_3) | True ->
                                                                     (case (_rowTyExprItyRow) of
                                                                      { _ty | _ty `seq` (True) ->
                                                                      (case (_ty) of
                                                                       { _lhsOty | _lhsOty `seq` (True) ->
                                                                       (case (_rowTyExprItyGam) of
                                                                        { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                        (case ((let sem_TyExpr_Row_3 :: T_TyExpr_3 
                                                                                    sem_TyExpr_Row_3  =
                                                                                        (\ _lhsIknPolCtx ->
                                                                                             _lhsIknPolCtx `seq`
                                                                                             ((case ([]) of
                                                                                               { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                               (case ((let sem_TyExpr_Row_4 :: T_TyExpr_4 
                                                                                                           sem_TyExpr_Row_4  =
                                                                                                               (\ _lhsIpolGam
                                                                                                                  _lhsIpolVarMp ->
                                                                                                                    _lhsIpolGam `seq`
                                                                                                                    (_lhsIpolVarMp `seq`
                                                                                                                     ((case (Nothing) of
                                                                                                                       { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                                       (case (_lhsIpolVarMp) of
                                                                                                                        { _rowTyExprOpolVarMp | _rowTyExprOpolVarMp `seq` (True) ->
                                                                                                                        (case (_lhsIpolGam) of
                                                                                                                         { _rowTyExprOpolGam | _rowTyExprOpolGam `seq` (True) ->
                                                                                                                         (case (_lhsIknPolCtx) of
                                                                                                                          { _rowTyExprOknPolCtx | _rowTyExprOknPolCtx `seq` (True) ->
                                                                                                                          (case (rowTyExpr_3 _rowTyExprOknPolCtx _rowTyExprOpolGam _rowTyExprOpolVarMp ) of
                                                                                                                           { ( _rowTyExprIpolVarMp,rowTyExpr_4) | True ->
                                                                                                                               (case (_rowTyExprIpolVarMp) of
                                                                                                                                { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                (case ((let sem_TyExpr_Row_5 :: T_TyExpr_5 
                                                                                                                                            sem_TyExpr_Row_5  =
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
                                                                                                                                                              (case ((let sem_TyExpr_Row_6 :: T_TyExpr_6 
                                                                                                                                                                          sem_TyExpr_Row_6  =
                                                                                                                                                                              (\ _lhsIkiVarMp ->
                                                                                                                                                                                   _lhsIkiVarMp `seq`
                                                                                                                                                                                   ((case (emptyGam) of
                                                                                                                                                                                     { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                     (case (kiRow) of
                                                                                                                                                                                      { _ki | _ki `seq` (True) ->
                                                                                                                                                                                      (case (_ki) of
                                                                                                                                                                                       { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                                       (case (_lhsIkiVarMp) of
                                                                                                                                                                                        { _rowTyExprOkiVarMp | _rowTyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                        (case (rowTyExpr_5 _rowTyExprOkiVarMp ) of
                                                                                                                                                                                         { ( _rowTyExprIkiVarMp,_rowTyExprItyVarWildMp,rowTyExpr_6) | True ->
                                                                                                                                                                                             (case (_rowTyExprIkiVarMp) of
                                                                                                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                              (case (_lhsIknPolCtx) of
                                                                                                                                                                                               { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                                               (case (_rowTyExprItyVarWildMp) of
                                                                                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                (case ((let sem_TyExpr_Row_7 :: T_TyExpr_7 
                                                                                                                                                                                                            sem_TyExpr_Row_7  =
                                                                                                                                                                                                                (\ _lhsIclGam ->
                                                                                                                                                                                                                     _lhsIclGam `seq`
                                                                                                                                                                                                                     ((case (_ty) of
                                                                                                                                                                                                                       { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                                                       (case ((let sem_TyExpr_Row_8 :: T_TyExpr_8 
                                                                                                                                                                                                                                   sem_TyExpr_Row_8  =
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
                                                                                                                                                                                                                                                                       (case (reverse _rowTyExprIppL) of
                                                                                                                                                                                                                                                                        { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                                                                        (case (_rowTyExprIextNm) of
                                                                                                                                                                                                                                                                         { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                                                         (case (_rowTyExprIpp) of
                                                                                                                                                                                                                                                                          { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                                                          (case (_rowTyExprIclMissNmS) of
                                                                                                                                                                                                                                                                           { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                           (case (_rowTyExprIclNmS) of
                                                                                                                                                                                                                                                                            { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                            (case (_rowTyExprIerrSq) of
                                                                                                                                                                                                                                                                             { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                             (case (_rowTyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                              (case (ppAppTop  (hsnRow,mkPPAppFun hsnRow _rowTyExprIpp)
                                                                                                                                                                                                                                                                                               (reverse _rowTyExprIppL) _rowTyExprIpp) of
                                                                                                                                                                                                                                                                               { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_pp) of
                                                                                                                                                                                                                                                                                { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case ([]) of
                                                                                                                                                                                                                                                                                 { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                                               in  sem_TyExpr_Row_8)) of
                                                                                                                                                                                                                        { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                                                        ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                                                        in  sem_TyExpr_Row_7)) of
                                                                                                                                                                                                 { ( sem_TyExpr_7) | True ->
                                                                                                                                                                                                 ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                      in  sem_TyExpr_Row_6)) of
                                                                                                                                                               { ( sem_TyExpr_6) | True ->
                                                                                                                                                               ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) })))
                                                                                                                                        in  sem_TyExpr_Row_5)) of
                                                                                                                                 { ( sem_TyExpr_5) | True ->
                                                                                                                                 ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }))))
                                                                                                       in  sem_TyExpr_Row_4)) of
                                                                                                { ( sem_TyExpr_4) | True ->
                                                                                                ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                                in  sem_TyExpr_Row_3)) of
                                                                         { ( sem_TyExpr_3) | True ->
                                                                         ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) })))
                                                 in  sem_TyExpr_Row_2)) of
                                          { ( sem_TyExpr_2) | True ->
                                          ( _lhsOgUniq,sem_TyExpr_2) }) }) }) }) })))
                   in  sem_TyExpr_Row_1)) of
            { ( sem_TyExpr_1) | True ->
            ( _lhsOrange,sem_TyExpr_1) }) }) }) })

sem_TyExpr_Var :: Range ->
                  HsName ->
                  T_TyExpr 

sem_TyExpr_Var hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_Var_1 :: T_TyExpr_1 
                  sem_TyExpr_Var_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_17_fitsIn_pol) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_17_fitsIn_pol,lUniq_ki)}}} )) of
                             { __tup235 | __tup235 `seq` (True) ->
                             (case (__tup235) of
                              { (_lhsOgUniq,_,_,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_Var_2 :: T_TyExpr_2 
                                          sem_TyExpr_Var_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup235) of
                                                     { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                     (case (tyGamLookupOrAdd _lUniq nm_ _lhsItyGam _lhsItyGam) of
                                                      { __tup232 | __tup232 `seq` (True) ->
                                                      (case (__tup232) of
                                                       { (_tgi_,_) | _tgi_ `seq` (True) ->
                                                       (case (tgiTy _tgi_) of
                                                        { _ty | _ty `seq` (True) ->
                                                        (case (_ty) of
                                                         { _lhsOty | _lhsOty `seq` (True) ->
                                                         (case (__tup232) of
                                                          { (_,_lhsOtyGam) | _lhsOtyGam `seq` (True) ->
                                                          (case ((let sem_TyExpr_Var_3 :: T_TyExpr_3 
                                                                      sem_TyExpr_Var_3  =
                                                                          (\ _lhsIknPolCtx ->
                                                                               _lhsIknPolCtx `seq`
                                                                               ((case ([_lhsIknPolCtx]) of
                                                                                 { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                 (case ((let sem_TyExpr_Var_4 :: T_TyExpr_4 
                                                                                             sem_TyExpr_Var_4  =
                                                                                                 (\ _lhsIpolGam
                                                                                                    _lhsIpolVarMp ->
                                                                                                      _lhsIpolGam `seq`
                                                                                                      (_lhsIpolVarMp `seq`
                                                                                                       ((case (Nothing) of
                                                                                                         { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                         (case (__tup235) of
                                                                                                          { (_,_,_lUniq_17_fitsIn_pol,_) | _lUniq_17_fitsIn_pol `seq` (True) ->
                                                                                                          (case (polGamLookupErr nm_ _lhsIpolGam) of
                                                                                                           { __tup234 | __tup234 `seq` (True) ->
                                                                                                           (case (__tup234) of
                                                                                                            { (_pgi_,_) | _pgi_ `seq` (True) ->
                                                                                                            (case (pgiPol _pgi_) of
                                                                                                             { _polFromEnv | _polFromEnv `seq` (True) ->
                                                                                                             (case (fitsIn weakFIOpts defaultFIEnv _lUniq_17_fitsIn_pol _lhsIpolVarMp _polFromEnv _lhsIknPolCtx) of
                                                                                                              { _foPol | _foPol `seq` (True) ->
                                                                                                              (case (foVarMp _foPol `varUpd` _lhsIpolVarMp) of
                                                                                                               { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                               (case ((let sem_TyExpr_Var_5 :: T_TyExpr_5 
                                                                                                                           sem_TyExpr_Var_5  =
                                                                                                                               (\ _lhsItyKiGam ->
                                                                                                                                    _lhsItyKiGam `seq`
                                                                                                                                    ((case (__tup235) of
                                                                                                                                      { (_,_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                                                                                      (case (tyKiGamLookupOrAdd _lUniq_ki (tgiTy _tgi_) _lhsItyKiGam) of
                                                                                                                                       { __tup233 | __tup233 `seq` (True) ->
                                                                                                                                       (case (__tup233) of
                                                                                                                                        { (_,_,_tyKiGamNew) | _tyKiGamNew `seq` (True) ->
                                                                                                                                        (case (_tyKiGamNew) of
                                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                         (case (__tup233) of
                                                                                                                                          { (_,_lhsOtyKiGam,_) | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                          (case ((let sem_TyExpr_Var_6 :: T_TyExpr_6 
                                                                                                                                                      sem_TyExpr_Var_6  =
                                                                                                                                                          (\ _lhsIkiVarMp ->
                                                                                                                                                               _lhsIkiVarMp `seq`
                                                                                                                                                               ((case (gamSingleton nm_ (mkPGI _lhsIknPolCtx)) of
                                                                                                                                                                 { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                 (case (__tup233) of
                                                                                                                                                                  { (_tkgi_,_,_) | _tkgi_ `seq` (True) ->
                                                                                                                                                                  (case (tkgiKi _tkgi_) of
                                                                                                                                                                   { _ki | _ki `seq` (True) ->
                                                                                                                                                                   (case (_ki) of
                                                                                                                                                                    { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                                    (case (_lhsIkiVarMp) of
                                                                                                                                                                     { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                     (case (foTy _foPol) of
                                                                                                                                                                      { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                      (case (Map.empty) of
                                                                                                                                                                       { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                       (case ((let sem_TyExpr_Var_7 :: T_TyExpr_7 
                                                                                                                                                                                   sem_TyExpr_Var_7  =
                                                                                                                                                                                       (\ _lhsIclGam ->
                                                                                                                                                                                            _lhsIclGam `seq`
                                                                                                                                                                                            ((case (_ty) of
                                                                                                                                                                                              { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                              (case ((let sem_TyExpr_Var_8 :: T_TyExpr_8 
                                                                                                                                                                                                          sem_TyExpr_Var_8  =
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
                                                                                                                                                                                                                            ((case (Seq.empty) of
                                                                                                                                                                                                                              { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                               { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                                                                { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                                (case (pp nm_) of
                                                                                                                                                                                                                                 { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                                                                  { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                                  (case (Set.empty) of
                                                                                                                                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                   (case (Set.empty) of
                                                                                                                                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                    (case (rngLift _range mkNestErr' _pp [foErrSq _foPol    ]) of
                                                                                                                                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                     (case (Map.empty) of
                                                                                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                                        { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                        ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                      in  sem_TyExpr_Var_8)) of
                                                                                                                                                                                               { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                               ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                               in  sem_TyExpr_Var_7)) of
                                                                                                                                                                        { ( sem_TyExpr_7) | True ->
                                                                                                                                                                        ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                                  in  sem_TyExpr_Var_6)) of
                                                                                                                                           { ( sem_TyExpr_6) | True ->
                                                                                                                                           ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) }) })))
                                                                                                                       in  sem_TyExpr_Var_5)) of
                                                                                                                { ( sem_TyExpr_5) | True ->
                                                                                                                ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }) }) }) }) }) }))))
                                                                                         in  sem_TyExpr_Var_4)) of
                                                                                  { ( sem_TyExpr_4) | True ->
                                                                                  ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                  in  sem_TyExpr_Var_3)) of
                                                           { ( sem_TyExpr_3) | True ->
                                                           ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) })))
                                      in  sem_TyExpr_Var_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_Var_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_VarWild :: Range ->
                      HsName ->
                      T_TyExpr 

sem_TyExpr_VarWild hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_VarWild_1 :: T_TyExpr_1 
                  sem_TyExpr_VarWild_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_ki)}} )) of
                             { __tup239 | __tup239 `seq` (True) ->
                             (case (__tup239) of
                              { (_lhsOgUniq,_,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_VarWild_2 :: T_TyExpr_2 
                                          sem_TyExpr_VarWild_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup239) of
                                                     { (_,_lUniq,_) | _lUniq `seq` (True) ->
                                                     (case (tyGamLookupOrAdd _lUniq nm_ _lhsItyGam _lhsItyGam) of
                                                      { __tup237 | __tup237 `seq` (True) ->
                                                      (case (__tup237) of
                                                       { (_tgi_,_) | _tgi_ `seq` (True) ->
                                                       (case (tgiTy _tgi_) of
                                                        { _ty | _ty `seq` (True) ->
                                                        (case (_ty) of
                                                         { _lhsOty | _lhsOty `seq` (True) ->
                                                         (case (__tup237) of
                                                          { (_,_lhsOtyGam) | _lhsOtyGam `seq` (True) ->
                                                          (case ((let sem_TyExpr_VarWild_3 :: T_TyExpr_3 
                                                                      sem_TyExpr_VarWild_3  =
                                                                          (\ _lhsIknPolCtx ->
                                                                               _lhsIknPolCtx `seq`
                                                                               ((case ([]) of
                                                                                 { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                 (case ((let sem_TyExpr_VarWild_4 :: T_TyExpr_4 
                                                                                             sem_TyExpr_VarWild_4  =
                                                                                                 (\ _lhsIpolGam
                                                                                                    _lhsIpolVarMp ->
                                                                                                      _lhsIpolGam `seq`
                                                                                                      (_lhsIpolVarMp `seq`
                                                                                                       ((case (Nothing) of
                                                                                                         { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                         (case (_lhsIpolVarMp) of
                                                                                                          { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                          (case ((let sem_TyExpr_VarWild_5 :: T_TyExpr_5 
                                                                                                                      sem_TyExpr_VarWild_5  =
                                                                                                                          (\ _lhsItyKiGam ->
                                                                                                                               _lhsItyKiGam `seq`
                                                                                                                               ((case (__tup239) of
                                                                                                                                 { (_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                                                                                 (case (tyKiGamLookupOrAdd _lUniq_ki (tgiTy _tgi_) _lhsItyKiGam) of
                                                                                                                                  { __tup238 | __tup238 `seq` (True) ->
                                                                                                                                  (case (__tup238) of
                                                                                                                                   { (_,_,_tyKiGamNew) | _tyKiGamNew `seq` (True) ->
                                                                                                                                   (case (_tyKiGamNew) of
                                                                                                                                    { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                    (case (__tup238) of
                                                                                                                                     { (_,_lhsOtyKiGam,_) | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                     (case ((let sem_TyExpr_VarWild_6 :: T_TyExpr_6 
                                                                                                                                                 sem_TyExpr_VarWild_6  =
                                                                                                                                                     (\ _lhsIkiVarMp ->
                                                                                                                                                          _lhsIkiVarMp `seq`
                                                                                                                                                          ((case (emptyGam) of
                                                                                                                                                            { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                            (case (__tup238) of
                                                                                                                                                             { (_tkgi_,_,_) | _tkgi_ `seq` (True) ->
                                                                                                                                                             (case (tkgiKi _tkgi_) of
                                                                                                                                                              { _ki | _ki `seq` (True) ->
                                                                                                                                                              (case (_ki) of
                                                                                                                                                               { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                               (case (_lhsIkiVarMp) of
                                                                                                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                (case (_lhsIknPolCtx) of
                                                                                                                                                                 { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                                 (case (_lUniq) of
                                                                                                                                                                  { _tyVarId | _tyVarId `seq` (True) ->
                                                                                                                                                                  (case (Map.singleton _tyVarId TyVarWild_NoQuantTyExpr_YesQuantLetBinding) of
                                                                                                                                                                   { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                   (case ((let sem_TyExpr_VarWild_7 :: T_TyExpr_7 
                                                                                                                                                                               sem_TyExpr_VarWild_7  =
                                                                                                                                                                                   (\ _lhsIclGam ->
                                                                                                                                                                                        _lhsIclGam `seq`
                                                                                                                                                                                        ((case (_ty) of
                                                                                                                                                                                          { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                          (case ((let sem_TyExpr_VarWild_8 :: T_TyExpr_8 
                                                                                                                                                                                                      sem_TyExpr_VarWild_8  =
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
                                                                                                                                                                                                                        ((case (Seq.empty) of
                                                                                                                                                                                                                          { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                          (case ([]) of
                                                                                                                                                                                                                           { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                           (case (hsnUnknown) of
                                                                                                                                                                                                                            { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                            (case ("%" >|< pp nm_) of
                                                                                                                                                                                                                             { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                             (case (_pp) of
                                                                                                                                                                                                                              { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                              (case (Set.empty) of
                                                                                                                                                                                                                               { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                               (case (Set.empty) of
                                                                                                                                                                                                                                { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                (case (Seq.empty) of
                                                                                                                                                                                                                                 { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                 (case (Map.empty) of
                                                                                                                                                                                                                                  { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                  (case (_pp) of
                                                                                                                                                                                                                                   { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                   (case ([]) of
                                                                                                                                                                                                                                    { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                    ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                                  in  sem_TyExpr_VarWild_8)) of
                                                                                                                                                                                           { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                           ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                           in  sem_TyExpr_VarWild_7)) of
                                                                                                                                                                    { ( sem_TyExpr_7) | True ->
                                                                                                                                                                    ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) }) })))
                                                                                                                                             in  sem_TyExpr_VarWild_6)) of
                                                                                                                                      { ( sem_TyExpr_6) | True ->
                                                                                                                                      ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) }) }) }) })))
                                                                                                                  in  sem_TyExpr_VarWild_5)) of
                                                                                                           { ( sem_TyExpr_5) | True ->
                                                                                                           ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }))))
                                                                                         in  sem_TyExpr_VarWild_4)) of
                                                                                  { ( sem_TyExpr_4) | True ->
                                                                                  ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                  in  sem_TyExpr_VarWild_3)) of
                                                           { ( sem_TyExpr_3) | True ->
                                                           ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) })))
                                      in  sem_TyExpr_VarWild_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_VarWild_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })

sem_TyExpr_Wild :: Range ->
                   T_TyExpr 

sem_TyExpr_Wild hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyExpr_Wild_1 :: T_TyExpr_1 
                  sem_TyExpr_Wild_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup240 | __tup240 `seq` (True) ->
                             (case (__tup240) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyExpr_Wild_2 :: T_TyExpr_2 
                                          sem_TyExpr_Wild_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup240) of
                                                     { (_,_lUniq) | _lUniq `seq` (True) ->
                                                     (case (_lUniq) of
                                                      { _tyVarId | _tyVarId `seq` (True) ->
                                                      (case (mkNewTyVar _tyVarId) of
                                                       { _tvarv | _tvarv `seq` (True) ->
                                                       (case (mkTGI _tvarv) of
                                                        { _tgi_ | _tgi_ `seq` (True) ->
                                                        (case (tgiTy _tgi_) of
                                                         { _ty | _ty `seq` (True) ->
                                                         (case (_ty) of
                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                          (case (_lhsItyGam) of
                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                           (case ((let sem_TyExpr_Wild_3 :: T_TyExpr_3 
                                                                       sem_TyExpr_Wild_3  =
                                                                           (\ _lhsIknPolCtx ->
                                                                                _lhsIknPolCtx `seq`
                                                                                ((case ([]) of
                                                                                  { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                  (case ((let sem_TyExpr_Wild_4 :: T_TyExpr_4 
                                                                                              sem_TyExpr_Wild_4  =
                                                                                                  (\ _lhsIpolGam
                                                                                                     _lhsIpolVarMp ->
                                                                                                       _lhsIpolGam `seq`
                                                                                                       (_lhsIpolVarMp `seq`
                                                                                                        ((case (Nothing) of
                                                                                                          { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
                                                                                                          (case (_lhsIpolVarMp) of
                                                                                                           { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                           (case ((let sem_TyExpr_Wild_5 :: T_TyExpr_5 
                                                                                                                       sem_TyExpr_Wild_5  =
                                                                                                                           (\ _lhsItyKiGam ->
                                                                                                                                _lhsItyKiGam `seq`
                                                                                                                                ((case (emptyGam) of
                                                                                                                                  { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                  (case (_lhsItyKiGam) of
                                                                                                                                   { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                   (case ((let sem_TyExpr_Wild_6 :: T_TyExpr_6 
                                                                                                                                               sem_TyExpr_Wild_6  =
                                                                                                                                                   (\ _lhsIkiVarMp ->
                                                                                                                                                        _lhsIkiVarMp `seq`
                                                                                                                                                        ((case (emptyGam) of
                                                                                                                                                          { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                          (case (TyKiGamInfo _tvarv) of
                                                                                                                                                           { _tkgi_ | _tkgi_ `seq` (True) ->
                                                                                                                                                           (case (tkgiKi _tkgi_) of
                                                                                                                                                            { _ki | _ki `seq` (True) ->
                                                                                                                                                            (case (_ki) of
                                                                                                                                                             { _lhsOki | _lhsOki `seq` (True) ->
                                                                                                                                                             (case (_lhsIkiVarMp) of
                                                                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                              (case (_lhsIknPolCtx) of
                                                                                                                                                               { _lhsOpol | _lhsOpol `seq` (True) ->
                                                                                                                                                               (case (Map.singleton _tyVarId TyVarWild_NoQuantTyExpr_YesQuantLetBinding) of
                                                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                (case ((let sem_TyExpr_Wild_7 :: T_TyExpr_7 
                                                                                                                                                                            sem_TyExpr_Wild_7  =
                                                                                                                                                                                (\ _lhsIclGam ->
                                                                                                                                                                                     _lhsIclGam `seq`
                                                                                                                                                                                     ((case (_ty) of
                                                                                                                                                                                       { _lhsOevTy | _lhsOevTy `seq` (True) ->
                                                                                                                                                                                       (case ((let sem_TyExpr_Wild_8 :: T_TyExpr_8 
                                                                                                                                                                                                   sem_TyExpr_Wild_8  =
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
                                                                                                                                                                                                                     ((case (Seq.empty) of
                                                                                                                                                                                                                       { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                       (case ([]) of
                                                                                                                                                                                                                        { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                                                                                        (case (hsnUnknown) of
                                                                                                                                                                                                                         { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                                                                                         (case (pp "...") of
                                                                                                                                                                                                                          { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                          (case (_pp) of
                                                                                                                                                                                                                           { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                                                                                           (case (Set.empty) of
                                                                                                                                                                                                                            { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                            (case (Set.empty) of
                                                                                                                                                                                                                             { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                             (case (Seq.empty) of
                                                                                                                                                                                                                              { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                              (case (Map.empty) of
                                                                                                                                                                                                                               { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                               (case (_pp) of
                                                                                                                                                                                                                                { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                (case ([]) of
                                                                                                                                                                                                                                 { _lhsOtyWildL | _lhsOtyWildL `seq` (True) ->
                                                                                                                                                                                                                                 ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOtyWildL) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                                                                                                               in  sem_TyExpr_Wild_8)) of
                                                                                                                                                                                        { ( sem_TyExpr_8) | True ->
                                                                                                                                                                                        ( _lhsOevTy,sem_TyExpr_8) }) })))
                                                                                                                                                                        in  sem_TyExpr_Wild_7)) of
                                                                                                                                                                 { ( sem_TyExpr_7) | True ->
                                                                                                                                                                 ( _lhsOgathTyVarPolGam,_lhsOki,_lhsOkiVarMp,_lhsOpol,_lhsOtyVarWildMp,sem_TyExpr_7) }) }) }) }) }) }) }) })))
                                                                                                                                           in  sem_TyExpr_Wild_6)) of
                                                                                                                                    { ( sem_TyExpr_6) | True ->
                                                                                                                                    ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExpr_6) }) }) })))
                                                                                                                   in  sem_TyExpr_Wild_5)) of
                                                                                                            { ( sem_TyExpr_5) | True ->
                                                                                                            ( _lhsOmbStrictness,_lhsOpolVarMp,sem_TyExpr_5) }) }) }))))
                                                                                          in  sem_TyExpr_Wild_4)) of
                                                                                   { ( sem_TyExpr_4) | True ->
                                                                                   ( _lhsOpolVarL,sem_TyExpr_4) }) })))
                                                                   in  sem_TyExpr_Wild_3)) of
                                                            { ( sem_TyExpr_3) | True ->
                                                            ( _lhsOty,_lhsOtyGam,sem_TyExpr_3) }) }) }) }) }) }) }) })))
                                      in  sem_TyExpr_Wild_2)) of
                               { ( sem_TyExpr_2) | True ->
                               ( _lhsOgUniq,sem_TyExpr_2) }) }) })))
              in  sem_TyExpr_Wild_1)) of
       { ( sem_TyExpr_1) | True ->
       ( _lhsOrange,sem_TyExpr_1) }) }) })


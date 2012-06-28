


module EH101.EH.MainAG_KiExpr where

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

-- KiExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      chained attribute:
         kiGam                : KiGam
      synthesized attribute:
         ki                   : Ty
   visit 3:
      inherited attributes:
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         finTyVarMp           : VarMp
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
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         pp                   : PP_Doc
   alternatives:
      alternative Ann:
         child hsrange        : {Range}
         child ann            : KiExprAnn 
         child kiExpr         : KiExpr 
         visit 0:
            local range       : {Range}
         visit 3:
            local pp          : _
      alternative App:
         child hsrange        : {Range}
         child func           : KiExpr 
         child arg            : KiExpr 
         visit 0:
            local range       : {Range}
         visit 3:
            local pp          : _
      alternative AppTop:
         child hsrange        : {Range}
         child kiExpr         : KiExpr 
         visit 0:
            local range       : {Range}
         visit 3:
            local pp          : _
      alternative Con:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            intra range       : {Range}
         visit 2:
            local _tup164     : {(KiGamInfo,ErrL)}
            local kgi         : {KiGamInfo}
            intra range       : {Range}
         visit 3:
            local pp          : _
            local nmErrs      : {ErrL}
            intra _tup164     : {(KiGamInfo,ErrL)}
            intra range       : {Range}
      alternative Parens:
         child hsrange        : {Range}
         child kiExpr         : KiExpr 
         visit 0:
            local range       : {Range}
         visit 3:
            local pp          : _
      alternative Var:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup166     : {(UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local _tup165     : {(KiGamInfo,KiGam)}
            local kgi         : {KiGamInfo}
            intra _tup166     : {(UID,UID)}
         visit 3:
            local pp          : _
-}
sem_KiExpr_Ann :: Range ->
                  T_KiExprAnn  ->
                  T_KiExpr  ->
                  T_KiExpr 

sem_KiExpr_Ann hsrange_ ann_ kiExpr_  | hsrange_ `seq` (ann_ `seq` (kiExpr_ `seq` (True))) =
    (case (kiExpr_ ) of
     { ( _kiExprIrange,kiExpr_1) | True ->
         (case (rangeUnions [hsrange_, _kiExprIrange , _kiExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_KiExpr_Ann_1 :: T_KiExpr_1 
                       sem_KiExpr_Ann_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _kiExprOgUniq | _kiExprOgUniq `seq` (True) ->
                                  (case (kiExpr_1 _kiExprOgUniq ) of
                                   { ( _kiExprIgUniq,kiExpr_2) | True ->
                                       (case (_kiExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_KiExpr_Ann_2 :: T_KiExpr_2 
                                                    sem_KiExpr_Ann_2  =
                                                        (\ _lhsIkiGam ->
                                                             _lhsIkiGam `seq`
                                                             ((case (_lhsIkiGam) of
                                                               { _kiExprOkiGam | _kiExprOkiGam `seq` (True) ->
                                                               (case (kiExpr_2 _kiExprOkiGam ) of
                                                                { ( _kiExprIki,_kiExprIkiGam,kiExpr_3) | True ->
                                                                    (case (_kiExprIki) of
                                                                     { _lhsOki | _lhsOki `seq` (True) ->
                                                                     (case (_kiExprIkiGam) of
                                                                      { _lhsOkiGam | _lhsOkiGam `seq` (True) ->
                                                                      (case ((let sem_KiExpr_Ann_3 :: T_KiExpr_3 
                                                                                  sem_KiExpr_Ann_3  =
                                                                                      (\ _lhsIfinKiVarMp
                                                                                         _lhsIfinTyKiGam
                                                                                         _lhsIfinTyVarMp
                                                                                         _lhsImoduleNm
                                                                                         _lhsIopts
                                                                                         _lhsItyKiGlobFreeTvarS
                                                                                         _lhsItyTyGlobFreeTvarS
                                                                                         _lhsItyTyTySigFreeTvarS
                                                                                         _lhsIvalTyGlobFreeTvarS ->
                                                                                           _lhsIfinKiVarMp `seq`
                                                                                           (_lhsIfinTyKiGam `seq`
                                                                                            (_lhsIfinTyVarMp `seq`
                                                                                             (_lhsImoduleNm `seq`
                                                                                              (_lhsIopts `seq`
                                                                                               (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                 (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                   ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                     { _kiExprOvalTyGlobFreeTvarS | _kiExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                      { _kiExprOtyTyTySigFreeTvarS | _kiExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                       { _kiExprOtyTyGlobFreeTvarS | _kiExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                        { _kiExprOtyKiGlobFreeTvarS | _kiExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                        (case (_lhsIopts) of
                                                                                                         { _kiExprOopts | _kiExprOopts `seq` (True) ->
                                                                                                         (case (_lhsImoduleNm) of
                                                                                                          { _kiExprOmoduleNm | _kiExprOmoduleNm `seq` (True) ->
                                                                                                          (case (_lhsIfinTyVarMp) of
                                                                                                           { _kiExprOfinTyVarMp | _kiExprOfinTyVarMp `seq` (True) ->
                                                                                                           (case (_lhsIfinTyKiGam) of
                                                                                                            { _kiExprOfinTyKiGam | _kiExprOfinTyKiGam `seq` (True) ->
                                                                                                            (case (_lhsIfinKiVarMp) of
                                                                                                             { _kiExprOfinKiVarMp | _kiExprOfinKiVarMp `seq` (True) ->
                                                                                                             (case (kiExpr_3 _kiExprOfinKiVarMp _kiExprOfinTyKiGam _kiExprOfinTyVarMp _kiExprOmoduleNm _kiExprOopts _kiExprOtyKiGlobFreeTvarS _kiExprOtyTyGlobFreeTvarS _kiExprOtyTyTySigFreeTvarS _kiExprOvalTyGlobFreeTvarS ) of
                                                                                                              { ( _kiExprIallErrSq,_kiExprIappArgPPL,_kiExprIappFunNm,_kiExprIappFunPP,_kiExprIerrSq,_kiExprIgathMentrelFilterMp,_kiExprIpp) | True ->
                                                                                                                  (case (_kiExprIallErrSq) of
                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                   (case ([]) of
                                                                                                                    { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                    (case (hsnUnknown) of
                                                                                                                     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                     (case (_kiExprIpp) of
                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                      (case (_pp) of
                                                                                                                       { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                       (case (_kiExprIerrSq) of
                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                        (case (_kiExprIgathMentrelFilterMp) of
                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                         (case (_pp) of
                                                                                                                          { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                          ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                              in  sem_KiExpr_Ann_3)) of
                                                                       { ( sem_KiExpr_3) | True ->
                                                                       ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) })))
                                                in  sem_KiExpr_Ann_2)) of
                                         { ( sem_KiExpr_2) | True ->
                                         ( _lhsOgUniq,sem_KiExpr_2) }) }) }) })))
                   in  sem_KiExpr_Ann_1)) of
            { ( sem_KiExpr_1) | True ->
            ( _lhsOrange,sem_KiExpr_1) }) }) }) })

sem_KiExpr_App :: Range ->
                  T_KiExpr  ->
                  T_KiExpr  ->
                  T_KiExpr 

sem_KiExpr_App hsrange_ func_ arg_  | hsrange_ `seq` (func_ `seq` (arg_ `seq` (True))) =
    (case (arg_ ) of
     { ( _argIrange,arg_1) | True ->
         (case (func_ ) of
          { ( _funcIrange,func_1) | True ->
              (case (rangeUnions [hsrange_, _funcIrange   , _argIrange   ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_KiExpr_App_1 :: T_KiExpr_1 
                            sem_KiExpr_App_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (_lhsIgUniq) of
                                       { _funcOgUniq | _funcOgUniq `seq` (True) ->
                                       (case (func_1 _funcOgUniq ) of
                                        { ( _funcIgUniq,func_2) | True ->
                                            (case (_funcIgUniq) of
                                             { _argOgUniq | _argOgUniq `seq` (True) ->
                                             (case (arg_1 _argOgUniq ) of
                                              { ( _argIgUniq,arg_2) | True ->
                                                  (case (_argIgUniq) of
                                                   { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                   (case ((let sem_KiExpr_App_2 :: T_KiExpr_2 
                                                               sem_KiExpr_App_2  =
                                                                   (\ _lhsIkiGam ->
                                                                        _lhsIkiGam `seq`
                                                                        ((case (_lhsIkiGam) of
                                                                          { _funcOkiGam | _funcOkiGam `seq` (True) ->
                                                                          (case (func_2 _funcOkiGam ) of
                                                                           { ( _funcIki,_funcIkiGam,func_3) | True ->
                                                                               (case (_funcIkiGam) of
                                                                                { _argOkiGam | _argOkiGam `seq` (True) ->
                                                                                (case (arg_2 _argOkiGam ) of
                                                                                 { ( _argIki,_argIkiGam,arg_3) | True ->
                                                                                     (case (Ty_App _funcIki _argIki) of
                                                                                      { _lhsOki | _lhsOki `seq` (True) ->
                                                                                      (case (_argIkiGam) of
                                                                                       { _lhsOkiGam | _lhsOkiGam `seq` (True) ->
                                                                                       (case ((let sem_KiExpr_App_3 :: T_KiExpr_3 
                                                                                                   sem_KiExpr_App_3  =
                                                                                                       (\ _lhsIfinKiVarMp
                                                                                                          _lhsIfinTyKiGam
                                                                                                          _lhsIfinTyVarMp
                                                                                                          _lhsImoduleNm
                                                                                                          _lhsIopts
                                                                                                          _lhsItyKiGlobFreeTvarS
                                                                                                          _lhsItyTyGlobFreeTvarS
                                                                                                          _lhsItyTyTySigFreeTvarS
                                                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                                                            _lhsIfinKiVarMp `seq`
                                                                                                            (_lhsIfinTyKiGam `seq`
                                                                                                             (_lhsIfinTyVarMp `seq`
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
                                                                                                                           (case (_lhsIfinTyVarMp) of
                                                                                                                            { _argOfinTyVarMp | _argOfinTyVarMp `seq` (True) ->
                                                                                                                            (case (_lhsIfinTyKiGam) of
                                                                                                                             { _argOfinTyKiGam | _argOfinTyKiGam `seq` (True) ->
                                                                                                                             (case (_lhsIfinKiVarMp) of
                                                                                                                              { _argOfinKiVarMp | _argOfinKiVarMp `seq` (True) ->
                                                                                                                              (case (arg_3 _argOfinKiVarMp _argOfinTyKiGam _argOfinTyVarMp _argOmoduleNm _argOopts _argOtyKiGlobFreeTvarS _argOtyTyGlobFreeTvarS _argOtyTyTySigFreeTvarS _argOvalTyGlobFreeTvarS ) of
                                                                                                                               { ( _argIallErrSq,_argIappArgPPL,_argIappFunNm,_argIappFunPP,_argIerrSq,_argIgathMentrelFilterMp,_argIpp) | True ->
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
                                                                                                                                         (case (_lhsIfinTyVarMp) of
                                                                                                                                          { _funcOfinTyVarMp | _funcOfinTyVarMp `seq` (True) ->
                                                                                                                                          (case (_lhsIfinTyKiGam) of
                                                                                                                                           { _funcOfinTyKiGam | _funcOfinTyKiGam `seq` (True) ->
                                                                                                                                           (case (_lhsIfinKiVarMp) of
                                                                                                                                            { _funcOfinKiVarMp | _funcOfinKiVarMp `seq` (True) ->
                                                                                                                                            (case (func_3 _funcOfinKiVarMp _funcOfinTyKiGam _funcOfinTyVarMp _funcOmoduleNm _funcOopts _funcOtyKiGlobFreeTvarS _funcOtyTyGlobFreeTvarS _funcOtyTyTySigFreeTvarS _funcOvalTyGlobFreeTvarS ) of
                                                                                                                                             { ( _funcIallErrSq,_funcIappArgPPL,_funcIappFunNm,_funcIappFunPP,_funcIerrSq,_funcIgathMentrelFilterMp,_funcIpp) | True ->
                                                                                                                                                 (case (_funcIallErrSq `Seq.union` _argIallErrSq) of
                                                                                                                                                  { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                  (case (_funcIappArgPPL ++ [_argIpp]) of
                                                                                                                                                   { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                                                   (case (_funcIappFunNm) of
                                                                                                                                                    { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                                                    (case (_funcIappFunPP) of
                                                                                                                                                     { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                                                     (case (_funcIerrSq `Seq.union` _argIerrSq) of
                                                                                                                                                      { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                      (case (_funcIgathMentrelFilterMp `mentrelFilterMpUnion` _argIgathMentrelFilterMp) of
                                                                                                                                                       { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                       (case (_funcIpp >#< _argIpp) of
                                                                                                                                                        { _pp | _pp `seq` (True) ->
                                                                                                                                                        (case (_pp) of
                                                                                                                                                         { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                         ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                                               in  sem_KiExpr_App_3)) of
                                                                                        { ( sem_KiExpr_3) | True ->
                                                                                        ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) }) }) })))
                                                           in  sem_KiExpr_App_2)) of
                                                    { ( sem_KiExpr_2) | True ->
                                                    ( _lhsOgUniq,sem_KiExpr_2) }) }) }) }) }) })))
                        in  sem_KiExpr_App_1)) of
                 { ( sem_KiExpr_1) | True ->
                 ( _lhsOrange,sem_KiExpr_1) }) }) }) }) })

sem_KiExpr_AppTop :: Range ->
                     T_KiExpr  ->
                     T_KiExpr 

sem_KiExpr_AppTop hsrange_ kiExpr_  | hsrange_ `seq` (kiExpr_ `seq` (True)) =
    (case (kiExpr_ ) of
     { ( _kiExprIrange,kiExpr_1) | True ->
         (case (rangeUnions [hsrange_, _kiExprIrange , _kiExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_KiExpr_AppTop_1 :: T_KiExpr_1 
                       sem_KiExpr_AppTop_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _kiExprOgUniq | _kiExprOgUniq `seq` (True) ->
                                  (case (kiExpr_1 _kiExprOgUniq ) of
                                   { ( _kiExprIgUniq,kiExpr_2) | True ->
                                       (case (_kiExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_KiExpr_AppTop_2 :: T_KiExpr_2 
                                                    sem_KiExpr_AppTop_2  =
                                                        (\ _lhsIkiGam ->
                                                             _lhsIkiGam `seq`
                                                             ((case (_lhsIkiGam) of
                                                               { _kiExprOkiGam | _kiExprOkiGam `seq` (True) ->
                                                               (case (kiExpr_2 _kiExprOkiGam ) of
                                                                { ( _kiExprIki,_kiExprIkiGam,kiExpr_3) | True ->
                                                                    (case (_kiExprIki) of
                                                                     { _lhsOki | _lhsOki `seq` (True) ->
                                                                     (case (_kiExprIkiGam) of
                                                                      { _lhsOkiGam | _lhsOkiGam `seq` (True) ->
                                                                      (case ((let sem_KiExpr_AppTop_3 :: T_KiExpr_3 
                                                                                  sem_KiExpr_AppTop_3  =
                                                                                      (\ _lhsIfinKiVarMp
                                                                                         _lhsIfinTyKiGam
                                                                                         _lhsIfinTyVarMp
                                                                                         _lhsImoduleNm
                                                                                         _lhsIopts
                                                                                         _lhsItyKiGlobFreeTvarS
                                                                                         _lhsItyTyGlobFreeTvarS
                                                                                         _lhsItyTyTySigFreeTvarS
                                                                                         _lhsIvalTyGlobFreeTvarS ->
                                                                                           _lhsIfinKiVarMp `seq`
                                                                                           (_lhsIfinTyKiGam `seq`
                                                                                            (_lhsIfinTyVarMp `seq`
                                                                                             (_lhsImoduleNm `seq`
                                                                                              (_lhsIopts `seq`
                                                                                               (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                 (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                   ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                     { _kiExprOvalTyGlobFreeTvarS | _kiExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                      { _kiExprOtyTyTySigFreeTvarS | _kiExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                       { _kiExprOtyTyGlobFreeTvarS | _kiExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                        { _kiExprOtyKiGlobFreeTvarS | _kiExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                        (case (_lhsIopts) of
                                                                                                         { _kiExprOopts | _kiExprOopts `seq` (True) ->
                                                                                                         (case (_lhsImoduleNm) of
                                                                                                          { _kiExprOmoduleNm | _kiExprOmoduleNm `seq` (True) ->
                                                                                                          (case (_lhsIfinTyVarMp) of
                                                                                                           { _kiExprOfinTyVarMp | _kiExprOfinTyVarMp `seq` (True) ->
                                                                                                           (case (_lhsIfinTyKiGam) of
                                                                                                            { _kiExprOfinTyKiGam | _kiExprOfinTyKiGam `seq` (True) ->
                                                                                                            (case (_lhsIfinKiVarMp) of
                                                                                                             { _kiExprOfinKiVarMp | _kiExprOfinKiVarMp `seq` (True) ->
                                                                                                             (case (kiExpr_3 _kiExprOfinKiVarMp _kiExprOfinTyKiGam _kiExprOfinTyVarMp _kiExprOmoduleNm _kiExprOopts _kiExprOtyKiGlobFreeTvarS _kiExprOtyTyGlobFreeTvarS _kiExprOtyTyTySigFreeTvarS _kiExprOvalTyGlobFreeTvarS ) of
                                                                                                              { ( _kiExprIallErrSq,_kiExprIappArgPPL,_kiExprIappFunNm,_kiExprIappFunPP,_kiExprIerrSq,_kiExprIgathMentrelFilterMp,_kiExprIpp) | True ->
                                                                                                                  (case (_kiExprIallErrSq) of
                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                   (case ([]) of
                                                                                                                    { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                    (case (hsnUnknown) of
                                                                                                                     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                     (case (ppAppTop  (_kiExprIappFunNm,_kiExprIappFunPP)
                                                                                                                                      _kiExprIappArgPPL _kiExprIpp) of
                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                      (case (_pp) of
                                                                                                                       { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                       (case (_kiExprIerrSq) of
                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                        (case (_kiExprIgathMentrelFilterMp) of
                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                         (case (_pp) of
                                                                                                                          { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                          ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                              in  sem_KiExpr_AppTop_3)) of
                                                                       { ( sem_KiExpr_3) | True ->
                                                                       ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) })))
                                                in  sem_KiExpr_AppTop_2)) of
                                         { ( sem_KiExpr_2) | True ->
                                         ( _lhsOgUniq,sem_KiExpr_2) }) }) }) })))
                   in  sem_KiExpr_AppTop_1)) of
            { ( sem_KiExpr_1) | True ->
            ( _lhsOrange,sem_KiExpr_1) }) }) }) })

sem_KiExpr_Con :: Range ->
                  HsName ->
                  T_KiExpr 

sem_KiExpr_Con hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_KiExpr_Con_1 :: T_KiExpr_1 
                  sem_KiExpr_Con_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (_lhsIgUniq) of
                             { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                             (case ((let sem_KiExpr_Con_2 :: T_KiExpr_2 
                                         sem_KiExpr_Con_2  =
                                             (\ _lhsIkiGam ->
                                                  _lhsIkiGam `seq`
                                                  ((case (case gamLookup nm_ _lhsIkiGam of
                                                            Nothing    ->  (KiGamInfo Ty_Any,[rngLift _range mkErr_NamesNotIntrod "kind" [nm_]])
                                                            Just kgi   ->  (kgi,[])) of
                                                    { __tup164 | __tup164 `seq` (True) ->
                                                    (case (__tup164) of
                                                     { (_kgi,_) | _kgi `seq` (True) ->
                                                     (case (kgiKi _kgi) of
                                                      { _lhsOki | _lhsOki `seq` (True) ->
                                                      (case (_lhsIkiGam) of
                                                       { _lhsOkiGam | _lhsOkiGam `seq` (True) ->
                                                       (case ((let sem_KiExpr_Con_3 :: T_KiExpr_3 
                                                                   sem_KiExpr_Con_3  =
                                                                       (\ _lhsIfinKiVarMp
                                                                          _lhsIfinTyKiGam
                                                                          _lhsIfinTyVarMp
                                                                          _lhsImoduleNm
                                                                          _lhsIopts
                                                                          _lhsItyKiGlobFreeTvarS
                                                                          _lhsItyTyGlobFreeTvarS
                                                                          _lhsItyTyTySigFreeTvarS
                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                            _lhsIfinKiVarMp `seq`
                                                                            (_lhsIfinTyKiGam `seq`
                                                                             (_lhsIfinTyVarMp `seq`
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
                                                                                        (case (pp nm_) of
                                                                                         { _pp | _pp `seq` (True) ->
                                                                                         (case (_pp) of
                                                                                          { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                          (case (__tup164) of
                                                                                           { (_,_nmErrs) | _nmErrs `seq` (True) ->
                                                                                           (case (rngLift _range mkNestErr' _pp [Seq.fromList _nmErrs]) of
                                                                                            { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                            (case (Map.empty) of
                                                                                             { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                             (case (_pp) of
                                                                                              { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                              ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) }) })))))))))))
                                                               in  sem_KiExpr_Con_3)) of
                                                        { ( sem_KiExpr_3) | True ->
                                                        ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) })))
                                     in  sem_KiExpr_Con_2)) of
                              { ( sem_KiExpr_2) | True ->
                              ( _lhsOgUniq,sem_KiExpr_2) }) })))
              in  sem_KiExpr_Con_1)) of
       { ( sem_KiExpr_1) | True ->
       ( _lhsOrange,sem_KiExpr_1) }) }) })

sem_KiExpr_Parens :: Range ->
                     T_KiExpr  ->
                     T_KiExpr 

sem_KiExpr_Parens hsrange_ kiExpr_  | hsrange_ `seq` (kiExpr_ `seq` (True)) =
    (case (kiExpr_ ) of
     { ( _kiExprIrange,kiExpr_1) | True ->
         (case (rangeUnions [hsrange_, _kiExprIrange , _kiExprIrange]) of
          { _range | _range `seq` (True) ->
          (case (_range) of
           { _lhsOrange | _lhsOrange `seq` (True) ->
           (case ((let sem_KiExpr_Parens_1 :: T_KiExpr_1 
                       sem_KiExpr_Parens_1  =
                           (\ _lhsIgUniq ->
                                _lhsIgUniq `seq`
                                ((case (_lhsIgUniq) of
                                  { _kiExprOgUniq | _kiExprOgUniq `seq` (True) ->
                                  (case (kiExpr_1 _kiExprOgUniq ) of
                                   { ( _kiExprIgUniq,kiExpr_2) | True ->
                                       (case (_kiExprIgUniq) of
                                        { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                        (case ((let sem_KiExpr_Parens_2 :: T_KiExpr_2 
                                                    sem_KiExpr_Parens_2  =
                                                        (\ _lhsIkiGam ->
                                                             _lhsIkiGam `seq`
                                                             ((case (_lhsIkiGam) of
                                                               { _kiExprOkiGam | _kiExprOkiGam `seq` (True) ->
                                                               (case (kiExpr_2 _kiExprOkiGam ) of
                                                                { ( _kiExprIki,_kiExprIkiGam,kiExpr_3) | True ->
                                                                    (case (_kiExprIki) of
                                                                     { _lhsOki | _lhsOki `seq` (True) ->
                                                                     (case (_kiExprIkiGam) of
                                                                      { _lhsOkiGam | _lhsOkiGam `seq` (True) ->
                                                                      (case ((let sem_KiExpr_Parens_3 :: T_KiExpr_3 
                                                                                  sem_KiExpr_Parens_3  =
                                                                                      (\ _lhsIfinKiVarMp
                                                                                         _lhsIfinTyKiGam
                                                                                         _lhsIfinTyVarMp
                                                                                         _lhsImoduleNm
                                                                                         _lhsIopts
                                                                                         _lhsItyKiGlobFreeTvarS
                                                                                         _lhsItyTyGlobFreeTvarS
                                                                                         _lhsItyTyTySigFreeTvarS
                                                                                         _lhsIvalTyGlobFreeTvarS ->
                                                                                           _lhsIfinKiVarMp `seq`
                                                                                           (_lhsIfinTyKiGam `seq`
                                                                                            (_lhsIfinTyVarMp `seq`
                                                                                             (_lhsImoduleNm `seq`
                                                                                              (_lhsIopts `seq`
                                                                                               (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                 (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                   ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                     { _kiExprOvalTyGlobFreeTvarS | _kiExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                      { _kiExprOtyTyTySigFreeTvarS | _kiExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                       { _kiExprOtyTyGlobFreeTvarS | _kiExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                        { _kiExprOtyKiGlobFreeTvarS | _kiExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                        (case (_lhsIopts) of
                                                                                                         { _kiExprOopts | _kiExprOopts `seq` (True) ->
                                                                                                         (case (_lhsImoduleNm) of
                                                                                                          { _kiExprOmoduleNm | _kiExprOmoduleNm `seq` (True) ->
                                                                                                          (case (_lhsIfinTyVarMp) of
                                                                                                           { _kiExprOfinTyVarMp | _kiExprOfinTyVarMp `seq` (True) ->
                                                                                                           (case (_lhsIfinTyKiGam) of
                                                                                                            { _kiExprOfinTyKiGam | _kiExprOfinTyKiGam `seq` (True) ->
                                                                                                            (case (_lhsIfinKiVarMp) of
                                                                                                             { _kiExprOfinKiVarMp | _kiExprOfinKiVarMp `seq` (True) ->
                                                                                                             (case (kiExpr_3 _kiExprOfinKiVarMp _kiExprOfinTyKiGam _kiExprOfinTyVarMp _kiExprOmoduleNm _kiExprOopts _kiExprOtyKiGlobFreeTvarS _kiExprOtyTyGlobFreeTvarS _kiExprOtyTyTySigFreeTvarS _kiExprOvalTyGlobFreeTvarS ) of
                                                                                                              { ( _kiExprIallErrSq,_kiExprIappArgPPL,_kiExprIappFunNm,_kiExprIappFunPP,_kiExprIerrSq,_kiExprIgathMentrelFilterMp,_kiExprIpp) | True ->
                                                                                                                  (case (_kiExprIallErrSq) of
                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                   (case ([]) of
                                                                                                                    { _lhsOappArgPPL | _lhsOappArgPPL `seq` (True) ->
                                                                                                                    (case (hsnUnknown) of
                                                                                                                     { _lhsOappFunNm | _lhsOappFunNm `seq` (True) ->
                                                                                                                     (case (ppParens _kiExprIpp) of
                                                                                                                      { _pp | _pp `seq` (True) ->
                                                                                                                      (case (_pp) of
                                                                                                                       { _lhsOappFunPP | _lhsOappFunPP `seq` (True) ->
                                                                                                                       (case (_kiExprIerrSq) of
                                                                                                                        { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                        (case (_kiExprIgathMentrelFilterMp) of
                                                                                                                         { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                         (case (_pp) of
                                                                                                                          { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                          ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                              in  sem_KiExpr_Parens_3)) of
                                                                       { ( sem_KiExpr_3) | True ->
                                                                       ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) })))
                                                in  sem_KiExpr_Parens_2)) of
                                         { ( sem_KiExpr_2) | True ->
                                         ( _lhsOgUniq,sem_KiExpr_2) }) }) }) })))
                   in  sem_KiExpr_Parens_1)) of
            { ( sem_KiExpr_1) | True ->
            ( _lhsOrange,sem_KiExpr_1) }) }) }) })

sem_KiExpr_Var :: Range ->
                  HsName ->
                  T_KiExpr 

sem_KiExpr_Var hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_KiExpr_Var_1 :: T_KiExpr_1 
                  sem_KiExpr_Var_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                             { __tup166 | __tup166 `seq` (True) ->
                             (case (__tup166) of
                              { (_lhsOgUniq,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_KiExpr_Var_2 :: T_KiExpr_2 
                                          sem_KiExpr_Var_2  =
                                              (\ _lhsIkiGam ->
                                                   _lhsIkiGam `seq`
                                                   ((case (__tup166) of
                                                     { (_,_lUniq) | _lUniq `seq` (True) ->
                                                     (case (case gamLookup nm_ _lhsIkiGam of
                                                              Nothing    ->  let  t    =  mkNewTyVar _lUniq
                                                                                  kgi  =  KiGamInfo t
                                                                             in   (kgi,gamAdd nm_ kgi _lhsIkiGam)
                                                              Just kgi   ->  (kgi,_lhsIkiGam)) of
                                                      { __tup165 | __tup165 `seq` (True) ->
                                                      (case (__tup165) of
                                                       { (_kgi,_) | _kgi `seq` (True) ->
                                                       (case (kgiKi _kgi) of
                                                        { _lhsOki | _lhsOki `seq` (True) ->
                                                        (case (__tup165) of
                                                         { (_,_lhsOkiGam) | _lhsOkiGam `seq` (True) ->
                                                         (case ((let sem_KiExpr_Var_3 :: T_KiExpr_3 
                                                                     sem_KiExpr_Var_3  =
                                                                         (\ _lhsIfinKiVarMp
                                                                            _lhsIfinTyKiGam
                                                                            _lhsIfinTyVarMp
                                                                            _lhsImoduleNm
                                                                            _lhsIopts
                                                                            _lhsItyKiGlobFreeTvarS
                                                                            _lhsItyTyGlobFreeTvarS
                                                                            _lhsItyTyTySigFreeTvarS
                                                                            _lhsIvalTyGlobFreeTvarS ->
                                                                              _lhsIfinKiVarMp `seq`
                                                                              (_lhsIfinTyKiGam `seq`
                                                                               (_lhsIfinTyVarMp `seq`
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
                                                                                            (case (Seq.empty) of
                                                                                             { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                             (case (Map.empty) of
                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                              (case (_pp) of
                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                               ( _lhsOallErrSq,_lhsOappArgPPL,_lhsOappFunNm,_lhsOappFunPP,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp) }) }) }) }) }) }) }) })))))))))))
                                                                 in  sem_KiExpr_Var_3)) of
                                                          { ( sem_KiExpr_3) | True ->
                                                          ( _lhsOki,_lhsOkiGam,sem_KiExpr_3) }) }) }) }) }) })))
                                      in  sem_KiExpr_Var_2)) of
                               { ( sem_KiExpr_2) | True ->
                               ( _lhsOgUniq,sem_KiExpr_2) }) }) })))
              in  sem_KiExpr_Var_1)) of
       { ( sem_KiExpr_1) | True ->
       ( _lhsOrange,sem_KiExpr_1) }) }) })


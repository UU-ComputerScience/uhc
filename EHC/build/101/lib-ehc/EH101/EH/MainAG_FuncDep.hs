


module EH101.EH.MainAG_FuncDep where

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

-- FuncDep -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         gUniq                : UID
   visit 1:
      chained attribute:
         tyGam                : TyGam
   visit 2:
      chained attribute:
         tyKiGam              : TyKiGam
   visit 3:
      inherited attributes:
         clsTyArgs            : TyL
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
         errSq                : ErrSq
         funcDeps             : [ClsFuncDep]
         gathMentrelFilterMp  : ModEntRelFilterMp
         pp                   : PP_Doc
         range                : Range
   alternatives:
      alternative Dep:
         child hsrange        : {Range}
         child fromTvs        : TyVars 
         child toTvs          : TyVars 
         visit 3:
            local range       : {Range}
-}
sem_FuncDep_Dep :: Range ->
                   T_TyVars  ->
                   T_TyVars  ->
                   T_FuncDep 

sem_FuncDep_Dep hsrange_ fromTvs_ toTvs_  | hsrange_ `seq` (fromTvs_ `seq` (toTvs_ `seq` (True))) =
    (\ _lhsIgUniq ->
         _lhsIgUniq `seq`
         ((case (_lhsIgUniq) of
           { _fromTvsOgUniq | _fromTvsOgUniq `seq` (True) ->
           (case (fromTvs_ ) of
            { ( _fromTvsIrange,fromTvs_1) | True ->
                (case (fromTvs_1 _fromTvsOgUniq ) of
                 { ( _fromTvsIgUniq,fromTvs_2) | True ->
                     (case (_fromTvsIgUniq) of
                      { _toTvsOgUniq | _toTvsOgUniq `seq` (True) ->
                      (case (toTvs_ ) of
                       { ( _toTvsIrange,toTvs_1) | True ->
                           (case (toTvs_1 _toTvsOgUniq ) of
                            { ( _toTvsIgUniq,toTvs_2) | True ->
                                (case (_toTvsIgUniq) of
                                 { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                 (case ((let sem_FuncDep_Dep_1 :: T_FuncDep_1 
                                             sem_FuncDep_Dep_1  =
                                                 (\ _lhsItyGam ->
                                                      _lhsItyGam `seq`
                                                      ((case (_lhsItyGam) of
                                                        { _fromTvsOtyGam | _fromTvsOtyGam `seq` (True) ->
                                                        (case (fromTvs_2 _fromTvsOtyGam ) of
                                                         { ( _fromTvsItyGam,fromTvs_3) | True ->
                                                             (case (_fromTvsItyGam) of
                                                              { _toTvsOtyGam | _toTvsOtyGam `seq` (True) ->
                                                              (case (toTvs_2 _toTvsOtyGam ) of
                                                               { ( _toTvsItyGam,toTvs_3) | True ->
                                                                   (case (_toTvsItyGam) of
                                                                    { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                    (case ((let sem_FuncDep_Dep_2 :: T_FuncDep_2 
                                                                                sem_FuncDep_Dep_2  =
                                                                                    (\ _lhsItyKiGam ->
                                                                                         _lhsItyKiGam `seq`
                                                                                         ((case (_lhsItyKiGam) of
                                                                                           { _fromTvsOtyKiGam | _fromTvsOtyKiGam `seq` (True) ->
                                                                                           (case (fromTvs_3 _fromTvsOtyKiGam ) of
                                                                                            { ( _fromTvsIgathTyVarPolGam,_fromTvsIintlTyKiGam,_fromTvsIkiL,_fromTvsIpolVarL,_fromTvsItyKiGam,_fromTvsItyL,fromTvs_4) | True ->
                                                                                                (case (_fromTvsItyKiGam) of
                                                                                                 { _toTvsOtyKiGam | _toTvsOtyKiGam `seq` (True) ->
                                                                                                 (case (toTvs_3 _toTvsOtyKiGam ) of
                                                                                                  { ( _toTvsIgathTyVarPolGam,_toTvsIintlTyKiGam,_toTvsIkiL,_toTvsIpolVarL,_toTvsItyKiGam,_toTvsItyL,toTvs_4) | True ->
                                                                                                      (case (_toTvsItyKiGam) of
                                                                                                       { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                       (case ((let sem_FuncDep_Dep_3 :: T_FuncDep_3 
                                                                                                                   sem_FuncDep_Dep_3  =
                                                                                                                       (\ _lhsIclsTyArgs
                                                                                                                          _lhsIfinKiVarMp
                                                                                                                          _lhsIfinTyKiGam
                                                                                                                          _lhsIfinTyVarMp
                                                                                                                          _lhsImoduleNm
                                                                                                                          _lhsIopts
                                                                                                                          _lhsItyKiGlobFreeTvarS
                                                                                                                          _lhsItyTyGlobFreeTvarS
                                                                                                                          _lhsItyTyTySigFreeTvarS
                                                                                                                          _lhsIvalTyGlobFreeTvarS ->
                                                                                                                            _lhsIclsTyArgs `seq`
                                                                                                                            (_lhsIfinKiVarMp `seq`
                                                                                                                             (_lhsIfinTyKiGam `seq`
                                                                                                                              (_lhsIfinTyVarMp `seq`
                                                                                                                               (_lhsImoduleNm `seq`
                                                                                                                                (_lhsIopts `seq`
                                                                                                                                 (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                  (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                   (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                    (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                     ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                       { _toTvsOvalTyGlobFreeTvarS | _toTvsOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                       (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                        { _toTvsOtyTyTySigFreeTvarS | _toTvsOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                        (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                         { _toTvsOtyTyGlobFreeTvarS | _toTvsOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                         (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                          { _toTvsOtyKiGlobFreeTvarS | _toTvsOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                          (case (_lhsIopts) of
                                                                                                                                           { _toTvsOopts | _toTvsOopts `seq` (True) ->
                                                                                                                                           (case (_lhsImoduleNm) of
                                                                                                                                            { _toTvsOmoduleNm | _toTvsOmoduleNm `seq` (True) ->
                                                                                                                                            (case (_lhsIfinTyVarMp) of
                                                                                                                                             { _toTvsOfinTyVarMp | _toTvsOfinTyVarMp `seq` (True) ->
                                                                                                                                             (case (_lhsIfinTyKiGam) of
                                                                                                                                              { _toTvsOfinTyKiGam | _toTvsOfinTyKiGam `seq` (True) ->
                                                                                                                                              (case (_lhsIfinKiVarMp) of
                                                                                                                                               { _toTvsOfinKiVarMp | _toTvsOfinKiVarMp `seq` (True) ->
                                                                                                                                               (case (toTvs_4 _toTvsOfinKiVarMp _toTvsOfinTyKiGam _toTvsOfinTyVarMp _toTvsOmoduleNm _toTvsOopts _toTvsOtyKiGlobFreeTvarS _toTvsOtyTyGlobFreeTvarS _toTvsOtyTyTySigFreeTvarS _toTvsOvalTyGlobFreeTvarS ) of
                                                                                                                                                { ( _toTvsIallErrSq,_toTvsIerrSq,_toTvsIgathMentrelFilterMp,_toTvsInmL,_toTvsIpp,_toTvsIppL) | True ->
                                                                                                                                                    (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                     { _fromTvsOvalTyGlobFreeTvarS | _fromTvsOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                      { _fromTvsOtyTyTySigFreeTvarS | _fromTvsOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                       { _fromTvsOtyTyGlobFreeTvarS | _fromTvsOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                        { _fromTvsOtyKiGlobFreeTvarS | _fromTvsOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                        (case (_lhsIopts) of
                                                                                                                                                         { _fromTvsOopts | _fromTvsOopts `seq` (True) ->
                                                                                                                                                         (case (_lhsImoduleNm) of
                                                                                                                                                          { _fromTvsOmoduleNm | _fromTvsOmoduleNm `seq` (True) ->
                                                                                                                                                          (case (_lhsIfinTyVarMp) of
                                                                                                                                                           { _fromTvsOfinTyVarMp | _fromTvsOfinTyVarMp `seq` (True) ->
                                                                                                                                                           (case (_lhsIfinTyKiGam) of
                                                                                                                                                            { _fromTvsOfinTyKiGam | _fromTvsOfinTyKiGam `seq` (True) ->
                                                                                                                                                            (case (_lhsIfinKiVarMp) of
                                                                                                                                                             { _fromTvsOfinKiVarMp | _fromTvsOfinKiVarMp `seq` (True) ->
                                                                                                                                                             (case (fromTvs_4 _fromTvsOfinKiVarMp _fromTvsOfinTyKiGam _fromTvsOfinTyVarMp _fromTvsOmoduleNm _fromTvsOopts _fromTvsOtyKiGlobFreeTvarS _fromTvsOtyTyGlobFreeTvarS _fromTvsOtyTyTySigFreeTvarS _fromTvsOvalTyGlobFreeTvarS ) of
                                                                                                                                                              { ( _fromTvsIallErrSq,_fromTvsIerrSq,_fromTvsIgathMentrelFilterMp,_fromTvsInmL,_fromTvsIpp,_fromTvsIppL) | True ->
                                                                                                                                                                  (case (_fromTvsIallErrSq `Seq.union` _toTvsIallErrSq) of
                                                                                                                                                                   { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                   (case (_fromTvsIerrSq `Seq.union` _toTvsIerrSq) of
                                                                                                                                                                    { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                    (case (let  l v = maybe (-1) id . elemIndex v $ _lhsIclsTyArgs
                                                                                                                                                                           in   [ClsFuncDep (map l _fromTvsItyL) (map l _toTvsItyL)]) of
                                                                                                                                                                     { _lhsOfuncDeps | _lhsOfuncDeps `seq` (True) ->
                                                                                                                                                                     (case (_fromTvsIgathMentrelFilterMp `mentrelFilterMpUnion` _toTvsIgathMentrelFilterMp) of
                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                      (case (_fromTvsIpp >#< "->" >#< _toTvsIpp) of
                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                       (case (rangeUnions [hsrange_, _fromTvsIrange, _toTvsIrange ]) of
                                                                                                                                                                        { _range | _range `seq` (True) ->
                                                                                                                                                                        (case (_range) of
                                                                                                                                                                         { _lhsOrange | _lhsOrange `seq` (True) ->
                                                                                                                                                                         ( _lhsOallErrSq,_lhsOerrSq,_lhsOfuncDeps,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOrange) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                               in  sem_FuncDep_Dep_3)) of
                                                                                                        { ( sem_FuncDep_3) | True ->
                                                                                                        ( _lhsOtyKiGam,sem_FuncDep_3) }) }) }) }) }) })))
                                                                            in  sem_FuncDep_Dep_2)) of
                                                                     { ( sem_FuncDep_2) | True ->
                                                                     ( _lhsOtyGam,sem_FuncDep_2) }) }) }) }) }) })))
                                         in  sem_FuncDep_Dep_1)) of
                                  { ( sem_FuncDep_1) | True ->
                                  ( _lhsOgUniq,sem_FuncDep_1) }) }) }) }) }) }) }) })))


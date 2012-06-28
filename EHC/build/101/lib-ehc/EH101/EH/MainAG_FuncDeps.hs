


module EH101.EH.MainAG_FuncDeps where

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

-- FuncDeps ----------------------------------------------------
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
         ppL                  : [PP_Doc]
         range                : Range
   alternatives:
      alternative Cons:
         child hd             : FuncDep 
         child tl             : FuncDeps 
      alternative Nil:
-}
sem_FuncDeps_Cons :: T_FuncDep  ->
                     T_FuncDeps  ->
                     T_FuncDeps 

sem_FuncDeps_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (\ _lhsIgUniq ->
         _lhsIgUniq `seq`
         ((case (_lhsIgUniq) of
           { _hdOgUniq | _hdOgUniq `seq` (True) ->
           (case (hd_ _hdOgUniq ) of
            { ( _hdIgUniq,hd_1) | True ->
                (case (_hdIgUniq) of
                 { _tlOgUniq | _tlOgUniq `seq` (True) ->
                 (case (tl_ _tlOgUniq ) of
                  { ( _tlIgUniq,tl_1) | True ->
                      (case (_tlIgUniq) of
                       { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                       (case ((let sem_FuncDeps_Cons_1 :: T_FuncDeps_1 
                                   sem_FuncDeps_Cons_1  =
                                       (\ _lhsItyGam ->
                                            _lhsItyGam `seq`
                                            ((case (_lhsItyGam) of
                                              { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                              (case (hd_1 _hdOtyGam ) of
                                               { ( _hdItyGam,hd_2) | True ->
                                                   (case (_hdItyGam) of
                                                    { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                    (case (tl_1 _tlOtyGam ) of
                                                     { ( _tlItyGam,tl_2) | True ->
                                                         (case (_tlItyGam) of
                                                          { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                          (case ((let sem_FuncDeps_Cons_2 :: T_FuncDeps_2 
                                                                      sem_FuncDeps_Cons_2  =
                                                                          (\ _lhsItyKiGam ->
                                                                               _lhsItyKiGam `seq`
                                                                               ((case (_lhsItyKiGam) of
                                                                                 { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                 (case (hd_2 _hdOtyKiGam ) of
                                                                                  { ( _hdItyKiGam,hd_3) | True ->
                                                                                      (case (_hdItyKiGam) of
                                                                                       { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                       (case (tl_2 _tlOtyKiGam ) of
                                                                                        { ( _tlItyKiGam,tl_3) | True ->
                                                                                            (case (_tlItyKiGam) of
                                                                                             { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                             (case ((let sem_FuncDeps_Cons_3 :: T_FuncDeps_3 
                                                                                                         sem_FuncDeps_Cons_3  =
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
                                                                                                                             { _tlOvalTyGlobFreeTvarS | _tlOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                             (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                              { _tlOtyTyTySigFreeTvarS | _tlOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                              (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                               { _tlOtyTyGlobFreeTvarS | _tlOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                               (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                { _tlOtyKiGlobFreeTvarS | _tlOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                (case (_lhsIopts) of
                                                                                                                                 { _tlOopts | _tlOopts `seq` (True) ->
                                                                                                                                 (case (_lhsImoduleNm) of
                                                                                                                                  { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                                                                                  (case (_lhsIfinTyVarMp) of
                                                                                                                                   { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
                                                                                                                                   (case (_lhsIfinTyKiGam) of
                                                                                                                                    { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                                                                    (case (_lhsIfinKiVarMp) of
                                                                                                                                     { _tlOfinKiVarMp | _tlOfinKiVarMp `seq` (True) ->
                                                                                                                                     (case (_lhsIclsTyArgs) of
                                                                                                                                      { _tlOclsTyArgs | _tlOclsTyArgs `seq` (True) ->
                                                                                                                                      (case (tl_3 _tlOclsTyArgs _tlOfinKiVarMp _tlOfinTyKiGam _tlOfinTyVarMp _tlOmoduleNm _tlOopts _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                       { ( _tlIallErrSq,_tlIerrSq,_tlIfuncDeps,_tlIgathMentrelFilterMp,_tlIpp,_tlIppL,_tlIrange) | True ->
                                                                                                                                           (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                            { _hdOvalTyGlobFreeTvarS | _hdOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                            (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                             { _hdOtyTyTySigFreeTvarS | _hdOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                             (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                              { _hdOtyTyGlobFreeTvarS | _hdOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                              (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                               { _hdOtyKiGlobFreeTvarS | _hdOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                               (case (_lhsIopts) of
                                                                                                                                                { _hdOopts | _hdOopts `seq` (True) ->
                                                                                                                                                (case (_lhsImoduleNm) of
                                                                                                                                                 { _hdOmoduleNm | _hdOmoduleNm `seq` (True) ->
                                                                                                                                                 (case (_lhsIfinTyVarMp) of
                                                                                                                                                  { _hdOfinTyVarMp | _hdOfinTyVarMp `seq` (True) ->
                                                                                                                                                  (case (_lhsIfinTyKiGam) of
                                                                                                                                                   { _hdOfinTyKiGam | _hdOfinTyKiGam `seq` (True) ->
                                                                                                                                                   (case (_lhsIfinKiVarMp) of
                                                                                                                                                    { _hdOfinKiVarMp | _hdOfinKiVarMp `seq` (True) ->
                                                                                                                                                    (case (_lhsIclsTyArgs) of
                                                                                                                                                     { _hdOclsTyArgs | _hdOclsTyArgs `seq` (True) ->
                                                                                                                                                     (case (hd_3 _hdOclsTyArgs _hdOfinKiVarMp _hdOfinTyKiGam _hdOfinTyVarMp _hdOmoduleNm _hdOopts _hdOtyKiGlobFreeTvarS _hdOtyTyGlobFreeTvarS _hdOtyTyTySigFreeTvarS _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                      { ( _hdIallErrSq,_hdIerrSq,_hdIfuncDeps,_hdIgathMentrelFilterMp,_hdIpp,_hdIrange) | True ->
                                                                                                                                                          (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                           { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                           (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                            { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                            (case (_hdIfuncDeps ++ _tlIfuncDeps) of
                                                                                                                                                             { _lhsOfuncDeps | _lhsOfuncDeps `seq` (True) ->
                                                                                                                                                             (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                              { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                              (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                               { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                               (case (_hdIpp : _tlIppL) of
                                                                                                                                                                { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                (case (_hdIrange `rangeUnion` _tlIrange) of
                                                                                                                                                                 { _lhsOrange | _lhsOrange `seq` (True) ->
                                                                                                                                                                 ( _lhsOallErrSq,_lhsOerrSq,_lhsOfuncDeps,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL,_lhsOrange) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))
                                                                                                     in  sem_FuncDeps_Cons_3)) of
                                                                                              { ( sem_FuncDeps_3) | True ->
                                                                                              ( _lhsOtyKiGam,sem_FuncDeps_3) }) }) }) }) }) })))
                                                                  in  sem_FuncDeps_Cons_2)) of
                                                           { ( sem_FuncDeps_2) | True ->
                                                           ( _lhsOtyGam,sem_FuncDeps_2) }) }) }) }) }) })))
                               in  sem_FuncDeps_Cons_1)) of
                        { ( sem_FuncDeps_1) | True ->
                        ( _lhsOgUniq,sem_FuncDeps_1) }) }) }) }) }) })))

sem_FuncDeps_Nil :: T_FuncDeps 

sem_FuncDeps_Nil  =
    (\ _lhsIgUniq ->
         _lhsIgUniq `seq`
         ((case (_lhsIgUniq) of
           { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
           (case ((let sem_FuncDeps_Nil_1 :: T_FuncDeps_1 
                       sem_FuncDeps_Nil_1  =
                           (\ _lhsItyGam ->
                                _lhsItyGam `seq`
                                ((case (_lhsItyGam) of
                                  { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                  (case ((let sem_FuncDeps_Nil_2 :: T_FuncDeps_2 
                                              sem_FuncDeps_Nil_2  =
                                                  (\ _lhsItyKiGam ->
                                                       _lhsItyKiGam `seq`
                                                       ((case (_lhsItyKiGam) of
                                                         { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                         (case ((let sem_FuncDeps_Nil_3 :: T_FuncDeps_3 
                                                                     sem_FuncDeps_Nil_3  =
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
                                                                                       ((case (Seq.empty) of
                                                                                         { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                         (case (Seq.empty) of
                                                                                          { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                          (case ([]) of
                                                                                           { _lhsOfuncDeps | _lhsOfuncDeps `seq` (True) ->
                                                                                           (case (Map.empty) of
                                                                                            { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                            (case (empty) of
                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                             (case ([]) of
                                                                                              { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                              (case (emptyRange) of
                                                                                               { _lhsOrange | _lhsOrange `seq` (True) ->
                                                                                               ( _lhsOallErrSq,_lhsOerrSq,_lhsOfuncDeps,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL,_lhsOrange) }) }) }) }) }) }) }))))))))))))
                                                                 in  sem_FuncDeps_Nil_3)) of
                                                          { ( sem_FuncDeps_3) | True ->
                                                          ( _lhsOtyKiGam,sem_FuncDeps_3) }) })))
                                          in  sem_FuncDeps_Nil_2)) of
                                   { ( sem_FuncDeps_2) | True ->
                                   ( _lhsOtyGam,sem_FuncDeps_2) }) })))
                   in  sem_FuncDeps_Nil_1)) of
            { ( sem_FuncDeps_1) | True ->
            ( _lhsOgUniq,sem_FuncDeps_1) }) })))


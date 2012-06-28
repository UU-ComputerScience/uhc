


module EH101.EH.MainAG_TyVars where

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

-- TyVars ------------------------------------------------------
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
   visit 3:
      chained attribute:
         tyKiGam              : TyKiGam
      synthesized attributes:
         gathTyVarPolGam      : PolGam
         intlTyKiGam          : TyKiGam
         kiL                  : TyL
         polVarL              : [Polarity]
         tyL                  : TyL
   visit 4:
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
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         nmL                  : [HsName]
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : TyVar 
         child tl             : TyVars 
      alternative Nil:
-}
sem_TyVars_Cons :: T_TyVar  ->
                   T_TyVars  ->
                   T_TyVars 

sem_TyVars_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (case (tl_ ) of
     { ( _tlIrange,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdIrange,hd_1) | True ->
              (case (_hdIrange `rangeUnion` _tlIrange) of
               { _lhsOrange | _lhsOrange `seq` (True) ->
               (case ((let sem_TyVars_Cons_1 :: T_TyVars_1 
                           sem_TyVars_Cons_1  =
                               (\ _lhsIgUniq ->
                                    _lhsIgUniq `seq`
                                    ((case (_lhsIgUniq) of
                                      { _hdOgUniq | _hdOgUniq `seq` (True) ->
                                      (case (hd_1 _hdOgUniq ) of
                                       { ( _hdIgUniq,hd_2) | True ->
                                           (case (_hdIgUniq) of
                                            { _tlOgUniq | _tlOgUniq `seq` (True) ->
                                            (case (tl_1 _tlOgUniq ) of
                                             { ( _tlIgUniq,tl_2) | True ->
                                                 (case (_tlIgUniq) of
                                                  { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                  (case ((let sem_TyVars_Cons_2 :: T_TyVars_2 
                                                              sem_TyVars_Cons_2  =
                                                                  (\ _lhsItyGam ->
                                                                       _lhsItyGam `seq`
                                                                       ((case (_lhsItyGam) of
                                                                         { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                                                         (case (hd_2 _hdOtyGam ) of
                                                                          { ( _hdItyGam,hd_3) | True ->
                                                                              (case (_hdItyGam) of
                                                                               { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                               (case (tl_2 _tlOtyGam ) of
                                                                                { ( _tlItyGam,tl_3) | True ->
                                                                                    (case (_tlItyGam) of
                                                                                     { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                     (case ((let sem_TyVars_Cons_3 :: T_TyVars_3 
                                                                                                 sem_TyVars_Cons_3  =
                                                                                                     (\ _lhsItyKiGam ->
                                                                                                          _lhsItyKiGam `seq`
                                                                                                          ((case (_lhsItyKiGam) of
                                                                                                            { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                                            (case (hd_3 _hdOtyKiGam ) of
                                                                                                             { ( _hdIgathTyVarPolGam,_hdIintlTyKiGam,_hdIki,_hdIpolVarL,_hdIty,_hdItyKiGam,hd_4) | True ->
                                                                                                                 (case (_hdItyKiGam) of
                                                                                                                  { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                                                  (case (tl_3 _tlOtyKiGam ) of
                                                                                                                   { ( _tlIgathTyVarPolGam,_tlIintlTyKiGam,_tlIkiL,_tlIpolVarL,_tlItyKiGam,_tlItyL,tl_4) | True ->
                                                                                                                       (case (_hdIgathTyVarPolGam `gamUnion` _tlIgathTyVarPolGam) of
                                                                                                                        { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                        (case (_hdIintlTyKiGam `gamUnion` _tlIintlTyKiGam) of
                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                         (case (_hdIki : _tlIkiL) of
                                                                                                                          { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                                          (case (_hdIpolVarL ++ _tlIpolVarL) of
                                                                                                                           { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                                                           (case (_tlItyKiGam) of
                                                                                                                            { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                            (case (_hdIty : _tlItyL) of
                                                                                                                             { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                                                                                                             (case ((let sem_TyVars_Cons_4 :: T_TyVars_4 
                                                                                                                                         sem_TyVars_Cons_4  =
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
                                                                                                                                                                    (case (tl_4 _tlOfinKiVarMp _tlOfinTyKiGam _tlOfinTyVarMp _tlOmoduleNm _tlOopts _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                                                     { ( _tlIallErrSq,_tlIerrSq,_tlIgathMentrelFilterMp,_tlInmL,_tlIpp,_tlIppL) | True ->
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
                                                                                                                                                                                  (case (hd_4 _hdOfinKiVarMp _hdOfinTyKiGam _hdOfinTyVarMp _hdOmoduleNm _hdOopts _hdOtyKiGlobFreeTvarS _hdOtyTyGlobFreeTvarS _hdOtyTyTySigFreeTvarS _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                   { ( _hdIallErrSq,_hdIerrSq,_hdIgathMentrelFilterMp,_hdInm,_hdIpp) | True ->
                                                                                                                                                                                       (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                                                        { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                        (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                                                         { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                         (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                                                          { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                          (case (_hdInm : _tlInmL) of
                                                                                                                                                                                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                                                                                                                                                           (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                                                            { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                            (case (_hdIpp : _tlIppL) of
                                                                                                                                                                                             { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                             ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOnmL,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))
                                                                                                                                     in  sem_TyVars_Cons_4)) of
                                                                                                                              { ( sem_TyVars_4) | True ->
                                                                                                                              ( _lhsOgathTyVarPolGam,_lhsOintlTyKiGam,_lhsOkiL,_lhsOpolVarL,_lhsOtyKiGam,_lhsOtyL,sem_TyVars_4) }) }) }) }) }) }) }) }) }) }) })))
                                                                                             in  sem_TyVars_Cons_3)) of
                                                                                      { ( sem_TyVars_3) | True ->
                                                                                      ( _lhsOtyGam,sem_TyVars_3) }) }) }) }) }) })))
                                                          in  sem_TyVars_Cons_2)) of
                                                   { ( sem_TyVars_2) | True ->
                                                   ( _lhsOgUniq,sem_TyVars_2) }) }) }) }) }) })))
                       in  sem_TyVars_Cons_1)) of
                { ( sem_TyVars_1) | True ->
                ( _lhsOrange,sem_TyVars_1) }) }) }) })

sem_TyVars_Nil :: T_TyVars 

sem_TyVars_Nil  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_TyVars_Nil_1 :: T_TyVars_1 
                 sem_TyVars_Nil_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_TyVars_Nil_2 :: T_TyVars_2 
                                        sem_TyVars_Nil_2  =
                                            (\ _lhsItyGam ->
                                                 _lhsItyGam `seq`
                                                 ((case (_lhsItyGam) of
                                                   { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                   (case ((let sem_TyVars_Nil_3 :: T_TyVars_3 
                                                               sem_TyVars_Nil_3  =
                                                                   (\ _lhsItyKiGam ->
                                                                        _lhsItyKiGam `seq`
                                                                        ((case (emptyGam) of
                                                                          { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                          (case (emptyGam) of
                                                                           { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                           (case ([]) of
                                                                            { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                            (case ([]) of
                                                                             { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                             (case (_lhsItyKiGam) of
                                                                              { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                              (case ([]) of
                                                                               { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                                                               (case ((let sem_TyVars_Nil_4 :: T_TyVars_4 
                                                                                           sem_TyVars_Nil_4  =
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
                                                                                                              (case (Seq.empty) of
                                                                                                               { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                               (case (Map.empty) of
                                                                                                                { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                (case ([]) of
                                                                                                                 { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                                                                                 (case (empty) of
                                                                                                                  { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                  (case ([]) of
                                                                                                                   { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                   ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOnmL,_lhsOpp,_lhsOppL) }) }) }) }) }) })))))))))))
                                                                                       in  sem_TyVars_Nil_4)) of
                                                                                { ( sem_TyVars_4) | True ->
                                                                                ( _lhsOgathTyVarPolGam,_lhsOintlTyKiGam,_lhsOkiL,_lhsOpolVarL,_lhsOtyKiGam,_lhsOtyL,sem_TyVars_4) }) }) }) }) }) }) })))
                                                           in  sem_TyVars_Nil_3)) of
                                                    { ( sem_TyVars_3) | True ->
                                                    ( _lhsOtyGam,sem_TyVars_3) }) })))
                                    in  sem_TyVars_Nil_2)) of
                             { ( sem_TyVars_2) | True ->
                             ( _lhsOgUniq,sem_TyVars_2) }) })))
             in  sem_TyVars_Nil_1)) of
      { ( sem_TyVars_1) | True ->
      ( _lhsOrange,sem_TyVars_1) }) })


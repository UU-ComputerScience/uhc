


module EH101.EH.MainAG_TyExprs where

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

-- TyExprs -----------------------------------------------------
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
         polVarL              : [Polarity]
         tyL                  : TyL
   visit 3:
      inherited attribute:
         polGam               : PolGam
      chained attribute:
         polVarMp             : VarMp
   visit 4:
      chained attribute:
         tyKiGam              : TyKiGam
      synthesized attribute:
         intlTyKiGam          : TyKiGam
   visit 5:
      chained attribute:
         kiVarMp              : VarMp
      synthesized attributes:
         gathTyVarPolGam      : PolGam
         kiL                  : TyL
         tyVarWildMp          : TyVarWildMp
   visit 6:
      inherited attributes:
         clGam                : ClGam
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         finTyVarMp           : VarMp
         kiGam                : KiGam
         knPolCtx             : Polarity
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
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : TyExpr 
         child tl             : TyExprs 
         visit 1:
            local _tup241     : {(UID,UID)}
         visit 2:
            local lUniq_17_polCtx : {UID}
            local polCtxVar   : {Polarity}
            intra _tup241     : {(UID,UID)}
      alternative Nil:
-}
sem_TyExprs_Cons :: T_TyExpr  ->
                    T_TyExprs  ->
                    T_TyExprs 

sem_TyExprs_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (case (tl_ ) of
     { ( _tlIrange,tl_1) | True ->
         (case (hd_ ) of
          { ( _hdIrange,hd_1) | True ->
              (case (_hdIrange `rangeUnion` _tlIrange) of
               { _lhsOrange | _lhsOrange `seq` (True) ->
               (case ((let sem_TyExprs_Cons_1 :: T_TyExprs_1 
                           sem_TyExprs_Cons_1  =
                               (\ _lhsIgUniq ->
                                    _lhsIgUniq `seq`
                                    ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq_17_polCtx) -> (__cont, lUniq_17_polCtx)} )) of
                                      { __tup241 | __tup241 `seq` (True) ->
                                      (case (__tup241) of
                                       { (_hdOgUniq,_) | _hdOgUniq `seq` (True) ->
                                       (case (hd_1 _hdOgUniq ) of
                                        { ( _hdIgUniq,hd_2) | True ->
                                            (case (_hdIgUniq) of
                                             { _tlOgUniq | _tlOgUniq `seq` (True) ->
                                             (case (tl_1 _tlOgUniq ) of
                                              { ( _tlIgUniq,tl_2) | True ->
                                                  (case (_tlIgUniq) of
                                                   { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                   (case ((let sem_TyExprs_Cons_2 :: T_TyExprs_2 
                                                               sem_TyExprs_Cons_2  =
                                                                   (\ _lhsItyGam ->
                                                                        _lhsItyGam `seq`
                                                                        ((case (_lhsItyGam) of
                                                                          { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                                                          (case (hd_2 _hdOtyGam ) of
                                                                           { ( _hdIty,_hdItyGam,hd_3) | True ->
                                                                               (case (_hdItyGam) of
                                                                                { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                                (case (__tup241) of
                                                                                 { (_,_lUniq_17_polCtx) | _lUniq_17_polCtx `seq` (True) ->
                                                                                 (case (mkPolVar _lUniq_17_polCtx) of
                                                                                  { _polCtxVar | _polCtxVar `seq` (True) ->
                                                                                  (case (_polCtxVar) of
                                                                                   { _hdOknPolCtx | _hdOknPolCtx `seq` (True) ->
                                                                                   (case (tl_2 _tlOtyGam ) of
                                                                                    { ( _tlIpolVarL,_tlItyGam,_tlItyL,tl_3) | True ->
                                                                                        (case (hd_3 _hdOknPolCtx ) of
                                                                                         { ( _hdIpolVarL,hd_4) | True ->
                                                                                             (case (_hdIpolVarL ++ _tlIpolVarL) of
                                                                                              { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                              (case (_tlItyGam) of
                                                                                               { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                               (case (_hdIty : _tlItyL) of
                                                                                                { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                                                                                (case ((let sem_TyExprs_Cons_3 :: T_TyExprs_3 
                                                                                                            sem_TyExprs_Cons_3  =
                                                                                                                (\ _lhsIpolGam
                                                                                                                   _lhsIpolVarMp ->
                                                                                                                     _lhsIpolGam `seq`
                                                                                                                     (_lhsIpolVarMp `seq`
                                                                                                                      ((case (_lhsIpolVarMp) of
                                                                                                                        { _hdOpolVarMp | _hdOpolVarMp `seq` (True) ->
                                                                                                                        (case (_lhsIpolGam) of
                                                                                                                         { _hdOpolGam | _hdOpolGam `seq` (True) ->
                                                                                                                         (case (hd_4 _hdOpolGam _hdOpolVarMp ) of
                                                                                                                          { ( _hdImbStrictness,_hdIpolVarMp,hd_5) | True ->
                                                                                                                              (case (_hdIpolVarMp) of
                                                                                                                               { _tlOpolVarMp | _tlOpolVarMp `seq` (True) ->
                                                                                                                               (case (_lhsIpolGam) of
                                                                                                                                { _tlOpolGam | _tlOpolGam `seq` (True) ->
                                                                                                                                (case (tl_3 _tlOpolGam _tlOpolVarMp ) of
                                                                                                                                 { ( _tlIpolVarMp,tl_4) | True ->
                                                                                                                                     (case (_tlIpolVarMp) of
                                                                                                                                      { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                                      (case ((let sem_TyExprs_Cons_4 :: T_TyExprs_4 
                                                                                                                                                  sem_TyExprs_Cons_4  =
                                                                                                                                                      (\ _lhsItyKiGam ->
                                                                                                                                                           _lhsItyKiGam `seq`
                                                                                                                                                           ((case (_lhsItyKiGam) of
                                                                                                                                                             { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                                                                                                                                             (case (hd_5 _hdOtyKiGam ) of
                                                                                                                                                              { ( _hdIintlTyKiGam,_hdItyKiGam,hd_6) | True ->
                                                                                                                                                                  (case (_hdItyKiGam) of
                                                                                                                                                                   { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                                                                                                                   (case (tl_4 _tlOtyKiGam ) of
                                                                                                                                                                    { ( _tlIintlTyKiGam,_tlItyKiGam,tl_5) | True ->
                                                                                                                                                                        (case (_hdIintlTyKiGam `gamUnion` _tlIintlTyKiGam) of
                                                                                                                                                                         { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                         (case (_tlItyKiGam) of
                                                                                                                                                                          { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                          (case ((let sem_TyExprs_Cons_5 :: T_TyExprs_5 
                                                                                                                                                                                      sem_TyExprs_Cons_5  =
                                                                                                                                                                                          (\ _lhsIkiVarMp ->
                                                                                                                                                                                               _lhsIkiVarMp `seq`
                                                                                                                                                                                               ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                 { _hdOkiVarMp | _hdOkiVarMp `seq` (True) ->
                                                                                                                                                                                                 (case (hd_6 _hdOkiVarMp ) of
                                                                                                                                                                                                  { ( _hdIgathTyVarPolGam,_hdIki,_hdIkiVarMp,_hdIpol,_hdItyVarWildMp,hd_7) | True ->
                                                                                                                                                                                                      (case (_hdIkiVarMp) of
                                                                                                                                                                                                       { _tlOkiVarMp | _tlOkiVarMp `seq` (True) ->
                                                                                                                                                                                                       (case (tl_5 _tlOkiVarMp ) of
                                                                                                                                                                                                        { ( _tlIgathTyVarPolGam,_tlIkiL,_tlIkiVarMp,_tlItyVarWildMp,tl_6) | True ->
                                                                                                                                                                                                            (case (_hdIgathTyVarPolGam `gamUnion` _tlIgathTyVarPolGam) of
                                                                                                                                                                                                             { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                                                                                                             (case (_hdIki : _tlIkiL) of
                                                                                                                                                                                                              { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                                                                                                                              (case (_tlIkiVarMp) of
                                                                                                                                                                                                               { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                               (case (_hdItyVarWildMp `Map.union` _tlItyVarWildMp) of
                                                                                                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                (case ((let sem_TyExprs_Cons_6 :: T_TyExprs_6 
                                                                                                                                                                                                                            sem_TyExprs_Cons_6  =
                                                                                                                                                                                                                                (\ _lhsIclGam
                                                                                                                                                                                                                                   _lhsIfinKiVarMp
                                                                                                                                                                                                                                   _lhsIfinTyKiGam
                                                                                                                                                                                                                                   _lhsIfinTyVarMp
                                                                                                                                                                                                                                   _lhsIkiGam
                                                                                                                                                                                                                                   _lhsIknPolCtx
                                                                                                                                                                                                                                   _lhsImoduleNm
                                                                                                                                                                                                                                   _lhsIopts
                                                                                                                                                                                                                                   _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                   _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                   _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                   _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                     _lhsIclGam `seq`
                                                                                                                                                                                                                                     (_lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                      (_lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                                       (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                        (_lhsIkiGam `seq`
                                                                                                                                                                                                                                         (_lhsIknPolCtx `seq`
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
                                                                                                                                                                                                                                                       (case (_lhsIknPolCtx) of
                                                                                                                                                                                                                                                        { _tlOknPolCtx | _tlOknPolCtx `seq` (True) ->
                                                                                                                                                                                                                                                        (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                         { _tlOkiGam | _tlOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                         (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                          { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                          (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                           { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                           (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                            { _tlOfinKiVarMp | _tlOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                            (case (_lhsIclGam) of
                                                                                                                                                                                                                                                             { _tlOclGam | _tlOclGam `seq` (True) ->
                                                                                                                                                                                                                                                             (case (tl_6 _tlOclGam _tlOfinKiVarMp _tlOfinTyKiGam _tlOfinTyVarMp _tlOkiGam _tlOknPolCtx _tlOmoduleNm _tlOopts _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                              { ( _tlIallErrSq,_tlIclMissNmS,_tlIclNmS,_tlIerrSq,_tlIgathMentrelFilterMp,_tlIpp,_tlIppL) | True ->
                                                                                                                                                                                                                                                                  (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                   { _hdOclGam | _hdOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                   (case (hd_7 _hdOclGam ) of
                                                                                                                                                                                                                                                                    { ( _hdIevTy,hd_8) | True ->
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
                                                                                                                                                                                                                                                                              (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                               { _hdOkiGam | _hdOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                               (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                { _hdOfinTyVarMp | _hdOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                 { _hdOfinTyKiGam | _hdOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                 (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                  { _hdOfinKiVarMp | _hdOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                  (case (hd_8 _hdOfinKiVarMp _hdOfinTyKiGam _hdOfinTyVarMp _hdOkiGam _hdOmoduleNm _hdOopts _hdOtyKiGlobFreeTvarS _hdOtyTyGlobFreeTvarS _hdOtyTyTySigFreeTvarS _hdOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                   { ( _hdIallErrSq,_hdIappArgPPL,_hdIappFunNm,_hdIappFunPP,_hdIclMissNmS,_hdIclNmS,_hdIerrSq,_hdIgathMentrelFilterMp,_hdIpp,_hdItyWildL) | True ->
                                                                                                                                                                                                                                                                                       (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                                                                                                                                                                                        { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_hdIclMissNmS `Set.union` _tlIclMissNmS) of
                                                                                                                                                                                                                                                                                         { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_hdIclNmS `Set.union` _tlIclNmS) of
                                                                                                                                                                                                                                                                                          { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                                                                                                                                                                                           { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                            { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_hdIpp >-< _tlIpp) of
                                                                                                                                                                                                                                                                                             { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_hdIpp : _tlIppL) of
                                                                                                                                                                                                                                                                                              { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                              ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))
                                                                                                                                                                                                                        in  sem_TyExprs_Cons_6)) of
                                                                                                                                                                                                                 { ( sem_TyExprs_6) | True ->
                                                                                                                                                                                                                 ( _lhsOgathTyVarPolGam,_lhsOkiL,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_TyExprs_6) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                  in  sem_TyExprs_Cons_5)) of
                                                                                                                                                                           { ( sem_TyExprs_5) | True ->
                                                                                                                                                                           ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExprs_5) }) }) }) }) }) }) })))
                                                                                                                                              in  sem_TyExprs_Cons_4)) of
                                                                                                                                       { ( sem_TyExprs_4) | True ->
                                                                                                                                       ( _lhsOpolVarMp,sem_TyExprs_4) }) }) }) }) }) }) }) }))))
                                                                                                        in  sem_TyExprs_Cons_3)) of
                                                                                                 { ( sem_TyExprs_3) | True ->
                                                                                                 ( _lhsOpolVarL,_lhsOtyGam,_lhsOtyL,sem_TyExprs_3) }) }) }) }) }) }) }) }) }) }) }) })))
                                                           in  sem_TyExprs_Cons_2)) of
                                                    { ( sem_TyExprs_2) | True ->
                                                    ( _lhsOgUniq,sem_TyExprs_2) }) }) }) }) }) }) })))
                       in  sem_TyExprs_Cons_1)) of
                { ( sem_TyExprs_1) | True ->
                ( _lhsOrange,sem_TyExprs_1) }) }) }) })

sem_TyExprs_Nil :: T_TyExprs 

sem_TyExprs_Nil  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_TyExprs_Nil_1 :: T_TyExprs_1 
                 sem_TyExprs_Nil_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_TyExprs_Nil_2 :: T_TyExprs_2 
                                        sem_TyExprs_Nil_2  =
                                            (\ _lhsItyGam ->
                                                 _lhsItyGam `seq`
                                                 ((case ([]) of
                                                   { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                   (case (_lhsItyGam) of
                                                    { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                    (case ([]) of
                                                     { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                                     (case ((let sem_TyExprs_Nil_3 :: T_TyExprs_3 
                                                                 sem_TyExprs_Nil_3  =
                                                                     (\ _lhsIpolGam
                                                                        _lhsIpolVarMp ->
                                                                          _lhsIpolGam `seq`
                                                                          (_lhsIpolVarMp `seq`
                                                                           ((case (_lhsIpolVarMp) of
                                                                             { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                             (case ((let sem_TyExprs_Nil_4 :: T_TyExprs_4 
                                                                                         sem_TyExprs_Nil_4  =
                                                                                             (\ _lhsItyKiGam ->
                                                                                                  _lhsItyKiGam `seq`
                                                                                                  ((case (emptyGam) of
                                                                                                    { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                    (case (_lhsItyKiGam) of
                                                                                                     { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                     (case ((let sem_TyExprs_Nil_5 :: T_TyExprs_5 
                                                                                                                 sem_TyExprs_Nil_5  =
                                                                                                                     (\ _lhsIkiVarMp ->
                                                                                                                          _lhsIkiVarMp `seq`
                                                                                                                          ((case (emptyGam) of
                                                                                                                            { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                                                            (case ([]) of
                                                                                                                             { _lhsOkiL | _lhsOkiL `seq` (True) ->
                                                                                                                             (case (_lhsIkiVarMp) of
                                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                              (case (Map.empty) of
                                                                                                                               { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                               (case ((let sem_TyExprs_Nil_6 :: T_TyExprs_6 
                                                                                                                                           sem_TyExprs_Nil_6  =
                                                                                                                                               (\ _lhsIclGam
                                                                                                                                                  _lhsIfinKiVarMp
                                                                                                                                                  _lhsIfinTyKiGam
                                                                                                                                                  _lhsIfinTyVarMp
                                                                                                                                                  _lhsIkiGam
                                                                                                                                                  _lhsIknPolCtx
                                                                                                                                                  _lhsImoduleNm
                                                                                                                                                  _lhsIopts
                                                                                                                                                  _lhsItyKiGlobFreeTvarS
                                                                                                                                                  _lhsItyTyGlobFreeTvarS
                                                                                                                                                  _lhsItyTyTySigFreeTvarS
                                                                                                                                                  _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                    _lhsIclGam `seq`
                                                                                                                                                    (_lhsIfinKiVarMp `seq`
                                                                                                                                                     (_lhsIfinTyKiGam `seq`
                                                                                                                                                      (_lhsIfinTyVarMp `seq`
                                                                                                                                                       (_lhsIkiGam `seq`
                                                                                                                                                        (_lhsIknPolCtx `seq`
                                                                                                                                                         (_lhsImoduleNm `seq`
                                                                                                                                                          (_lhsIopts `seq`
                                                                                                                                                           (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                            (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                             (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                              (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                               ((case (Seq.empty) of
                                                                                                                                                                 { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                 (case (Set.empty) of
                                                                                                                                                                  { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                  (case (Set.empty) of
                                                                                                                                                                   { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                   (case (Seq.empty) of
                                                                                                                                                                    { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                    (case (Map.empty) of
                                                                                                                                                                     { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                     (case (empty) of
                                                                                                                                                                      { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                      (case ([]) of
                                                                                                                                                                       { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                       ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }))))))))))))))
                                                                                                                                       in  sem_TyExprs_Nil_6)) of
                                                                                                                                { ( sem_TyExprs_6) | True ->
                                                                                                                                ( _lhsOgathTyVarPolGam,_lhsOkiL,_lhsOkiVarMp,_lhsOtyVarWildMp,sem_TyExprs_6) }) }) }) }) })))
                                                                                                             in  sem_TyExprs_Nil_5)) of
                                                                                                      { ( sem_TyExprs_5) | True ->
                                                                                                      ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_TyExprs_5) }) }) })))
                                                                                     in  sem_TyExprs_Nil_4)) of
                                                                              { ( sem_TyExprs_4) | True ->
                                                                              ( _lhsOpolVarMp,sem_TyExprs_4) }) }))))
                                                             in  sem_TyExprs_Nil_3)) of
                                                      { ( sem_TyExprs_3) | True ->
                                                      ( _lhsOpolVarL,_lhsOtyGam,_lhsOtyL,sem_TyExprs_3) }) }) }) })))
                                    in  sem_TyExprs_Nil_2)) of
                             { ( sem_TyExprs_2) | True ->
                             ( _lhsOgUniq,sem_TyExprs_2) }) })))
             in  sem_TyExprs_Nil_1)) of
      { ( sem_TyExprs_1) | True ->
      ( _lhsOrange,sem_TyExprs_1) }) })


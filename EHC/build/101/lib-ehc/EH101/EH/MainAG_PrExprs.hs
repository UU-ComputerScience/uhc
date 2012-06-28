


module EH101.EH.MainAG_PrExprs where

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

-- PrExprs -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         clGam                : ClGam
         finKiVarMp           : VarMp
         finTyKiGam           : TyKiGam
         finTyVarMp           : VarMp
         kiGam                : KiGam
         knPolCtx             : Polarity
         moduleNm             : HsName
         opts                 : EHCOpts
         polGam               : PolGam
         tyKiGlobFreeTvarS    : TyVarIdS
         tyTyGlobFreeTvarS    : TyVarIdS
         tyTyTySigFreeTvarS   : TyVarIdS
         valTyGlobFreeTvarS   : TyVarIdS
      chained attributes:
         gUniq                : UID
         kiVarMp              : VarMp
         polVarMp             : VarMp
         tyGam                : TyGam
         tyKiGam              : TyKiGam
      synthesized attributes:
         allErrSq             : ErrSq
         clMissNmS            : HsNameS
         clNmS                : HsNameS
         errSq                : ErrSq
         gathMentrelFilterMp  : ModEntRelFilterMp
         intlTyKiGam          : TyKiGam
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
         prL                  : [Pred]
         range                : Range
         tyL                  : TyL
         tyVarWildMp          : TyVarWildMp
   alternatives:
      alternative Cons:
         child hd             : PrExpr 
         child tl             : PrExprs 
      alternative Nil:
-}
sem_PrExprs_Cons :: T_PrExpr  ->
                    T_PrExprs  ->
                    T_PrExprs 

sem_PrExprs_Cons hd_ tl_  | hd_ `seq` (tl_ `seq` (True)) =
    (\ _lhsIclGam
       _lhsIfinKiVarMp
       _lhsIfinTyKiGam
       _lhsIfinTyVarMp
       _lhsIgUniq
       _lhsIkiGam
       _lhsIkiVarMp
       _lhsIknPolCtx
       _lhsImoduleNm
       _lhsIopts
       _lhsIpolGam
       _lhsIpolVarMp
       _lhsItyGam
       _lhsItyKiGam
       _lhsItyKiGlobFreeTvarS
       _lhsItyTyGlobFreeTvarS
       _lhsItyTyTySigFreeTvarS
       _lhsIvalTyGlobFreeTvarS ->
         _lhsIclGam `seq`
         (_lhsIfinKiVarMp `seq`
          (_lhsIfinTyKiGam `seq`
           (_lhsIfinTyVarMp `seq`
            (_lhsIgUniq `seq`
             (_lhsIkiGam `seq`
              (_lhsIkiVarMp `seq`
               (_lhsIknPolCtx `seq`
                (_lhsImoduleNm `seq`
                 (_lhsIopts `seq`
                  (_lhsIpolGam `seq`
                   (_lhsIpolVarMp `seq`
                    (_lhsItyGam `seq`
                     (_lhsItyKiGam `seq`
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
                               (case (_lhsItyKiGam) of
                                { _hdOtyKiGam | _hdOtyKiGam `seq` (True) ->
                                (case (_lhsItyGam) of
                                 { _hdOtyGam | _hdOtyGam `seq` (True) ->
                                 (case (_lhsIpolVarMp) of
                                  { _hdOpolVarMp | _hdOpolVarMp `seq` (True) ->
                                  (case (_lhsIpolGam) of
                                   { _hdOpolGam | _hdOpolGam `seq` (True) ->
                                   (case (_lhsIknPolCtx) of
                                    { _hdOknPolCtx | _hdOknPolCtx `seq` (True) ->
                                    (case (_lhsIgUniq) of
                                     { _hdOgUniq | _hdOgUniq `seq` (True) ->
                                     (case (hd_ ) of
                                      { ( _hdIrange,hd_1) | True ->
                                          (case (hd_1 _hdOgUniq ) of
                                           { ( _hdIgUniq,hd_2) | True ->
                                               (case (hd_2 _hdOtyGam ) of
                                                { ( _hdIprTy,_hdIty,_hdItyGam,hd_3) | True ->
                                                    (case (hd_3 _hdOknPolCtx ) of
                                                     { ( _hdIpolVarL,hd_4) | True ->
                                                         (case (hd_4 _hdOpolGam _hdOpolVarMp ) of
                                                          { ( _hdIpolVarMp,hd_5) | True ->
                                                              (case (hd_5 _hdOtyKiGam ) of
                                                               { ( _hdIintlTyKiGam,_hdItyKiGam,hd_6) | True ->
                                                                   (case (_hdItyKiGam) of
                                                                    { _tlOtyKiGam | _tlOtyKiGam `seq` (True) ->
                                                                    (case (_hdItyGam) of
                                                                     { _tlOtyGam | _tlOtyGam `seq` (True) ->
                                                                     (case (_hdIpolVarMp) of
                                                                      { _tlOpolVarMp | _tlOpolVarMp `seq` (True) ->
                                                                      (case (_lhsIpolGam) of
                                                                       { _tlOpolGam | _tlOpolGam `seq` (True) ->
                                                                       (case (_lhsIopts) of
                                                                        { _tlOopts | _tlOopts `seq` (True) ->
                                                                        (case (_lhsImoduleNm) of
                                                                         { _tlOmoduleNm | _tlOmoduleNm `seq` (True) ->
                                                                         (case (_lhsIknPolCtx) of
                                                                          { _tlOknPolCtx | _tlOknPolCtx `seq` (True) ->
                                                                          (case (_lhsIkiVarMp) of
                                                                           { _hdOkiVarMp | _hdOkiVarMp `seq` (True) ->
                                                                           (case (hd_6 _hdOkiVarMp ) of
                                                                            { ( _hdIgathTyVarPolGam,_hdIki,_hdIkiVarMp,_hdItyVarWildMp,hd_7) | True ->
                                                                                (case (_hdIkiVarMp) of
                                                                                 { _tlOkiVarMp | _tlOkiVarMp `seq` (True) ->
                                                                                 (case (_lhsIkiGam) of
                                                                                  { _tlOkiGam | _tlOkiGam `seq` (True) ->
                                                                                  (case (_hdIgUniq) of
                                                                                   { _tlOgUniq | _tlOgUniq `seq` (True) ->
                                                                                   (case (_lhsIfinTyVarMp) of
                                                                                    { _tlOfinTyVarMp | _tlOfinTyVarMp `seq` (True) ->
                                                                                    (case (_lhsIfinTyKiGam) of
                                                                                     { _tlOfinTyKiGam | _tlOfinTyKiGam `seq` (True) ->
                                                                                     (case (_lhsIfinKiVarMp) of
                                                                                      { _tlOfinKiVarMp | _tlOfinKiVarMp `seq` (True) ->
                                                                                      (case (_lhsIclGam) of
                                                                                       { _tlOclGam | _tlOclGam `seq` (True) ->
                                                                                       (case (tl_ _tlOclGam _tlOfinKiVarMp _tlOfinTyKiGam _tlOfinTyVarMp _tlOgUniq _tlOkiGam _tlOkiVarMp _tlOknPolCtx _tlOmoduleNm _tlOopts _tlOpolGam _tlOpolVarMp _tlOtyGam _tlOtyKiGam _tlOtyKiGlobFreeTvarS _tlOtyTyGlobFreeTvarS _tlOtyTyTySigFreeTvarS _tlOvalTyGlobFreeTvarS ) of
                                                                                        { ( _tlIallErrSq,_tlIclMissNmS,_tlIclNmS,_tlIerrSq,_tlIgUniq,_tlIgathMentrelFilterMp,_tlIintlTyKiGam,_tlIkiVarMp,_tlIpolVarMp,_tlIpp,_tlIppL,_tlIprL,_tlIrange,_tlItyGam,_tlItyKiGam,_tlItyL,_tlItyVarWildMp) | True ->
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
                                                                                                             { ( _hdIallErrSq,_hdIclMissNmS,_hdIclNmS,_hdIerrSq,_hdIgathMentrelFilterMp,_hdIpp,_hdIpr) | True ->
                                                                                                                 (case (_hdIallErrSq `Seq.union` _tlIallErrSq) of
                                                                                                                  { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                  (case (_hdIclMissNmS `Set.union` _tlIclMissNmS) of
                                                                                                                   { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                   (case (_hdIclNmS `Set.union` _tlIclNmS) of
                                                                                                                    { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                    (case (_hdIerrSq `Seq.union` _tlIerrSq) of
                                                                                                                     { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                     (case (_tlIgUniq) of
                                                                                                                      { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                                                                                      (case (_hdIgathMentrelFilterMp `mentrelFilterMpUnion` _tlIgathMentrelFilterMp) of
                                                                                                                       { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                       (case (_hdIintlTyKiGam `gamUnion` _tlIintlTyKiGam) of
                                                                                                                        { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                        (case (_tlIkiVarMp) of
                                                                                                                         { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                         (case (_tlIpolVarMp) of
                                                                                                                          { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                                          (case (_hdIpp >-< _tlIpp) of
                                                                                                                           { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                           (case (_hdIpp : _tlIppL) of
                                                                                                                            { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                            (case (_hdIpr : _tlIprL) of
                                                                                                                             { _lhsOprL | _lhsOprL `seq` (True) ->
                                                                                                                             (case (_hdIrange `rangeUnion` _tlIrange) of
                                                                                                                              { _lhsOrange | _lhsOrange `seq` (True) ->
                                                                                                                              (case (_tlItyGam) of
                                                                                                                               { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                                                               (case (_tlItyKiGam) of
                                                                                                                                { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                (case (_hdIty : _tlItyL) of
                                                                                                                                 { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                                                                                                                 (case (_hdItyVarWildMp `Map.union` _tlItyVarWildMp) of
                                                                                                                                  { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                  ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgUniq,_lhsOgathMentrelFilterMp,_lhsOintlTyKiGam,_lhsOkiVarMp,_lhsOpolVarMp,_lhsOpp,_lhsOppL,_lhsOprL,_lhsOrange,_lhsOtyGam,_lhsOtyKiGam,_lhsOtyL,_lhsOtyVarWildMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))))))))

sem_PrExprs_Nil :: T_PrExprs 

sem_PrExprs_Nil  =
    (\ _lhsIclGam
       _lhsIfinKiVarMp
       _lhsIfinTyKiGam
       _lhsIfinTyVarMp
       _lhsIgUniq
       _lhsIkiGam
       _lhsIkiVarMp
       _lhsIknPolCtx
       _lhsImoduleNm
       _lhsIopts
       _lhsIpolGam
       _lhsIpolVarMp
       _lhsItyGam
       _lhsItyKiGam
       _lhsItyKiGlobFreeTvarS
       _lhsItyTyGlobFreeTvarS
       _lhsItyTyTySigFreeTvarS
       _lhsIvalTyGlobFreeTvarS ->
         _lhsIclGam `seq`
         (_lhsIfinKiVarMp `seq`
          (_lhsIfinTyKiGam `seq`
           (_lhsIfinTyVarMp `seq`
            (_lhsIgUniq `seq`
             (_lhsIkiGam `seq`
              (_lhsIkiVarMp `seq`
               (_lhsIknPolCtx `seq`
                (_lhsImoduleNm `seq`
                 (_lhsIopts `seq`
                  (_lhsIpolGam `seq`
                   (_lhsIpolVarMp `seq`
                    (_lhsItyGam `seq`
                     (_lhsItyKiGam `seq`
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
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                (case (Map.empty) of
                                 { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                 (case (emptyGam) of
                                  { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                  (case (_lhsIkiVarMp) of
                                   { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                   (case (_lhsIpolVarMp) of
                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                    (case (empty) of
                                     { _lhsOpp | _lhsOpp `seq` (True) ->
                                     (case ([]) of
                                      { _lhsOppL | _lhsOppL `seq` (True) ->
                                      (case ([]) of
                                       { _lhsOprL | _lhsOprL `seq` (True) ->
                                       (case (emptyRange) of
                                        { _lhsOrange | _lhsOrange `seq` (True) ->
                                        (case (_lhsItyGam) of
                                         { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                         (case (_lhsItyKiGam) of
                                          { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                          (case ([]) of
                                           { _lhsOtyL | _lhsOtyL `seq` (True) ->
                                           (case (Map.empty) of
                                            { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                            ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgUniq,_lhsOgathMentrelFilterMp,_lhsOintlTyKiGam,_lhsOkiVarMp,_lhsOpolVarMp,_lhsOpp,_lhsOppL,_lhsOprL,_lhsOrange,_lhsOtyGam,_lhsOtyKiGam,_lhsOtyL,_lhsOtyVarWildMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))))))))))))))))


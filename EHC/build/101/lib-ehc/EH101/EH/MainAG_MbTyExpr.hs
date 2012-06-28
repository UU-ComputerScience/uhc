


module EH101.EH.MainAG_MbTyExpr where

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

-- MbTyExpr ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         knPolCtx             : Polarity
         polGam               : PolGam
         tyGam                : TyGam
      chained attribute:
         polVarMp             : VarMp
      synthesized attribute:
         tyMb                 : Maybe Ty
   visit 3:
      inherited attribute:
         tyKiGam              : TyKiGam
      chained attribute:
         kiVarMp              : VarMp
      synthesized attribute:
         intlTyKiGam          : TyKiGam
   visit 4:
      inherited attributes:
         clGam                : ClGam
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
         ppMb                 : Maybe PP_Doc
         tyGam                : TyGam
         tyKiGam              : TyKiGam
         tyVarWildMp          : TyVarWildMp
   alternatives:
      alternative Just:
         child just           : TyExpr 
      alternative Nothing:
-}
sem_MbTyExpr_Just :: T_TyExpr  ->
                     T_MbTyExpr 

sem_MbTyExpr_Just just_  | just_ `seq` (True) =
    (case (just_ ) of
     { ( _justIrange,just_1) | True ->
         (case (_justIrange) of
          { _lhsOrange | _lhsOrange `seq` (True) ->
          (case ((let sem_MbTyExpr_Just_1 :: T_MbTyExpr_1 
                      sem_MbTyExpr_Just_1  =
                          (\ _lhsIgUniq ->
                               _lhsIgUniq `seq`
                               ((case (_lhsIgUniq) of
                                 { _justOgUniq | _justOgUniq `seq` (True) ->
                                 (case (just_1 _justOgUniq ) of
                                  { ( _justIgUniq,just_2) | True ->
                                      (case (_justIgUniq) of
                                       { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                       (case ((let sem_MbTyExpr_Just_2 :: T_MbTyExpr_2 
                                                   sem_MbTyExpr_Just_2  =
                                                       (\ _lhsIknPolCtx
                                                          _lhsIpolGam
                                                          _lhsIpolVarMp
                                                          _lhsItyGam ->
                                                            _lhsIknPolCtx `seq`
                                                            (_lhsIpolGam `seq`
                                                             (_lhsIpolVarMp `seq`
                                                              (_lhsItyGam `seq`
                                                               ((case (_lhsItyGam) of
                                                                 { _justOtyGam | _justOtyGam `seq` (True) ->
                                                                 (case (_lhsIpolVarMp) of
                                                                  { _justOpolVarMp | _justOpolVarMp `seq` (True) ->
                                                                  (case (_lhsIpolGam) of
                                                                   { _justOpolGam | _justOpolGam `seq` (True) ->
                                                                   (case (_lhsIknPolCtx) of
                                                                    { _justOknPolCtx | _justOknPolCtx `seq` (True) ->
                                                                    (case (just_2 _justOtyGam ) of
                                                                     { ( _justIty,_justItyGam,just_3) | True ->
                                                                         (case (just_3 _justOknPolCtx ) of
                                                                          { ( _justIpolVarL,just_4) | True ->
                                                                              (case (just_4 _justOpolGam _justOpolVarMp ) of
                                                                               { ( _justImbStrictness,_justIpolVarMp,just_5) | True ->
                                                                                   (case (_justIpolVarMp) of
                                                                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                    (case (Just _justIty) of
                                                                                     { _lhsOtyMb | _lhsOtyMb `seq` (True) ->
                                                                                     (case ((let sem_MbTyExpr_Just_3 :: T_MbTyExpr_3 
                                                                                                 sem_MbTyExpr_Just_3  =
                                                                                                     (\ _lhsIkiVarMp
                                                                                                        _lhsItyKiGam ->
                                                                                                          _lhsIkiVarMp `seq`
                                                                                                          (_lhsItyKiGam `seq`
                                                                                                           ((case (_lhsItyKiGam) of
                                                                                                             { _justOtyKiGam | _justOtyKiGam `seq` (True) ->
                                                                                                             (case (just_5 _justOtyKiGam ) of
                                                                                                              { ( _justIintlTyKiGam,_justItyKiGam,just_6) | True ->
                                                                                                                  (case (_justIintlTyKiGam) of
                                                                                                                   { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                   (case (_lhsIkiVarMp) of
                                                                                                                    { _justOkiVarMp | _justOkiVarMp `seq` (True) ->
                                                                                                                    (case (just_6 _justOkiVarMp ) of
                                                                                                                     { ( _justIgathTyVarPolGam,_justIki,_justIkiVarMp,_justIpol,_justItyVarWildMp,just_7) | True ->
                                                                                                                         (case (_justIkiVarMp) of
                                                                                                                          { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                          (case ((let sem_MbTyExpr_Just_4 :: T_MbTyExpr_4 
                                                                                                                                      sem_MbTyExpr_Just_4  =
                                                                                                                                          (\ _lhsIclGam
                                                                                                                                             _lhsIfinKiVarMp
                                                                                                                                             _lhsIfinTyKiGam
                                                                                                                                             _lhsIfinTyVarMp
                                                                                                                                             _lhsIkiGam
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
                                                                                                                                                   (_lhsImoduleNm `seq`
                                                                                                                                                    (_lhsIopts `seq`
                                                                                                                                                     (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                      (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                       (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                        (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                         ((case (_lhsIclGam) of
                                                                                                                                                           { _justOclGam | _justOclGam `seq` (True) ->
                                                                                                                                                           (case (just_7 _justOclGam ) of
                                                                                                                                                            { ( _justIevTy,just_8) | True ->
                                                                                                                                                                (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                 { _justOvalTyGlobFreeTvarS | _justOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                 (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                  { _justOtyTyTySigFreeTvarS | _justOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                  (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                   { _justOtyTyGlobFreeTvarS | _justOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                   (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                    { _justOtyKiGlobFreeTvarS | _justOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                    (case (_lhsIopts) of
                                                                                                                                                                     { _justOopts | _justOopts `seq` (True) ->
                                                                                                                                                                     (case (_lhsImoduleNm) of
                                                                                                                                                                      { _justOmoduleNm | _justOmoduleNm `seq` (True) ->
                                                                                                                                                                      (case (_lhsIkiGam) of
                                                                                                                                                                       { _justOkiGam | _justOkiGam `seq` (True) ->
                                                                                                                                                                       (case (_lhsIfinTyVarMp) of
                                                                                                                                                                        { _justOfinTyVarMp | _justOfinTyVarMp `seq` (True) ->
                                                                                                                                                                        (case (_lhsIfinTyKiGam) of
                                                                                                                                                                         { _justOfinTyKiGam | _justOfinTyKiGam `seq` (True) ->
                                                                                                                                                                         (case (_lhsIfinKiVarMp) of
                                                                                                                                                                          { _justOfinKiVarMp | _justOfinKiVarMp `seq` (True) ->
                                                                                                                                                                          (case (just_8 _justOfinKiVarMp _justOfinTyKiGam _justOfinTyVarMp _justOkiGam _justOmoduleNm _justOopts _justOtyKiGlobFreeTvarS _justOtyTyGlobFreeTvarS _justOtyTyTySigFreeTvarS _justOvalTyGlobFreeTvarS ) of
                                                                                                                                                                           { ( _justIallErrSq,_justIappArgPPL,_justIappFunNm,_justIappFunPP,_justIclMissNmS,_justIclNmS,_justIerrSq,_justIgathMentrelFilterMp,_justIpp,_justItyWildL) | True ->
                                                                                                                                                                               (case (_justIallErrSq) of
                                                                                                                                                                                { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                (case (_justIclMissNmS) of
                                                                                                                                                                                 { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                 (case (_justIclNmS) of
                                                                                                                                                                                  { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                  (case (_justIerrSq) of
                                                                                                                                                                                   { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                   (case (_justIgathMentrelFilterMp) of
                                                                                                                                                                                    { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                    (case (_justIpp) of
                                                                                                                                                                                     { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                     (case (Just _justIpp) of
                                                                                                                                                                                      { _lhsOppMb | _lhsOppMb `seq` (True) ->
                                                                                                                                                                                      (case (_justItyGam) of
                                                                                                                                                                                       { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                                                                                                                       (case (_justItyKiGam) of
                                                                                                                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                        (case (_justItyVarWildMp) of
                                                                                                                                                                                         { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                         ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppMb,_lhsOtyGam,_lhsOtyKiGam,_lhsOtyVarWildMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))))
                                                                                                                                  in  sem_MbTyExpr_Just_4)) of
                                                                                                                           { ( sem_MbTyExpr_4) | True ->
                                                                                                                           ( _lhsOintlTyKiGam,_lhsOkiVarMp,sem_MbTyExpr_4) }) }) }) }) }) }) }))))
                                                                                             in  sem_MbTyExpr_Just_3)) of
                                                                                      { ( sem_MbTyExpr_3) | True ->
                                                                                      ( _lhsOpolVarMp,_lhsOtyMb,sem_MbTyExpr_3) }) }) }) }) }) }) }) }) }) }))))))
                                               in  sem_MbTyExpr_Just_2)) of
                                        { ( sem_MbTyExpr_2) | True ->
                                        ( _lhsOgUniq,sem_MbTyExpr_2) }) }) }) })))
                  in  sem_MbTyExpr_Just_1)) of
           { ( sem_MbTyExpr_1) | True ->
           ( _lhsOrange,sem_MbTyExpr_1) }) }) })

sem_MbTyExpr_Nothing :: T_MbTyExpr 

sem_MbTyExpr_Nothing  =
    (case (emptyRange) of
     { _lhsOrange | _lhsOrange `seq` (True) ->
     (case ((let sem_MbTyExpr_Nothing_1 :: T_MbTyExpr_1 
                 sem_MbTyExpr_Nothing_1  =
                     (\ _lhsIgUniq ->
                          _lhsIgUniq `seq`
                          ((case (_lhsIgUniq) of
                            { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                            (case ((let sem_MbTyExpr_Nothing_2 :: T_MbTyExpr_2 
                                        sem_MbTyExpr_Nothing_2  =
                                            (\ _lhsIknPolCtx
                                               _lhsIpolGam
                                               _lhsIpolVarMp
                                               _lhsItyGam ->
                                                 _lhsIknPolCtx `seq`
                                                 (_lhsIpolGam `seq`
                                                  (_lhsIpolVarMp `seq`
                                                   (_lhsItyGam `seq`
                                                    ((case (_lhsIpolVarMp) of
                                                      { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                      (case (Nothing) of
                                                       { _lhsOtyMb | _lhsOtyMb `seq` (True) ->
                                                       (case ((let sem_MbTyExpr_Nothing_3 :: T_MbTyExpr_3 
                                                                   sem_MbTyExpr_Nothing_3  =
                                                                       (\ _lhsIkiVarMp
                                                                          _lhsItyKiGam ->
                                                                            _lhsIkiVarMp `seq`
                                                                            (_lhsItyKiGam `seq`
                                                                             ((case (emptyGam) of
                                                                               { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                               (case (_lhsIkiVarMp) of
                                                                                { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                (case ((let sem_MbTyExpr_Nothing_4 :: T_MbTyExpr_4 
                                                                                            sem_MbTyExpr_Nothing_4  =
                                                                                                (\ _lhsIclGam
                                                                                                   _lhsIfinKiVarMp
                                                                                                   _lhsIfinTyKiGam
                                                                                                   _lhsIfinTyVarMp
                                                                                                   _lhsIkiGam
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
                                                                                                                      (case (Nothing) of
                                                                                                                       { _lhsOppMb | _lhsOppMb `seq` (True) ->
                                                                                                                       (case (_lhsItyGam) of
                                                                                                                        { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                                                        (case (_lhsItyKiGam) of
                                                                                                                         { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                         (case (Map.empty) of
                                                                                                                          { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                          ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppMb,_lhsOtyGam,_lhsOtyKiGam,_lhsOtyVarWildMp) }) }) }) }) }) }) }) }) }) })))))))))))))
                                                                                        in  sem_MbTyExpr_Nothing_4)) of
                                                                                 { ( sem_MbTyExpr_4) | True ->
                                                                                 ( _lhsOintlTyKiGam,_lhsOkiVarMp,sem_MbTyExpr_4) }) }) }))))
                                                               in  sem_MbTyExpr_Nothing_3)) of
                                                        { ( sem_MbTyExpr_3) | True ->
                                                        ( _lhsOpolVarMp,_lhsOtyMb,sem_MbTyExpr_3) }) }) }))))))
                                    in  sem_MbTyExpr_Nothing_2)) of
                             { ( sem_MbTyExpr_2) | True ->
                             ( _lhsOgUniq,sem_MbTyExpr_2) }) })))
             in  sem_MbTyExpr_Nothing_1)) of
      { ( sem_MbTyExpr_1) | True ->
      ( _lhsOrange,sem_MbTyExpr_1) }) })





module EH101.EH.MainAG_TyVar where

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

-- TyVar -------------------------------------------------------
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
         ki                   : Ty
         polVarL              : [Polarity]
         ty                   : Ty
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
         nm                   : HsName
         pp                   : PP_Doc
   alternatives:
      alternative Var:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup245     : {(UID,UID,UID,UID)}
         visit 2:
            local lUniq       : {UID}
            local _tup243     : {(TyGamInfo,TyGam)}
            intra _tup245     : {(UID,UID,UID,UID)}
         visit 3:
            local lUniq_17_pol : {UID}
            local polVar      : _
            local lUniq_ki    : {UID}
            local tgi_        : {TyGamInfo}
            local _tup244     : _
            local tyKiGamNew  : _
            local tkgi_       : {TyKiGamInfo}
            local ki          : {Ty}
            intra _tup245     : {(UID,UID,UID,UID)}
            intra _tup243     : {(TyGamInfo,TyGam)}
-}
sem_TyVar_Var :: Range ->
                 HsName ->
                 T_TyVar 

sem_TyVar_Var hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_TyVar_Var_1 :: T_TyVar_1 
                  sem_TyVar_Var_1  =
                      (\ _lhsIgUniq ->
                           _lhsIgUniq `seq`
                           ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_17_pol) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq_17_pol,lUniq_ki)}}} )) of
                             { __tup245 | __tup245 `seq` (True) ->
                             (case (__tup245) of
                              { (_lhsOgUniq,_,_,_) | _lhsOgUniq `seq` (True) ->
                              (case ((let sem_TyVar_Var_2 :: T_TyVar_2 
                                          sem_TyVar_Var_2  =
                                              (\ _lhsItyGam ->
                                                   _lhsItyGam `seq`
                                                   ((case (__tup245) of
                                                     { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                     (case (tyGamLookupOrAdd _lUniq nm_ (gamTop _lhsItyGam) _lhsItyGam) of
                                                      { __tup243 | __tup243 `seq` (True) ->
                                                      (case (__tup243) of
                                                       { (_,_lhsOtyGam) | _lhsOtyGam `seq` (True) ->
                                                       (case ((let sem_TyVar_Var_3 :: T_TyVar_3 
                                                                   sem_TyVar_Var_3  =
                                                                       (\ _lhsItyKiGam ->
                                                                            _lhsItyKiGam `seq`
                                                                            ((case (__tup245) of
                                                                              { (_,_,_lUniq_17_pol,_) | _lUniq_17_pol `seq` (True) ->
                                                                              (case (mkPolVar _lUniq_17_pol) of
                                                                               { _polVar | _polVar `seq` (True) ->
                                                                               (case (gamSingleton nm_ (mkPGI _polVar)) of
                                                                                { _lhsOgathTyVarPolGam | _lhsOgathTyVarPolGam `seq` (True) ->
                                                                                (case (__tup245) of
                                                                                 { (_,_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                                 (case (__tup243) of
                                                                                  { (_tgi_,_) | _tgi_ `seq` (True) ->
                                                                                  (case (tyKiGamLookupOrAdd _lUniq_ki (tgiTy _tgi_) _lhsItyKiGam) of
                                                                                   { __tup244 | __tup244 `seq` (True) ->
                                                                                   (case (__tup244) of
                                                                                    { (_,_,_tyKiGamNew) | _tyKiGamNew `seq` (True) ->
                                                                                    (case (_tyKiGamNew) of
                                                                                     { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                     (case (__tup244) of
                                                                                      { (_tkgi_,_,_) | _tkgi_ `seq` (True) ->
                                                                                      (case (tkgiKi _tkgi_) of
                                                                                       { _ki | _ki `seq` (True) ->
                                                                                       (case (_ki) of
                                                                                        { _lhsOki | _lhsOki `seq` (True) ->
                                                                                        (case ([_polVar]) of
                                                                                         { _lhsOpolVarL | _lhsOpolVarL `seq` (True) ->
                                                                                         (case (tgiTy _tgi_) of
                                                                                          { _lhsOty | _lhsOty `seq` (True) ->
                                                                                          (case (__tup244) of
                                                                                           { (_,_lhsOtyKiGam,_) | _lhsOtyKiGam `seq` (True) ->
                                                                                           (case ((let sem_TyVar_Var_4 :: T_TyVar_4 
                                                                                                       sem_TyVar_Var_4  =
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
                                                                                                                            (case (nm_) of
                                                                                                                             { _lhsOnm | _lhsOnm `seq` (True) ->
                                                                                                                             (case (pp nm_) of
                                                                                                                              { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                              ( _lhsOallErrSq,_lhsOerrSq,_lhsOgathMentrelFilterMp,_lhsOnm,_lhsOpp) }) }) }) }) })))))))))))
                                                                                                   in  sem_TyVar_Var_4)) of
                                                                                            { ( sem_TyVar_4) | True ->
                                                                                            ( _lhsOgathTyVarPolGam,_lhsOintlTyKiGam,_lhsOki,_lhsOpolVarL,_lhsOty,_lhsOtyKiGam,sem_TyVar_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                                               in  sem_TyVar_Var_3)) of
                                                        { ( sem_TyVar_3) | True ->
                                                        ( _lhsOtyGam,sem_TyVar_3) }) }) }) })))
                                      in  sem_TyVar_Var_2)) of
                               { ( sem_TyVar_2) | True ->
                               ( _lhsOgUniq,sem_TyVar_2) }) }) })))
              in  sem_TyVar_Var_1)) of
       { ( sem_TyVar_1) | True ->
       ( _lhsOrange,sem_TyVar_1) }) }) })


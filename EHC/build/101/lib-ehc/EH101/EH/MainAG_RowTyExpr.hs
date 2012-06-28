


module EH101.EH.MainAG_RowTyExpr where

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

-- RowTyExpr ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attributes:
         gUniq                : UID
         positionalFldNmL     : [HsName]
   visit 2:
      chained attribute:
         tyGam                : TyGam
      synthesized attribute:
         tyRow                : Ty
   visit 3:
      inherited attributes:
         knPolCtx             : Polarity
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
      synthesized attribute:
         tyVarWildMp          : TyVarWildMp
   visit 6:
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
         extNm                : HsName
         gathMentrelFilterMp  : ModEntRelFilterMp
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Empty:
         child hsrange        : {Range}
         visit 0:
            local range       : {Range}
      alternative Ext:
         child hsrange        : {Range}
         child rowTyExpr      : RowTyExpr 
         child mbNm           : {Maybe HsName}
         child tyExpr         : TyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup213     : {(UID,UID)}
            local _tup212     : {(HsName,[HsName])}
         visit 2:
            local positionalNm : {HsName}
            local nm          : {HsName}
            intra _tup212     : {(HsName,[HsName])}
            intra _tup213     : {(UID,UID)}
         visit 3:
            intra _tup213     : {(UID,UID)}
            intra nm          : {HsName}
            intra positionalNm : {HsName}
         visit 4:
            intra _tup213     : {(UID,UID)}
            intra nm          : {HsName}
            intra positionalNm : {HsName}
         visit 5:
            local lUniq       : {UID}
            local fo_         : {FIOut}
            intra _tup213     : {(UID,UID)}
            intra nm          : {HsName}
            intra positionalNm : {HsName}
         visit 6:
            intra nm          : {HsName}
            intra positionalNm : {HsName}
      alternative Var:
         child hsrange        : {Range}
         child nm             : {HsName}
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup216     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            local lUniq       : {UID}
            local _tup214     : {(TyGamInfo,TyGam)}
            local tgi_        : {TyGamInfo}
            intra _tup216     : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            intra _tup216     : {(UID,UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra range       : {Range}
         visit 4:
            local lUniq_ki    : {UID}
            local _tup215     : _
            local tyKiGamNew  : _
            intra _tup216     : {(UID,UID,UID,UID)}
            intra tgi_        : {TyGamInfo}
            intra range       : {Range}
         visit 5:
            local lUniq2      : {UID}
            local tkgi_       : {TyKiGamInfo}
            local fo_         : {FIOut}
            intra _tup216     : {(UID,UID,UID,UID)}
            intra _tup215     : _
            intra range       : {Range}
         visit 6:
            local pp          : _
            intra fo_         : {FIOut}
            intra range       : {Range}
-}
sem_RowTyExpr_Empty :: Range ->
                       T_RowTyExpr 

sem_RowTyExpr_Empty hsrange_  | hsrange_ `seq` (True) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_RowTyExpr_Empty_1 :: T_RowTyExpr_1 
                  sem_RowTyExpr_Empty_1  =
                      (\ _lhsIgUniq
                         _lhsIpositionalFldNmL ->
                           _lhsIgUniq `seq`
                           (_lhsIpositionalFldNmL `seq`
                            ((case (_lhsIgUniq) of
                              { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                              (case (_lhsIpositionalFldNmL) of
                               { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                               (case ((let sem_RowTyExpr_Empty_2 :: T_RowTyExpr_2 
                                           sem_RowTyExpr_Empty_2  =
                                               (\ _lhsItyGam ->
                                                    _lhsItyGam `seq`
                                                    ((case (_lhsItyGam) of
                                                      { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                      (case (tyRowEmpty) of
                                                       { _lhsOtyRow | _lhsOtyRow `seq` (True) ->
                                                       (case ((let sem_RowTyExpr_Empty_3 :: T_RowTyExpr_3 
                                                                   sem_RowTyExpr_Empty_3  =
                                                                       (\ _lhsIknPolCtx
                                                                          _lhsIpolGam
                                                                          _lhsIpolVarMp ->
                                                                            _lhsIknPolCtx `seq`
                                                                            (_lhsIpolGam `seq`
                                                                             (_lhsIpolVarMp `seq`
                                                                              ((case (_lhsIpolVarMp) of
                                                                                { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                (case ((let sem_RowTyExpr_Empty_4 :: T_RowTyExpr_4 
                                                                                            sem_RowTyExpr_Empty_4  =
                                                                                                (\ _lhsItyKiGam ->
                                                                                                     _lhsItyKiGam `seq`
                                                                                                     ((case (emptyGam) of
                                                                                                       { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                       (case (_lhsItyKiGam) of
                                                                                                        { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                        (case ((let sem_RowTyExpr_Empty_5 :: T_RowTyExpr_5 
                                                                                                                    sem_RowTyExpr_Empty_5  =
                                                                                                                        (\ _lhsIkiVarMp ->
                                                                                                                             _lhsIkiVarMp `seq`
                                                                                                                             ((case (_lhsIkiVarMp) of
                                                                                                                               { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                               (case (Map.empty) of
                                                                                                                                { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                (case ((let sem_RowTyExpr_Empty_6 :: T_RowTyExpr_6 
                                                                                                                                            sem_RowTyExpr_Empty_6  =
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
                                                                                                                                                                    (case (hsnRowEmpty) of
                                                                                                                                                                     { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                     (case (Map.empty) of
                                                                                                                                                                      { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                      (case (empty) of
                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                       (case ([]) of
                                                                                                                                                                        { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                        ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOextNm,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) })))))))))))))
                                                                                                                                        in  sem_RowTyExpr_Empty_6)) of
                                                                                                                                 { ( sem_RowTyExpr_6) | True ->
                                                                                                                                 ( _lhsOkiVarMp,_lhsOtyVarWildMp,sem_RowTyExpr_6) }) }) })))
                                                                                                                in  sem_RowTyExpr_Empty_5)) of
                                                                                                         { ( sem_RowTyExpr_5) | True ->
                                                                                                         ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_RowTyExpr_5) }) }) })))
                                                                                        in  sem_RowTyExpr_Empty_4)) of
                                                                                 { ( sem_RowTyExpr_4) | True ->
                                                                                 ( _lhsOpolVarMp,sem_RowTyExpr_4) }) })))))
                                                               in  sem_RowTyExpr_Empty_3)) of
                                                        { ( sem_RowTyExpr_3) | True ->
                                                        ( _lhsOtyGam,_lhsOtyRow,sem_RowTyExpr_3) }) }) })))
                                       in  sem_RowTyExpr_Empty_2)) of
                                { ( sem_RowTyExpr_2) | True ->
                                ( _lhsOgUniq,_lhsOpositionalFldNmL,sem_RowTyExpr_2) }) }) }))))
              in  sem_RowTyExpr_Empty_1)) of
       { ( sem_RowTyExpr_1) | True ->
       ( _lhsOrange,sem_RowTyExpr_1) }) }) })

sem_RowTyExpr_Ext :: Range ->
                     T_RowTyExpr  ->
                     (Maybe HsName) ->
                     T_TyExpr  ->
                     T_RowTyExpr 

sem_RowTyExpr_Ext hsrange_ rowTyExpr_ mbNm_ tyExpr_  | hsrange_ `seq` (rowTyExpr_ `seq` (mbNm_ `seq` (tyExpr_ `seq` (True)))) =
    (case (tyExpr_ ) of
     { ( _tyExprIrange,tyExpr_1) | True ->
         (case (rowTyExpr_ ) of
          { ( _rowTyExprIrange,rowTyExpr_1) | True ->
              (case (rangeUnions [hsrange_, _rowTyExprIrange
                                                          , _tyExprIrange]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_RowTyExpr_Ext_1 :: T_RowTyExpr_1 
                            sem_RowTyExpr_Ext_1  =
                                (\ _lhsIgUniq
                                   _lhsIpositionalFldNmL ->
                                     _lhsIgUniq `seq`
                                     (_lhsIpositionalFldNmL `seq`
                                      ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                                        { __tup213 | __tup213 `seq` (True) ->
                                        (case (__tup213) of
                                         { (_rowTyExprOgUniq,_) | _rowTyExprOgUniq `seq` (True) ->
                                         (case (_lhsIpositionalFldNmL) of
                                          { _rowTyExprOpositionalFldNmL | _rowTyExprOpositionalFldNmL `seq` (True) ->
                                          (case (rowTyExpr_1 _rowTyExprOgUniq _rowTyExprOpositionalFldNmL ) of
                                           { ( _rowTyExprIgUniq,_rowTyExprIpositionalFldNmL,rowTyExpr_2) | True ->
                                               (case (_rowTyExprIgUniq) of
                                                { _tyExprOgUniq | _tyExprOgUniq `seq` (True) ->
                                                (case (tyExpr_1 _tyExprOgUniq ) of
                                                 { ( _tyExprIgUniq,tyExpr_2) | True ->
                                                     (case (_tyExprIgUniq) of
                                                      { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                      (case (hdAndTl _rowTyExprIpositionalFldNmL) of
                                                       { __tup212 | __tup212 `seq` (True) ->
                                                       (case (__tup212) of
                                                        { (_,_lhsOpositionalFldNmL) | _lhsOpositionalFldNmL `seq` (True) ->
                                                        (case ((let sem_RowTyExpr_Ext_2 :: T_RowTyExpr_2 
                                                                    sem_RowTyExpr_Ext_2  =
                                                                        (\ _lhsItyGam ->
                                                                             _lhsItyGam `seq`
                                                                             ((case (_lhsItyGam) of
                                                                               { _rowTyExprOtyGam | _rowTyExprOtyGam `seq` (True) ->
                                                                               (case (rowTyExpr_2 _rowTyExprOtyGam ) of
                                                                                { ( _rowTyExprItyGam,_rowTyExprItyRow,rowTyExpr_3) | True ->
                                                                                    (case (_rowTyExprItyGam) of
                                                                                     { _tyExprOtyGam | _tyExprOtyGam `seq` (True) ->
                                                                                     (case (tyExpr_2 _tyExprOtyGam ) of
                                                                                      { ( _tyExprIty,_tyExprItyGam,tyExpr_3) | True ->
                                                                                          (case (_tyExprItyGam) of
                                                                                           { _lhsOtyGam | _lhsOtyGam `seq` (True) ->
                                                                                           (case (__tup212) of
                                                                                            { (_positionalNm,_) | _positionalNm `seq` (True) ->
                                                                                            (case (maybe _positionalNm id mbNm_) of
                                                                                             { _nm | _nm `seq` (True) ->
                                                                                             (case (Ty_Ext _rowTyExprItyRow _nm _tyExprIty) of
                                                                                              { _lhsOtyRow | _lhsOtyRow `seq` (True) ->
                                                                                              (case ((let sem_RowTyExpr_Ext_3 :: T_RowTyExpr_3 
                                                                                                          sem_RowTyExpr_Ext_3  =
                                                                                                              (\ _lhsIknPolCtx
                                                                                                                 _lhsIpolGam
                                                                                                                 _lhsIpolVarMp ->
                                                                                                                   _lhsIknPolCtx `seq`
                                                                                                                   (_lhsIpolGam `seq`
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
                                                                                                                                            (case ((let sem_RowTyExpr_Ext_4 :: T_RowTyExpr_4 
                                                                                                                                                        sem_RowTyExpr_Ext_4  =
                                                                                                                                                            (\ _lhsItyKiGam ->
                                                                                                                                                                 _lhsItyKiGam `seq`
                                                                                                                                                                 ((case (_lhsItyKiGam) of
                                                                                                                                                                   { _rowTyExprOtyKiGam | _rowTyExprOtyKiGam `seq` (True) ->
                                                                                                                                                                   (case (rowTyExpr_4 _rowTyExprOtyKiGam ) of
                                                                                                                                                                    { ( _rowTyExprIintlTyKiGam,_rowTyExprItyKiGam,rowTyExpr_5) | True ->
                                                                                                                                                                        (case (_rowTyExprItyKiGam) of
                                                                                                                                                                         { _tyExprOtyKiGam | _tyExprOtyKiGam `seq` (True) ->
                                                                                                                                                                         (case (tyExpr_5 _tyExprOtyKiGam ) of
                                                                                                                                                                          { ( _tyExprIintlTyKiGam,_tyExprItyKiGam,tyExpr_6) | True ->
                                                                                                                                                                              (case (_rowTyExprIintlTyKiGam `gamUnion` _tyExprIintlTyKiGam) of
                                                                                                                                                                               { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                                               (case (_tyExprItyKiGam) of
                                                                                                                                                                                { _lhsOtyKiGam | _lhsOtyKiGam `seq` (True) ->
                                                                                                                                                                                (case ((let sem_RowTyExpr_Ext_5 :: T_RowTyExpr_5 
                                                                                                                                                                                            sem_RowTyExpr_Ext_5  =
                                                                                                                                                                                                (\ _lhsIkiVarMp ->
                                                                                                                                                                                                     _lhsIkiVarMp `seq`
                                                                                                                                                                                                     ((case (_lhsIkiVarMp) of
                                                                                                                                                                                                       { _rowTyExprOkiVarMp | _rowTyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                                       (case (rowTyExpr_5 _rowTyExprOkiVarMp ) of
                                                                                                                                                                                                        { ( _rowTyExprIkiVarMp,_rowTyExprItyVarWildMp,rowTyExpr_6) | True ->
                                                                                                                                                                                                            (case (_rowTyExprIkiVarMp) of
                                                                                                                                                                                                             { _tyExprOkiVarMp | _tyExprOkiVarMp `seq` (True) ->
                                                                                                                                                                                                             (case (__tup213) of
                                                                                                                                                                                                              { (_,_lUniq) | _lUniq `seq` (True) ->
                                                                                                                                                                                                              (case (tyExpr_6 _tyExprOkiVarMp ) of
                                                                                                                                                                                                               { ( _tyExprIgathTyVarPolGam,_tyExprIki,_tyExprIkiVarMp,_tyExprIpol,_tyExprItyVarWildMp,tyExpr_7) | True ->
                                                                                                                                                                                                                   (case (fitsIn weakFIOpts defaultFIEnv _lUniq _tyExprIkiVarMp _tyExprIki kiStar) of
                                                                                                                                                                                                                    { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                                                                                                    (case (foVarMp _fo_ `varUpd` _tyExprIkiVarMp) of
                                                                                                                                                                                                                     { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                                                                                     (case (_rowTyExprItyVarWildMp `Map.union` _tyExprItyVarWildMp) of
                                                                                                                                                                                                                      { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                                                                                                      (case ((let sem_RowTyExpr_Ext_6 :: T_RowTyExpr_6 
                                                                                                                                                                                                                                  sem_RowTyExpr_Ext_6  =
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
                                                                                                                                                                                                                                                                           (case (_lhsIvalTyGlobFreeTvarS) of
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
                                                                                                                                                                                                                                                                                           (case (_rowTyExprIallErrSq `Seq.union` _tyExprIallErrSq) of
                                                                                                                                                                                                                                                                                            { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_rowTyExprIclMissNmS `Set.union` _tyExprIclMissNmS) of
                                                                                                                                                                                                                                                                                             { _lhsOclMissNmS | _lhsOclMissNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_rowTyExprIclNmS `Set.union` _tyExprIclNmS) of
                                                                                                                                                                                                                                                                                              { _lhsOclNmS | _lhsOclNmS `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_rowTyExprIerrSq `Seq.union` _tyExprIerrSq) of
                                                                                                                                                                                                                                                                                               { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (_rowTyExprIextNm) of
                                                                                                                                                                                                                                                                                                { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                (case (_rowTyExprIgathMentrelFilterMp `mentrelFilterMpUnion` _tyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                 { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                 (case (_rowTyExprIpp) of
                                                                                                                                                                                                                                                                                                  { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                  (case (ppFld "::" (Just _positionalNm) _nm (pp _nm) _tyExprIpp : _rowTyExprIppL) of
                                                                                                                                                                                                                                                                                                   { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                                                                                                                                   ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOextNm,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))))
                                                                                                                                                                                                                              in  sem_RowTyExpr_Ext_6)) of
                                                                                                                                                                                                                       { ( sem_RowTyExpr_6) | True ->
                                                                                                                                                                                                                       ( _lhsOkiVarMp,_lhsOtyVarWildMp,sem_RowTyExpr_6) }) }) }) }) }) }) }) }) })))
                                                                                                                                                                                        in  sem_RowTyExpr_Ext_5)) of
                                                                                                                                                                                 { ( sem_RowTyExpr_5) | True ->
                                                                                                                                                                                 ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_RowTyExpr_5) }) }) }) }) }) }) })))
                                                                                                                                                    in  sem_RowTyExpr_Ext_4)) of
                                                                                                                                             { ( sem_RowTyExpr_4) | True ->
                                                                                                                                             ( _lhsOpolVarMp,sem_RowTyExpr_4) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                      in  sem_RowTyExpr_Ext_3)) of
                                                                                               { ( sem_RowTyExpr_3) | True ->
                                                                                               ( _lhsOtyGam,_lhsOtyRow,sem_RowTyExpr_3) }) }) }) }) }) }) }) }) })))
                                                                in  sem_RowTyExpr_Ext_2)) of
                                                         { ( sem_RowTyExpr_2) | True ->
                                                         ( _lhsOgUniq,_lhsOpositionalFldNmL,sem_RowTyExpr_2) }) }) }) }) }) }) }) }) }) }))))
                        in  sem_RowTyExpr_Ext_1)) of
                 { ( sem_RowTyExpr_1) | True ->
                 ( _lhsOrange,sem_RowTyExpr_1) }) }) }) }) })

sem_RowTyExpr_Var :: Range ->
                     HsName ->
                     T_RowTyExpr 

sem_RowTyExpr_Var hsrange_ nm_  | hsrange_ `seq` (nm_ `seq` (True)) =
    (case (hsrange_) of
     { _range | _range `seq` (True) ->
     (case (_range) of
      { _lhsOrange | _lhsOrange `seq` (True) ->
      (case ((let sem_RowTyExpr_Var_1 :: T_RowTyExpr_1 
                  sem_RowTyExpr_Var_1  =
                      (\ _lhsIgUniq
                         _lhsIpositionalFldNmL ->
                           _lhsIgUniq `seq`
                           (_lhsIpositionalFldNmL `seq`
                            ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq2) -> case nextUnique __cont of { (__cont, lUniq_ki) -> (__cont, lUniq,lUniq2,lUniq_ki)}}} )) of
                              { __tup216 | __tup216 `seq` (True) ->
                              (case (__tup216) of
                               { (_lhsOgUniq,_,_,_) | _lhsOgUniq `seq` (True) ->
                               (case (_lhsIpositionalFldNmL) of
                                { _lhsOpositionalFldNmL | _lhsOpositionalFldNmL `seq` (True) ->
                                (case ((let sem_RowTyExpr_Var_2 :: T_RowTyExpr_2 
                                            sem_RowTyExpr_Var_2  =
                                                (\ _lhsItyGam ->
                                                     _lhsItyGam `seq`
                                                     ((case (__tup216) of
                                                       { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                       (case (tyGamLookupOrAdd _lUniq nm_ _lhsItyGam _lhsItyGam) of
                                                        { __tup214 | __tup214 `seq` (True) ->
                                                        (case (__tup214) of
                                                         { (_,_lhsOtyGam) | _lhsOtyGam `seq` (True) ->
                                                         (case (__tup214) of
                                                          { (_tgi_,_) | _tgi_ `seq` (True) ->
                                                          (case (tgiTy _tgi_) of
                                                           { _lhsOtyRow | _lhsOtyRow `seq` (True) ->
                                                           (case ((let sem_RowTyExpr_Var_3 :: T_RowTyExpr_3 
                                                                       sem_RowTyExpr_Var_3  =
                                                                           (\ _lhsIknPolCtx
                                                                              _lhsIpolGam
                                                                              _lhsIpolVarMp ->
                                                                                _lhsIknPolCtx `seq`
                                                                                (_lhsIpolGam `seq`
                                                                                 (_lhsIpolVarMp `seq`
                                                                                  ((case (_lhsIpolVarMp) of
                                                                                    { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                    (case ((let sem_RowTyExpr_Var_4 :: T_RowTyExpr_4 
                                                                                                sem_RowTyExpr_Var_4  =
                                                                                                    (\ _lhsItyKiGam ->
                                                                                                         _lhsItyKiGam `seq`
                                                                                                         ((case (__tup216) of
                                                                                                           { (_,_,_,_lUniq_ki) | _lUniq_ki `seq` (True) ->
                                                                                                           (case (tyKiGamLookupOrAdd _lUniq_ki (tgiTy _tgi_) _lhsItyKiGam) of
                                                                                                            { __tup215 | __tup215 `seq` (True) ->
                                                                                                            (case (__tup215) of
                                                                                                             { (_,_,_tyKiGamNew) | _tyKiGamNew `seq` (True) ->
                                                                                                             (case (_tyKiGamNew) of
                                                                                                              { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                              (case (__tup215) of
                                                                                                               { (_,_lhsOtyKiGam,_) | _lhsOtyKiGam `seq` (True) ->
                                                                                                               (case ((let sem_RowTyExpr_Var_5 :: T_RowTyExpr_5 
                                                                                                                           sem_RowTyExpr_Var_5  =
                                                                                                                               (\ _lhsIkiVarMp ->
                                                                                                                                    _lhsIkiVarMp `seq`
                                                                                                                                    ((case (__tup216) of
                                                                                                                                      { (_,_,_lUniq2,_) | _lUniq2 `seq` (True) ->
                                                                                                                                      (case (__tup215) of
                                                                                                                                       { (_tkgi_,_,_) | _tkgi_ `seq` (True) ->
                                                                                                                                       (case (fitsIn weakFIOpts defaultFIEnv _lUniq2 _lhsIkiVarMp (tkgiKi _tkgi_) kiRow) of
                                                                                                                                        { _fo_ | _fo_ `seq` (True) ->
                                                                                                                                        (case (foVarMp _fo_ `varUpd` _lhsIkiVarMp) of
                                                                                                                                         { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                         (case (Map.empty) of
                                                                                                                                          { _lhsOtyVarWildMp | _lhsOtyVarWildMp `seq` (True) ->
                                                                                                                                          (case ((let sem_RowTyExpr_Var_6 :: T_RowTyExpr_6 
                                                                                                                                                      sem_RowTyExpr_Var_6  =
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
                                                                                                                                                                             (case (pp nm_) of
                                                                                                                                                                              { _pp | _pp `seq` (True) ->
                                                                                                                                                                              (case (rngLift _range mkNestErr' _pp [foErrSq _fo_]) of
                                                                                                                                                                               { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                               (case (hsnUnknown) of
                                                                                                                                                                                { _lhsOextNm | _lhsOextNm `seq` (True) ->
                                                                                                                                                                                (case (Map.empty) of
                                                                                                                                                                                 { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                 (case (_pp) of
                                                                                                                                                                                  { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                  (case ([]) of
                                                                                                                                                                                   { _lhsOppL | _lhsOppL `seq` (True) ->
                                                                                                                                                                                   ( _lhsOallErrSq,_lhsOclMissNmS,_lhsOclNmS,_lhsOerrSq,_lhsOextNm,_lhsOgathMentrelFilterMp,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) })))))))))))))
                                                                                                                                                  in  sem_RowTyExpr_Var_6)) of
                                                                                                                                           { ( sem_RowTyExpr_6) | True ->
                                                                                                                                           ( _lhsOkiVarMp,_lhsOtyVarWildMp,sem_RowTyExpr_6) }) }) }) }) }) })))
                                                                                                                       in  sem_RowTyExpr_Var_5)) of
                                                                                                                { ( sem_RowTyExpr_5) | True ->
                                                                                                                ( _lhsOintlTyKiGam,_lhsOtyKiGam,sem_RowTyExpr_5) }) }) }) }) }) })))
                                                                                            in  sem_RowTyExpr_Var_4)) of
                                                                                     { ( sem_RowTyExpr_4) | True ->
                                                                                     ( _lhsOpolVarMp,sem_RowTyExpr_4) }) })))))
                                                                   in  sem_RowTyExpr_Var_3)) of
                                                            { ( sem_RowTyExpr_3) | True ->
                                                            ( _lhsOtyGam,_lhsOtyRow,sem_RowTyExpr_3) }) }) }) }) }) })))
                                        in  sem_RowTyExpr_Var_2)) of
                                 { ( sem_RowTyExpr_2) | True ->
                                 ( _lhsOgUniq,_lhsOpositionalFldNmL,sem_RowTyExpr_2) }) }) }) }))))
              in  sem_RowTyExpr_Var_1)) of
       { ( sem_RowTyExpr_1) | True ->
       ( _lhsOrange,sem_RowTyExpr_1) }) }) })


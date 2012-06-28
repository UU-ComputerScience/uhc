


module EH101.EH.MainAG_DataConstr where

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

-- DataConstr --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         range                : Range
   visit 1:
      chained attribute:
         gUniq                : UID
   visit 2:
      inherited attributes:
         dataTy               : Ty
         knPolCtx             : Polarity
         polGam               : PolGam
         tyGam                : TyGam
      chained attribute:
         polVarMp             : VarMp
      synthesized attribute:
         dataAltTyL           : AssocL HsName Ty
   visit 3:
      inherited attribute:
         tyKiGam              : TyKiGam
      chained attribute:
         kiVarMp              : VarMp
      synthesized attributes:
         gathMaxArity         : Int
         intlTyKiGam          : TyKiGam
   visit 4:
      inherited attributes:
         dataAltTy            : Ty
         maxArity             : Int
         tyNm                 : HsName
      synthesized attributes:
         dataAltForNewType    : Ty
         dataConstrNmL        : [HsName]
         dataConstrTagMp      : DataConstrTagMp
   visit 5:
      inherited attributes:
         finTyKiGam           : TyKiGam
         tvKiVarMp            : VarMp
      chained attributes:
         patTyVarMp           : VarMp
         patValGam            : ValGam
      synthesized attributes:
         fldSelGam            : ValGam
         fldUpdGam            : ValGam
         gathCnstrMp          : CHRPredOccCnstrMp
         gathRangeMp          : RangeMp
   visit 6:
      inherited attributes:
         chrEvidBindMp        : EvidKeyToCBindMap
         chrScopeBindMp       : PredScopeToCBindMap
         chrStore             : ScopedPredStore
         clDfGam              : ClassDefaultGam
         clGam                : ClGam
         finKiVarMp           : VarMp
         finTyVarMp           : VarMp
         isNewtype            : Bool
         kiGam                : KiGam
         lexLev               : Int
         moduleNm             : HsName
         opts                 : EHCOpts
         predScope            : PredScope
         rangeMp              : RangeMp
         tyKiGlobFreeTvarS    : TyVarIdS
         tyTyGlobFreeTvarS    : TyVarIdS
         tyTyTySigFreeTvarS   : TyVarIdS
         valGam               : ValGam
         valTyGlobFreeTvarS   : TyVarIdS
      synthesized attributes:
         allErrSq             : ErrSq
         cbindL               : CBindL
         errSq                : ErrSq
         ffeCBindL            : CBindL
         ffiCBindL            : CBindL
         gathMentrelFilterMp  : ModEntRelFilterMp
         gathTvKiVarMp        : VarMp
         pp                   : PP_Doc
   alternatives:
      alternative Constr:
         child hsrange        : {Range}
         child conNm          : {HsName}
         child mbFixityPrio   : {Maybe (Int,Fixity)}
         child fields         : DataFields 
         child mbGadtTyExpr   : MbTyExpr 
         visit 0:
            local range       : {Range}
         visit 1:
            local _tup10      : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 2:
            local dataConFldTyL : _
            local mbGadtFO    : _
            local gadtTyVarMp : _
            local dataConGadtTyL : _
            local dataConGadtFldTyL : _
            local dataConProdTy2 : _
            local dataConProdTy1 : _
            local dataConProdTy : {Ty}
            local dataAltTyL  : _
            intra _tup10      : {(UID,UID,UID,UID)}
            intra range       : {Range}
         visit 3:
            local arity       : {Int}
            local lUniq       : {UID}
            local fldsKiVarMp : _
            local _tup9       : _
            local fo_         : {FIOut}
            intra dataConGadtFldTyL : _
            intra _tup10      : {(UID,UID,UID,UID)}
            intra dataConGadtTyL : _
            intra dataConFldTyL : _
            intra dataConProdTy : {Ty}
            intra gadtTyVarMp : _
            intra range       : {Range}
            intra mbGadtFO    : _
         visit 4:
            local dataAltForNewType : _
            local lUniq_con   : {UID}
            local tag         : _
            local ctag        : {CTag}
            local fldMp       : _
            local dataConTyAsVar : {Ty}
            local dataConGadtFldAnnL : _
            local dti         : _
            intra dataConGadtFldTyL : _
            intra _tup10      : {(UID,UID,UID,UID)}
            intra arity       : {Int}
            intra dataConGadtTyL : _
            intra dataConFldTyL : _
            intra dataConProdTy : {Ty}
            intra gadtTyVarMp : _
            intra range       : {Range}
            intra mbGadtFO    : _
            intra fo_         : {FIOut}
         visit 5:
            local lUniq_uncon : {UID}
            local dataConTyL  : _
            local dataConTy2  : _
            local dataConTy1  : {Ty}
            local dataConTy   : {Ty}
            local dataUnConTyVar : _
            local dataConTyVar : _
            local dataConProdTyFv : _
            local dataConTyFv : {[TyVarId]}
            local dataQuUnConTy : _
            local dataConGadtTy : _
            local dataUnConTyAsVar : _
            intra _tup10      : {(UID,UID,UID,UID)}
            intra dataConFldTyL : _
            intra lUniq_con   : {UID}
            intra dataConProdTy : {Ty}
            intra gadtTyVarMp : _
            intra dataConTyAsVar : {Ty}
            intra dataConGadtFldTyL : _
            intra range       : {Range}
            intra mbGadtFO    : _
            intra fo_         : {FIOut}
         visit 6:
            local pp          : _
            local dupErrs     : _
            intra dataConGadtFldTyL : _
            intra range       : {Range}
            intra mbGadtFO    : _
            intra fo_         : {FIOut}
-}
sem_DataConstr_Constr :: Range ->
                         HsName ->
                         (Maybe (Int,Fixity)) ->
                         T_DataFields  ->
                         T_MbTyExpr  ->
                         T_DataConstr 

sem_DataConstr_Constr hsrange_ conNm_ mbFixityPrio_ fields_ mbGadtTyExpr_  | hsrange_ `seq` (conNm_ `seq` (mbFixityPrio_ `seq` (fields_ `seq` (mbGadtTyExpr_ `seq` (True))))) =
    (case (mbGadtTyExpr_ ) of
     { ( _mbGadtTyExprIrange,mbGadtTyExpr_1) | True ->
         (case (fields_ ) of
          { ( _fieldsIrange,fields_1) | True ->
              (case (rangeUnions [hsrange_, _fieldsIrange , _mbGadtTyExprIrange
                                                                         ]) of
               { _range | _range `seq` (True) ->
               (case (_range) of
                { _lhsOrange | _lhsOrange `seq` (True) ->
                (case ((let sem_DataConstr_Constr_1 :: T_DataConstr_1 
                            sem_DataConstr_Constr_1  =
                                (\ _lhsIgUniq ->
                                     _lhsIgUniq `seq`
                                     ((case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> case nextUnique __cont of { (__cont, lUniq_con) -> case nextUnique __cont of { (__cont, lUniq_uncon) -> (__cont, lUniq,lUniq_con,lUniq_uncon)}}} )) of
                                       { __tup10 | __tup10 `seq` (True) ->
                                       (case (__tup10) of
                                        { (_fieldsOgUniq,_,_,_) | _fieldsOgUniq `seq` (True) ->
                                        (case (fields_1 _fieldsOgUniq ) of
                                         { ( _fieldsIgUniq,fields_2) | True ->
                                             (case (_fieldsIgUniq) of
                                              { _mbGadtTyExprOgUniq | _mbGadtTyExprOgUniq `seq` (True) ->
                                              (case (mbGadtTyExpr_1 _mbGadtTyExprOgUniq ) of
                                               { ( _mbGadtTyExprIgUniq,mbGadtTyExpr_2) | True ->
                                                   (case (_mbGadtTyExprIgUniq) of
                                                    { _lhsOgUniq | _lhsOgUniq `seq` (True) ->
                                                    (case ((let sem_DataConstr_Constr_2 :: T_DataConstr_2 
                                                                sem_DataConstr_Constr_2  =
                                                                    (\ _lhsIdataTy
                                                                       _lhsIknPolCtx
                                                                       _lhsIpolGam
                                                                       _lhsIpolVarMp
                                                                       _lhsItyGam ->
                                                                         _lhsIdataTy `seq`
                                                                         (_lhsIknPolCtx `seq`
                                                                          (_lhsIpolGam `seq`
                                                                           (_lhsIpolVarMp `seq`
                                                                            (_lhsItyGam `seq`
                                                                             ((case (_lhsItyGam) of
                                                                               { _fieldsOtyGam | _fieldsOtyGam `seq` (True) ->
                                                                               (case (_lhsIpolVarMp) of
                                                                                { _fieldsOpolVarMp | _fieldsOpolVarMp `seq` (True) ->
                                                                                (case (_lhsIpolGam) of
                                                                                 { _fieldsOpolGam | _fieldsOpolGam `seq` (True) ->
                                                                                 (case (_lhsIknPolCtx) of
                                                                                  { _fieldsOknPolCtx | _fieldsOknPolCtx `seq` (True) ->
                                                                                  (case (fields_2 _fieldsOknPolCtx _fieldsOpolGam _fieldsOpolVarMp _fieldsOtyGam ) of
                                                                                   { ( _fieldsIfldAnnL,_fieldsIfldTyL,_fieldsIpolVarMp,_fieldsItyGam,fields_3) | True ->
                                                                                       (case (_fieldsItyGam) of
                                                                                        { _mbGadtTyExprOtyGam | _mbGadtTyExprOtyGam `seq` (True) ->
                                                                                        (case (_fieldsIfldTyL) of
                                                                                         { _dataConFldTyL | _dataConFldTyL `seq` (True) ->
                                                                                         (case (_fieldsIpolVarMp) of
                                                                                          { _mbGadtTyExprOpolVarMp | _mbGadtTyExprOpolVarMp `seq` (True) ->
                                                                                          (case (_lhsIpolGam) of
                                                                                           { _mbGadtTyExprOpolGam | _mbGadtTyExprOpolGam `seq` (True) ->
                                                                                           (case (_lhsIknPolCtx) of
                                                                                            { _mbGadtTyExprOknPolCtx | _mbGadtTyExprOknPolCtx `seq` (True) ->
                                                                                            (case (mbGadtTyExpr_2 _mbGadtTyExprOknPolCtx _mbGadtTyExprOpolGam _mbGadtTyExprOpolVarMp _mbGadtTyExprOtyGam ) of
                                                                                             { ( _mbGadtTyExprIpolVarMp,_mbGadtTyExprItyMb,mbGadtTyExpr_3) | True ->
                                                                                                 (case (fmap (\ty -> fitsIn (unifyFIOpts {fioDontBind = varFreeSet ty}) defaultFIEnv uidStart (emptyVarMp::VarMp) _lhsIdataTy ty) _mbGadtTyExprItyMb) of
                                                                                                  { _mbGadtFO | _mbGadtFO `seq` (True) ->
                                                                                                  (case (maybe emptyVarMp foVarMp _mbGadtFO) of
                                                                                                   { _gadtTyVarMp | _gadtTyVarMp `seq` (True) ->
                                                                                                   (case ([ mkTyPr $ Pred_Eq (mkTyVar v) t | (v,t) <- varmpToAssocTyL _gadtTyVarMp ]) of
                                                                                                    { _dataConGadtTyL | _dataConGadtTyL `seq` (True) ->
                                                                                                    (case (zip (repeat Nothing) _dataConGadtTyL ++ _dataConFldTyL) of
                                                                                                     { _dataConGadtFldTyL | _dataConGadtFldTyL `seq` (True) ->
                                                                                                     (case (let  lbls = zipWith (\p (ml,_) -> maybe p id ml) positionalFldNames _dataConGadtFldTyL
                                                                                                            in   mkTyRec (zipWith (\l (_,t) -> (l,t)) lbls _dataConGadtFldTyL)) of
                                                                                                      { _dataConProdTy2 | _dataConProdTy2 `seq` (True) ->
                                                                                                      (case (_dataConProdTy2) of
                                                                                                       { _dataConProdTy1 | _dataConProdTy1 `seq` (True) ->
                                                                                                       (case (_dataConProdTy1) of
                                                                                                        { _dataConProdTy | _dataConProdTy `seq` (True) ->
                                                                                                        (case ([(conNm_,_dataConProdTy)]) of
                                                                                                         { _dataAltTyL | _dataAltTyL `seq` (True) ->
                                                                                                         (case (_dataAltTyL) of
                                                                                                          { _lhsOdataAltTyL | _lhsOdataAltTyL `seq` (True) ->
                                                                                                          (case (_mbGadtTyExprIpolVarMp) of
                                                                                                           { _lhsOpolVarMp | _lhsOpolVarMp `seq` (True) ->
                                                                                                           (case ((let sem_DataConstr_Constr_3 :: T_DataConstr_3 
                                                                                                                       sem_DataConstr_Constr_3  =
                                                                                                                           (\ _lhsIkiVarMp
                                                                                                                              _lhsItyKiGam ->
                                                                                                                                _lhsIkiVarMp `seq`
                                                                                                                                (_lhsItyKiGam `seq`
                                                                                                                                 ((case (length _dataConGadtFldTyL) of
                                                                                                                                   { _arity | _arity `seq` (True) ->
                                                                                                                                   (case (_arity) of
                                                                                                                                    { _lhsOgathMaxArity | _lhsOgathMaxArity `seq` (True) ->
                                                                                                                                    (case (_lhsItyKiGam) of
                                                                                                                                     { _fieldsOtyKiGam | _fieldsOtyKiGam `seq` (True) ->
                                                                                                                                     (case (fields_3 _fieldsOtyKiGam ) of
                                                                                                                                      { ( _fieldsIintlTyKiGam,_fieldsItyKiGam,fields_4) | True ->
                                                                                                                                          (case (_fieldsItyKiGam) of
                                                                                                                                           { _mbGadtTyExprOtyKiGam | _mbGadtTyExprOtyKiGam `seq` (True) ->
                                                                                                                                           (case (_lhsIkiVarMp) of
                                                                                                                                            { _fieldsOkiVarMp | _fieldsOkiVarMp `seq` (True) ->
                                                                                                                                            (case (__tup10) of
                                                                                                                                             { (_,_lUniq,_,_) | _lUniq `seq` (True) ->
                                                                                                                                             (case (_lhsIdataTy) of
                                                                                                                                              { _fieldsOdataTy | _fieldsOdataTy `seq` (True) ->
                                                                                                                                              (case (fields_4 _fieldsOdataTy _fieldsOkiVarMp ) of
                                                                                                                                               { ( _fieldsIfldSelGam,_fieldsIfldUpdGam,_fieldsIgathCnstrMp,_fieldsIgathRangeMp,_fieldsIkiL,_fieldsIkiVarMp,fields_5) | True ->
                                                                                                                                                   (case (_fieldsIkiVarMp) of
                                                                                                                                                    { _fldsKiVarMp | _fldsKiVarMp `seq` (True) ->
                                                                                                                                                    (case (fitsInL  weakFIOpts defaultFIEnv _lUniq _fldsKiVarMp
                                                                                                                                                                    _fieldsIkiL (repeat kiStar)) of
                                                                                                                                                     { __tup9 | __tup9 `seq` (True) ->
                                                                                                                                                     (case (__tup9) of
                                                                                                                                                      { (_,_fo_) | _fo_ `seq` (True) ->
                                                                                                                                                      (case (foVarMp _fo_ `varUpd` _fldsKiVarMp) of
                                                                                                                                                       { _mbGadtTyExprOkiVarMp | _mbGadtTyExprOkiVarMp `seq` (True) ->
                                                                                                                                                       (case (mbGadtTyExpr_3 _mbGadtTyExprOkiVarMp _mbGadtTyExprOtyKiGam ) of
                                                                                                                                                        { ( _mbGadtTyExprIintlTyKiGam,_mbGadtTyExprIkiVarMp,mbGadtTyExpr_4) | True ->
                                                                                                                                                            (case (_fieldsIintlTyKiGam `gamUnion` _mbGadtTyExprIintlTyKiGam) of
                                                                                                                                                             { _lhsOintlTyKiGam | _lhsOintlTyKiGam `seq` (True) ->
                                                                                                                                                             (case (_mbGadtTyExprIkiVarMp) of
                                                                                                                                                              { _lhsOkiVarMp | _lhsOkiVarMp `seq` (True) ->
                                                                                                                                                              (case ((let sem_DataConstr_Constr_4 :: T_DataConstr_4 
                                                                                                                                                                          sem_DataConstr_Constr_4  =
                                                                                                                                                                              (\ _lhsIdataAltTy
                                                                                                                                                                                 _lhsImaxArity
                                                                                                                                                                                 _lhsItyNm ->
                                                                                                                                                                                   _lhsIdataAltTy `seq`
                                                                                                                                                                                   (_lhsImaxArity `seq`
                                                                                                                                                                                    (_lhsItyNm `seq`
                                                                                                                                                                                     ((case (maybeHd Ty_Any snd _dataConGadtFldTyL) of
                                                                                                                                                                                       { _dataAltForNewType | _dataAltForNewType `seq` (True) ->
                                                                                                                                                                                       (case (_dataAltForNewType) of
                                                                                                                                                                                        { _lhsOdataAltForNewType | _lhsOdataAltForNewType `seq` (True) ->
                                                                                                                                                                                        (case ([conNm_]) of
                                                                                                                                                                                         { _lhsOdataConstrNmL | _lhsOdataConstrNmL `seq` (True) ->
                                                                                                                                                                                         (case (__tup10) of
                                                                                                                                                                                          { (_,_,_lUniq_con,_) | _lUniq_con `seq` (True) ->
                                                                                                                                                                                          (case (tyRecOffset conNm_ _lhsIdataAltTy) of
                                                                                                                                                                                           { _tag | _tag `seq` (True) ->
                                                                                                                                                                                           (case (CTag _lhsItyNm conNm_ _tag _arity _lhsImaxArity) of
                                                                                                                                                                                            { _ctag | _ctag `seq` (True) ->
                                                                                                                                                                                            (case (let mk o = emptyDataFldInfo {dfiOffset = o}
                                                                                                                                                                                                       mkfs = fst . foldl (\(m,o) (ml,_) -> maybe (m,o+1) (\l -> ((l,mk o):m,o+1)) ml) ([],0)
                                                                                                                                                                                                   in  Map.fromList $ mkfs $ _dataConGadtFldTyL) of
                                                                                                                                                                                             { _fldMp | _fldMp `seq` (True) ->
                                                                                                                                                                                             (case (mkNewTyVar _lUniq_con) of
                                                                                                                                                                                              { _dataConTyAsVar | _dataConTyAsVar `seq` (True) ->
                                                                                                                                                                                              (case (replicate (length _dataConGadtTyL)
                                                                                                                                                                                                               (DataConFldAnnInfo
                                                                                                                                                                                                                 Strictness_NonStrict
                                                                                                                                                                                                               )
                                                                                                                                                                                                     ++ _fieldsIfldAnnL) of
                                                                                                                                                                                               { _dataConGadtFldAnnL | _dataConGadtFldAnnL `seq` (True) ->
                                                                                                                                                                                               (case (emptyDataTagInfo
                                                                                                                                                                                                        { dtiFldMp        = _fldMp
                                                                                                                                                                                                        , dtiFldTyL       = _dataConGadtFldTyL
                                                                                                                                                                                                        , dtiConFldAnnL   = _dataConGadtFldAnnL
                                                                                                                                                                                                        , dtiConNm        = conNm_
                                                                                                                                                                                                        , dtiConTy        = _dataConTyAsVar
                                                                                                                                                                                                        , dtiCTag         = _ctag
                                                                                                                                                                                                        , dtiMbFixityPrio = mbFixityPrio_
                                                                                                                                                                                                        }) of
                                                                                                                                                                                                { _dti | _dti `seq` (True) ->
                                                                                                                                                                                                (case (conNm_ `Map.singleton` _dti) of
                                                                                                                                                                                                 { _lhsOdataConstrTagMp | _lhsOdataConstrTagMp `seq` (True) ->
                                                                                                                                                                                                 (case ((let sem_DataConstr_Constr_5 :: T_DataConstr_5 
                                                                                                                                                                                                             sem_DataConstr_Constr_5  =
                                                                                                                                                                                                                 (\ _lhsIfinTyKiGam
                                                                                                                                                                                                                    _lhsIpatTyVarMp
                                                                                                                                                                                                                    _lhsIpatValGam
                                                                                                                                                                                                                    _lhsItvKiVarMp ->
                                                                                                                                                                                                                      _lhsIfinTyKiGam `seq`
                                                                                                                                                                                                                      (_lhsIpatTyVarMp `seq`
                                                                                                                                                                                                                       (_lhsIpatValGam `seq`
                                                                                                                                                                                                                        (_lhsItvKiVarMp `seq`
                                                                                                                                                                                                                         ((case (_fieldsIfldSelGam) of
                                                                                                                                                                                                                           { _lhsOfldSelGam | _lhsOfldSelGam `seq` (True) ->
                                                                                                                                                                                                                           (case (_fieldsIfldUpdGam) of
                                                                                                                                                                                                                            { _lhsOfldUpdGam | _lhsOfldUpdGam `seq` (True) ->
                                                                                                                                                                                                                            (case (_fieldsIgathCnstrMp) of
                                                                                                                                                                                                                             { _lhsOgathCnstrMp | _lhsOgathCnstrMp `seq` (True) ->
                                                                                                                                                                                                                             (case (_fieldsIgathRangeMp) of
                                                                                                                                                                                                                              { _lhsOgathRangeMp | _lhsOgathRangeMp `seq` (True) ->
                                                                                                                                                                                                                              (case (__tup10) of
                                                                                                                                                                                                                               { (_,_,_,_lUniq_uncon) | _lUniq_uncon `seq` (True) ->
                                                                                                                                                                                                                               (case (assocLElts _dataConFldTyL) of
                                                                                                                                                                                                                                { _dataConTyL | _dataConTyL `seq` (True) ->
                                                                                                                                                                                                                                (case (_dataConTyL `mkArrow` _lhsIdataTy) of
                                                                                                                                                                                                                                 { _dataConTy2 | _dataConTy2 `seq` (True) ->
                                                                                                                                                                                                                                 (case (_dataConTy2) of
                                                                                                                                                                                                                                  { _dataConTy1 | _dataConTy1 `seq` (True) ->
                                                                                                                                                                                                                                  (case (_dataConTy1) of
                                                                                                                                                                                                                                   { _dataConTy | _dataConTy `seq` (True) ->
                                                                                                                                                                                                                                   (case (_lUniq_uncon) of
                                                                                                                                                                                                                                    { _dataUnConTyVar | _dataUnConTyVar `seq` (True) ->
                                                                                                                                                                                                                                    (case (_lUniq_con) of
                                                                                                                                                                                                                                     { _dataConTyVar | _dataConTyVar `seq` (True) ->
                                                                                                                                                                                                                                     (case (varFree _dataConProdTy) of
                                                                                                                                                                                                                                      { _dataConProdTyFv | _dataConProdTyFv `seq` (True) ->
                                                                                                                                                                                                                                      (case (varFree _lhsIdataTy) of
                                                                                                                                                                                                                                       { _dataConTyFv | _dataConTyFv `seq` (True) ->
                                                                                                                                                                                                                                       (case (let fvD = _dataConTyFv
                                                                                                                                                                                                                                                  fvU = _dataConProdTyFv
                                                                                                                                                                                                                                                  tkg = gamUnion _lhsIfinTyKiGam _fieldsIintlTyKiGam
                                                                                                                                                                                                                                                  mkk tvs = [ (v,tvarKi tkg _lhsItvKiVarMp emptyVarMp v) | v <- tvs ]
                                                                                                                                                                                                                                              in  (                                   ([_lhsIdataTy] `mkArrow` mkTyQu tyQu_Exists (mkk (fvU \\ fvD)) _dataConProdTy))) of
                                                                                                                                                                                                                                        { _dataQuUnConTy | _dataQuUnConTy `seq` (True) ->
                                                                                                                                                                                                                                        (case (_gadtTyVarMp `varUpd` _dataConTy) of
                                                                                                                                                                                                                                         { _dataConGadtTy | _dataConGadtTy `seq` (True) ->
                                                                                                                                                                                                                                         (case (assocTyLToVarMp [(_dataConTyVar,_dataConGadtTy),(_dataUnConTyVar,_dataQuUnConTy)] `varUpd` _lhsIpatTyVarMp) of
                                                                                                                                                                                                                                          { _lhsOpatTyVarMp | _lhsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                          (case (mkNewTyVar _lUniq_uncon) of
                                                                                                                                                                                                                                           { _dataUnConTyAsVar | _dataUnConTyAsVar `seq` (True) ->
                                                                                                                                                                                                                                           (case (gamUnions
                                                                                                                                                                                                                                                    [ assocLToGam
                                                                                                                                                                                                                                                        [ (conNm_, ValGamInfo _dataConTyAsVar)
                                                                                                                                                                                                                                                        , (hsnUn conNm_, ValGamInfo _dataUnConTyAsVar)
                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                    , _lhsIpatValGam
                                                                                                                                                                                                                                                    ]) of
                                                                                                                                                                                                                                            { _lhsOpatValGam | _lhsOpatValGam `seq` (True) ->
                                                                                                                                                                                                                                            (case ((let sem_DataConstr_Constr_6 :: T_DataConstr_6 
                                                                                                                                                                                                                                                        sem_DataConstr_Constr_6  =
                                                                                                                                                                                                                                                            (\ _lhsIchrEvidBindMp
                                                                                                                                                                                                                                                               _lhsIchrScopeBindMp
                                                                                                                                                                                                                                                               _lhsIchrStore
                                                                                                                                                                                                                                                               _lhsIclDfGam
                                                                                                                                                                                                                                                               _lhsIclGam
                                                                                                                                                                                                                                                               _lhsIfinKiVarMp
                                                                                                                                                                                                                                                               _lhsIfinTyVarMp
                                                                                                                                                                                                                                                               _lhsIisNewtype
                                                                                                                                                                                                                                                               _lhsIkiGam
                                                                                                                                                                                                                                                               _lhsIlexLev
                                                                                                                                                                                                                                                               _lhsImoduleNm
                                                                                                                                                                                                                                                               _lhsIopts
                                                                                                                                                                                                                                                               _lhsIpredScope
                                                                                                                                                                                                                                                               _lhsIrangeMp
                                                                                                                                                                                                                                                               _lhsItyKiGlobFreeTvarS
                                                                                                                                                                                                                                                               _lhsItyTyGlobFreeTvarS
                                                                                                                                                                                                                                                               _lhsItyTyTySigFreeTvarS
                                                                                                                                                                                                                                                               _lhsIvalGam
                                                                                                                                                                                                                                                               _lhsIvalTyGlobFreeTvarS ->
                                                                                                                                                                                                                                                                 _lhsIchrEvidBindMp `seq`
                                                                                                                                                                                                                                                                 (_lhsIchrScopeBindMp `seq`
                                                                                                                                                                                                                                                                  (_lhsIchrStore `seq`
                                                                                                                                                                                                                                                                   (_lhsIclDfGam `seq`
                                                                                                                                                                                                                                                                    (_lhsIclGam `seq`
                                                                                                                                                                                                                                                                     (_lhsIfinKiVarMp `seq`
                                                                                                                                                                                                                                                                      (_lhsIfinTyVarMp `seq`
                                                                                                                                                                                                                                                                       (_lhsIisNewtype `seq`
                                                                                                                                                                                                                                                                        (_lhsIkiGam `seq`
                                                                                                                                                                                                                                                                         (_lhsIlexLev `seq`
                                                                                                                                                                                                                                                                          (_lhsImoduleNm `seq`
                                                                                                                                                                                                                                                                           (_lhsIopts `seq`
                                                                                                                                                                                                                                                                            (_lhsIpredScope `seq`
                                                                                                                                                                                                                                                                             (_lhsIrangeMp `seq`
                                                                                                                                                                                                                                                                              (_lhsItyKiGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                               (_lhsItyTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                (_lhsItyTyTySigFreeTvarS `seq`
                                                                                                                                                                                                                                                                                 (_lhsIvalGam `seq`
                                                                                                                                                                                                                                                                                  (_lhsIvalTyGlobFreeTvarS `seq`
                                                                                                                                                                                                                                                                                   ((case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                     { _mbGadtTyExprOvalTyGlobFreeTvarS | _mbGadtTyExprOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                      { _mbGadtTyExprOtyTyTySigFreeTvarS | _mbGadtTyExprOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                       { _mbGadtTyExprOtyTyGlobFreeTvarS | _mbGadtTyExprOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                        { _mbGadtTyExprOtyKiGlobFreeTvarS | _mbGadtTyExprOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                        (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                         { _mbGadtTyExprOopts | _mbGadtTyExprOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                         (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                          { _mbGadtTyExprOmoduleNm | _mbGadtTyExprOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                          (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                           { _mbGadtTyExprOkiGam | _mbGadtTyExprOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                           (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                            { _mbGadtTyExprOfinTyVarMp | _mbGadtTyExprOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                            (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                             { _mbGadtTyExprOfinTyKiGam | _mbGadtTyExprOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                             (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                              { _mbGadtTyExprOfinKiVarMp | _mbGadtTyExprOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                              (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                                               { _mbGadtTyExprOclGam | _mbGadtTyExprOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                                               (case (mbGadtTyExpr_4 _mbGadtTyExprOclGam _mbGadtTyExprOfinKiVarMp _mbGadtTyExprOfinTyKiGam _mbGadtTyExprOfinTyVarMp _mbGadtTyExprOkiGam _mbGadtTyExprOmoduleNm _mbGadtTyExprOopts _mbGadtTyExprOtyKiGlobFreeTvarS _mbGadtTyExprOtyTyGlobFreeTvarS _mbGadtTyExprOtyTyTySigFreeTvarS _mbGadtTyExprOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                { ( _mbGadtTyExprIallErrSq,_mbGadtTyExprIclMissNmS,_mbGadtTyExprIclNmS,_mbGadtTyExprIerrSq,_mbGadtTyExprIgathMentrelFilterMp,_mbGadtTyExprIpp,_mbGadtTyExprIppMb,_mbGadtTyExprItyGam,_mbGadtTyExprItyKiGam,_mbGadtTyExprItyVarWildMp) | True ->
                                                                                                                                                                                                                                                                                                    (case (_lhsIvalTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                     { _fieldsOvalTyGlobFreeTvarS | _fieldsOvalTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                     (case (_lhsItyTyTySigFreeTvarS) of
                                                                                                                                                                                                                                                                                                      { _fieldsOtyTyTySigFreeTvarS | _fieldsOtyTyTySigFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                      (case (_lhsItyTyGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                       { _fieldsOtyTyGlobFreeTvarS | _fieldsOtyTyGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                       (case (_lhsItyKiGlobFreeTvarS) of
                                                                                                                                                                                                                                                                                                        { _fieldsOtyKiGlobFreeTvarS | _fieldsOtyKiGlobFreeTvarS `seq` (True) ->
                                                                                                                                                                                                                                                                                                        (case (_lhsItvKiVarMp) of
                                                                                                                                                                                                                                                                                                         { _fieldsOtvKiVarMp | _fieldsOtvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                         (case (_lhsIrangeMp) of
                                                                                                                                                                                                                                                                                                          { _fieldsOrangeMp | _fieldsOrangeMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                          (case (_lhsIpredScope) of
                                                                                                                                                                                                                                                                                                           { _fieldsOpredScope | _fieldsOpredScope `seq` (True) ->
                                                                                                                                                                                                                                                                                                           (case (_lhsIpatTyVarMp) of
                                                                                                                                                                                                                                                                                                            { _fieldsOpatTyVarMp | _fieldsOpatTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                            (case (_lhsIopts) of
                                                                                                                                                                                                                                                                                                             { _fieldsOopts | _fieldsOopts `seq` (True) ->
                                                                                                                                                                                                                                                                                                             (case (_lhsImoduleNm) of
                                                                                                                                                                                                                                                                                                              { _fieldsOmoduleNm | _fieldsOmoduleNm `seq` (True) ->
                                                                                                                                                                                                                                                                                                              (case (_lhsIlexLev) of
                                                                                                                                                                                                                                                                                                               { _fieldsOlexLev | _fieldsOlexLev `seq` (True) ->
                                                                                                                                                                                                                                                                                                               (case (_lhsIkiGam) of
                                                                                                                                                                                                                                                                                                                { _fieldsOkiGam | _fieldsOkiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                (case (_lhsIfinTyVarMp) of
                                                                                                                                                                                                                                                                                                                 { _fieldsOfinTyVarMp | _fieldsOfinTyVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                 (case (_lhsIfinTyKiGam) of
                                                                                                                                                                                                                                                                                                                  { _fieldsOfinTyKiGam | _fieldsOfinTyKiGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                  (case (_lhsIfinKiVarMp) of
                                                                                                                                                                                                                                                                                                                   { _fieldsOfinKiVarMp | _fieldsOfinKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                   (case (_lhsIclGam) of
                                                                                                                                                                                                                                                                                                                    { _fieldsOclGam | _fieldsOclGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                    (case (_lhsIclDfGam) of
                                                                                                                                                                                                                                                                                                                     { _fieldsOclDfGam | _fieldsOclDfGam `seq` (True) ->
                                                                                                                                                                                                                                                                                                                     (case (_lhsIchrStore) of
                                                                                                                                                                                                                                                                                                                      { _fieldsOchrStore | _fieldsOchrStore `seq` (True) ->
                                                                                                                                                                                                                                                                                                                      (case (_lhsIchrScopeBindMp) of
                                                                                                                                                                                                                                                                                                                       { _fieldsOchrScopeBindMp | _fieldsOchrScopeBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                       (case (_lhsIchrEvidBindMp) of
                                                                                                                                                                                                                                                                                                                        { _fieldsOchrEvidBindMp | _fieldsOchrEvidBindMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                        (case (fields_5 _fieldsOchrEvidBindMp _fieldsOchrScopeBindMp _fieldsOchrStore _fieldsOclDfGam _fieldsOclGam _fieldsOfinKiVarMp _fieldsOfinTyKiGam _fieldsOfinTyVarMp _fieldsOkiGam _fieldsOlexLev _fieldsOmoduleNm _fieldsOopts _fieldsOpatTyVarMp _fieldsOpredScope _fieldsOrangeMp _fieldsOtvKiVarMp _fieldsOtyKiGlobFreeTvarS _fieldsOtyTyGlobFreeTvarS _fieldsOtyTyTySigFreeTvarS _fieldsOvalTyGlobFreeTvarS ) of
                                                                                                                                                                                                                                                                                                                         { ( _fieldsIallErrSq,_fieldsIerrSq,_fieldsIgathMentrelFilterMp,_fieldsIgathTvKiVarMp,_fieldsIpatTyVarMp,_fieldsIpp,_fieldsIppL) | True ->
                                                                                                                                                                                                                                                                                                                             (case (_fieldsIallErrSq `Seq.union` _mbGadtTyExprIallErrSq) of
                                                                                                                                                                                                                                                                                                                              { _lhsOallErrSq | _lhsOallErrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                              (case ([]) of
                                                                                                                                                                                                                                                                                                                               { _lhsOcbindL | _lhsOcbindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                               (case (pp conNm_
                                                                                                                                                                                                                                                                                                                                      >#<  (if null $ catMaybes $ assocLKeys $ _dataConGadtFldTyL
                                                                                                                                                                                                                                                                                                                                            then ppSpaced _fieldsIppL
                                                                                                                                                                                                                                                                                                                                            else ppCurlysCommas' _fieldsIppL
                                                                                                                                                                                                                                                                                                                                           )
                                                                                                                                                                                                                                                                                                                                                              >|< maybe empty (\t -> " ->" >#< t) _mbGadtTyExprIppMb) of
                                                                                                                                                                                                                                                                                                                                { _pp | _pp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                (case (let  nms = assocLKeys $ gamToOnlyDups _fieldsIfldSelGam
                                                                                                                                                                                                                                                                                                                                       in   if null nms then [] else [rngLift _range Err_NamesDupIntrod "data field" (zip nms (repeat Nothing))]) of
                                                                                                                                                                                                                                                                                                                                 { _dupErrs | _dupErrs `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                 (case (rngLift _range mkNestErr' _pp [ _fieldsIerrSq
                                                                                                                                                                                                                                                                                                                                                                      , _mbGadtTyExprIerrSq
                                                                                                                                                                                                                                                                                                                                                                      , Seq.fromList _dupErrs
                                                                                                                                                                                                                                                                                                                                                                      , foErrSq _fo_
                                                                                                                                                                                                                                                                                                                                                                      , maybe Seq.empty foErrSq _mbGadtFO
                                                                                                                                                                                                                                                                                                                                                                      ]) of
                                                                                                                                                                                                                                                                                                                                  { _lhsOerrSq | _lhsOerrSq `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                  (case ([]) of
                                                                                                                                                                                                                                                                                                                                   { _lhsOffeCBindL | _lhsOffeCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                   (case ([]) of
                                                                                                                                                                                                                                                                                                                                    { _lhsOffiCBindL | _lhsOffiCBindL `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                    (case (_fieldsIgathMentrelFilterMp `mentrelFilterMpUnion` _mbGadtTyExprIgathMentrelFilterMp) of
                                                                                                                                                                                                                                                                                                                                     { _lhsOgathMentrelFilterMp | _lhsOgathMentrelFilterMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                     (case (_fieldsIgathTvKiVarMp) of
                                                                                                                                                                                                                                                                                                                                      { _lhsOgathTvKiVarMp | _lhsOgathTvKiVarMp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                      (case (_pp) of
                                                                                                                                                                                                                                                                                                                                       { _lhsOpp | _lhsOpp `seq` (True) ->
                                                                                                                                                                                                                                                                                                                                       ( _lhsOallErrSq,_lhsOcbindL,_lhsOerrSq,_lhsOffeCBindL,_lhsOffiCBindL,_lhsOgathMentrelFilterMp,_lhsOgathTvKiVarMp,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))))))))))))))))
                                                                                                                                                                                                                                                    in  sem_DataConstr_Constr_6)) of
                                                                                                                                                                                                                                             { ( sem_DataConstr_6) | True ->
                                                                                                                                                                                                                                             ( _lhsOfldSelGam,_lhsOfldUpdGam,_lhsOgathCnstrMp,_lhsOgathRangeMp,_lhsOpatTyVarMp,_lhsOpatValGam,sem_DataConstr_6) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))))
                                                                                                                                                                                                         in  sem_DataConstr_Constr_5)) of
                                                                                                                                                                                                  { ( sem_DataConstr_5) | True ->
                                                                                                                                                                                                  ( _lhsOdataAltForNewType,_lhsOdataConstrNmL,_lhsOdataConstrTagMp,sem_DataConstr_5) }) }) }) }) }) }) }) }) }) }) }) })))))
                                                                                                                                                                      in  sem_DataConstr_Constr_4)) of
                                                                                                                                                               { ( sem_DataConstr_4) | True ->
                                                                                                                                                               ( _lhsOgathMaxArity,_lhsOintlTyKiGam,_lhsOkiVarMp,sem_DataConstr_4) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))))
                                                                                                                   in  sem_DataConstr_Constr_3)) of
                                                                                                            { ( sem_DataConstr_3) | True ->
                                                                                                            ( _lhsOdataAltTyL,_lhsOpolVarMp,sem_DataConstr_3) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))))))
                                                            in  sem_DataConstr_Constr_2)) of
                                                     { ( sem_DataConstr_2) | True ->
                                                     ( _lhsOgUniq,sem_DataConstr_2) }) }) }) }) }) }) })))
                        in  sem_DataConstr_Constr_1)) of
                 { ( sem_DataConstr_1) | True ->
                 ( _lhsOrange,sem_DataConstr_1) }) }) }) }) })


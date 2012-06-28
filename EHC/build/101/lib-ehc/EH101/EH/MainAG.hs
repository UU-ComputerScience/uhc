

module EH101.EH.MainAG where

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
import EH101.EH.MainAG_AGItf
import EH101.EH.MainAG_CaseAlt
import EH101.EH.MainAG_CaseAlts
import EH101.EH.MainAG_DataConstr
import EH101.EH.MainAG_DataConstrs
import EH101.EH.MainAG_DataField
import EH101.EH.MainAG_DataFieldExpr
import EH101.EH.MainAG_DataFieldPatExpr
import EH101.EH.MainAG_DataFields
import EH101.EH.MainAG_Decl
import EH101.EH.MainAG_Decls
import EH101.EH.MainAG_Expr
import EH101.EH.MainAG_ExprAnn
import EH101.EH.MainAG_FuncDep
import EH101.EH.MainAG_FuncDeps
import EH101.EH.MainAG_KiExpr
import EH101.EH.MainAG_KiExprAnn
import EH101.EH.MainAG_MbTyExpr
import EH101.EH.MainAG_PatExpr
import EH101.EH.MainAG_PatExprAnn
import EH101.EH.MainAG_PrExpr
import EH101.EH.MainAG_PrExprs
import EH101.EH.MainAG_RecExpr
import EH101.EH.MainAG_RecPatExpr
import EH101.EH.MainAG_RowTyExpr
import EH101.EH.MainAG_TyExpr
import EH101.EH.MainAG_TyExprAnn
import EH101.EH.MainAG_TyExprs
import EH101.EH.MainAG_TyVar
import EH101.EH.MainAG_TyVars
-- AGItf -------------------------------------------------------
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _expr )  | (AGItf_AGItf _expr ) `seq` (True) =
    (sem_AGItf_AGItf (sem_Expr _expr ) )
data Inh_AGItf  = Inh_AGItf {chrStore_Inh_AGItf :: !(ScopedPredStore),clDfGam_Inh_AGItf :: !(ClassDefaultGam),clGam_Inh_AGItf :: !(ClGam),dataGam_Inh_AGItf :: !(DataGam),gUniq_Inh_AGItf :: !(UID),idQualGam_Inh_AGItf :: !(IdQualGam),isMainMod_Inh_AGItf :: !(Bool),kiGam_Inh_AGItf :: !(KiGam),moduleNm_Inh_AGItf :: !(HsName),opts_Inh_AGItf :: !(EHCOpts),polGam_Inh_AGItf :: !(PolGam),tyGam_Inh_AGItf :: !(TyGam),tyKiGam_Inh_AGItf :: !(TyKiGam),valGam_Inh_AGItf :: !(ValGam)}
data Syn_AGItf  = Syn_AGItf {allErrSq_Syn_AGItf :: !(ErrSq),cmodule_Syn_AGItf :: !(CModule),gUniq_Syn_AGItf :: !(UID),gathChrStore_Syn_AGItf :: !(ScopedPredStore),gathClDfGam_Syn_AGItf :: !(ClassDefaultGam),gathClGam_Syn_AGItf :: !(ClGam),gathDataGam_Syn_AGItf :: !(DataGam),gathHiddenExports_Syn_AGItf :: !((Seq.Seq (HsName,IdOccKind))),gathKiGam_Syn_AGItf :: !(KiGam),gathLamMp_Syn_AGItf :: !(LamMp),gathMentrelFilterMp_Syn_AGItf :: !(ModEntRelFilterMp),gathPolGam_Syn_AGItf :: !(PolGam),gathTyGam_Syn_AGItf :: !(TyGam),gathTyKiGam_Syn_AGItf :: !(TyKiGam),gathValGam_Syn_AGItf :: !(ValGam),mbOrphan_Syn_AGItf :: !((Maybe (Set.Set HsName))),pp_Syn_AGItf :: !(PP_Doc),topTyPP_Syn_AGItf :: !(PP_Doc)}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIchrStore _lhsIclDfGam _lhsIclGam _lhsIdataGam _lhsIgUniq _lhsIidQualGam _lhsIisMainMod _lhsIkiGam _lhsImoduleNm _lhsIopts _lhsIpolGam _lhsItyGam _lhsItyKiGam _lhsIvalGam )  | sem `seq` ((Inh_AGItf _lhsIchrStore _lhsIclDfGam _lhsIclGam _lhsIdataGam _lhsIgUniq _lhsIidQualGam _lhsIisMainMod _lhsIkiGam _lhsImoduleNm _lhsIopts _lhsIpolGam _lhsItyGam _lhsItyKiGam _lhsIvalGam ) `seq` (True)) =
    (let ( _lhsOallErrSq,_lhsOcmodule,_lhsOgUniq,_lhsOgathChrStore,_lhsOgathClDfGam,_lhsOgathClGam,_lhsOgathDataGam,_lhsOgathHiddenExports,_lhsOgathKiGam,_lhsOgathLamMp,_lhsOgathMentrelFilterMp,_lhsOgathPolGam,_lhsOgathTyGam,_lhsOgathTyKiGam,_lhsOgathValGam,_lhsOmbOrphan,_lhsOpp,_lhsOtopTyPP) | True = sem _lhsIchrStore _lhsIclDfGam _lhsIclGam _lhsIdataGam _lhsIgUniq _lhsIidQualGam _lhsIisMainMod _lhsIkiGam _lhsImoduleNm _lhsIopts _lhsIpolGam _lhsItyGam _lhsItyKiGam _lhsIvalGam 
     in  (Syn_AGItf _lhsOallErrSq _lhsOcmodule _lhsOgUniq _lhsOgathChrStore _lhsOgathClDfGam _lhsOgathClGam _lhsOgathDataGam _lhsOgathHiddenExports _lhsOgathKiGam _lhsOgathLamMp _lhsOgathMentrelFilterMp _lhsOgathPolGam _lhsOgathTyGam _lhsOgathTyKiGam _lhsOgathValGam _lhsOmbOrphan _lhsOpp _lhsOtopTyPP ))
-- CaseAlt -----------------------------------------------------
-- cata
sem_CaseAlt :: CaseAlt  ->
               T_CaseAlt 
sem_CaseAlt (CaseAlt_Pat _hsrange _patExpr _expr )  | (CaseAlt_Pat _hsrange _patExpr _expr ) `seq` (True) =
    (sem_CaseAlt_Pat _hsrange (sem_PatExpr _patExpr ) (sem_Expr _expr ) )

-- CaseAlts ----------------------------------------------------
-- cata
sem_CaseAlts :: CaseAlts  ->
                T_CaseAlts 
sem_CaseAlts list  | list `seq` (True) =
    (Prelude.foldr sem_CaseAlts_Cons sem_CaseAlts_Nil (Prelude.map sem_CaseAlt list) )

-- DataConstr --------------------------------------------------
-- cata
sem_DataConstr :: DataConstr  ->
                  T_DataConstr 
sem_DataConstr (DataConstr_Constr _hsrange _conNm _mbFixityPrio _fields _mbGadtTyExpr )  | (DataConstr_Constr _hsrange _conNm _mbFixityPrio _fields _mbGadtTyExpr ) `seq` (True) =
    (sem_DataConstr_Constr _hsrange _conNm _mbFixityPrio (sem_DataFields _fields ) (sem_MbTyExpr _mbGadtTyExpr ) )

-- DataConstrs -------------------------------------------------
-- cata
sem_DataConstrs :: DataConstrs  ->
                   T_DataConstrs 
sem_DataConstrs list  | list `seq` (True) =
    (Prelude.foldr sem_DataConstrs_Cons sem_DataConstrs_Nil (Prelude.map sem_DataConstr list) )

-- DataField ---------------------------------------------------
-- cata
sem_DataField :: DataField  ->
                 T_DataField 
sem_DataField (DataField_Field _hsrange _mbLabels _tyExpr )  | (DataField_Field _hsrange _mbLabels _tyExpr ) `seq` (True) =
    (sem_DataField_Field _hsrange _mbLabels (sem_TyExpr _tyExpr ) )

-- DataFieldExpr -----------------------------------------------
-- cata
sem_DataFieldExpr :: DataFieldExpr  ->
                     T_DataFieldExpr 
sem_DataFieldExpr (DataFieldExpr_Con _hsrange _nm )  | (DataFieldExpr_Con _hsrange _nm ) `seq` (True) =
    (sem_DataFieldExpr_Con _hsrange _nm )
sem_DataFieldExpr (DataFieldExpr_Expr _hsrange _expr )  | (DataFieldExpr_Expr _hsrange _expr ) `seq` (True) =
    (sem_DataFieldExpr_Expr _hsrange (sem_Expr _expr ) )
sem_DataFieldExpr (DataFieldExpr_Upd _hsrange _dataFieldExpr _nm _expr )  | (DataFieldExpr_Upd _hsrange _dataFieldExpr _nm _expr ) `seq` (True) =
    (sem_DataFieldExpr_Upd _hsrange (sem_DataFieldExpr _dataFieldExpr ) _nm (sem_Expr _expr ) )

-- DataFieldPatExpr --------------------------------------------
-- cata
sem_DataFieldPatExpr :: DataFieldPatExpr  ->
                        T_DataFieldPatExpr 
sem_DataFieldPatExpr (DataFieldPatExpr_Con _hsrange _nm )  | (DataFieldPatExpr_Con _hsrange _nm ) `seq` (True) =
    (sem_DataFieldPatExpr_Con _hsrange _nm )
sem_DataFieldPatExpr (DataFieldPatExpr_Ext _hsrange _dataFieldPatExpr _nm _patExpr )  | (DataFieldPatExpr_Ext _hsrange _dataFieldPatExpr _nm _patExpr ) `seq` (True) =
    (sem_DataFieldPatExpr_Ext _hsrange (sem_DataFieldPatExpr _dataFieldPatExpr ) _nm (sem_PatExpr _patExpr ) )

-- DataFields --------------------------------------------------
-- cata
sem_DataFields :: DataFields  ->
                  T_DataFields 
sem_DataFields list  | list `seq` (True) =
    (Prelude.foldr sem_DataFields_Cons sem_DataFields_Nil (Prelude.map sem_DataField list) )

-- Decl --------------------------------------------------------
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl (Decl_Class _hsrange _tyPrExpr _funcDeps _mbDefaultInstNm _decls _generDerivs )  | (Decl_Class _hsrange _tyPrExpr _funcDeps _mbDefaultInstNm _decls _generDerivs ) `seq` (True) =
    (sem_Decl_Class _hsrange (sem_TyExpr _tyPrExpr ) (sem_FuncDeps _funcDeps ) _mbDefaultInstNm (sem_Decls _decls ) _generDerivs )
sem_Decl (Decl_Data _hsrange _isNewtype _tyNm _tyVars _constrs _mbGenerInfo )  | (Decl_Data _hsrange _isNewtype _tyNm _tyVars _constrs _mbGenerInfo ) `seq` (True) =
    (sem_Decl_Data _hsrange _isNewtype _tyNm (sem_TyVars _tyVars ) (sem_DataConstrs _constrs ) _mbGenerInfo )
sem_Decl (Decl_Default _hsrange _nm _tyExprs )  | (Decl_Default _hsrange _nm _tyExprs ) `seq` (True) =
    (sem_Decl_Default _hsrange _nm (sem_TyExprs _tyExprs ) )
sem_Decl (Decl_FFE _hsrange _nm _callconv _expEnt _expr _tyExpr )  | (Decl_FFE _hsrange _nm _callconv _expEnt _expr _tyExpr ) `seq` (True) =
    (sem_Decl_FFE _hsrange _nm _callconv _expEnt (sem_Expr _expr ) (sem_TyExpr _tyExpr ) )
sem_Decl (Decl_FFI _hsrange _callconv _safety _impEnt _nm _tyExpr )  | (Decl_FFI _hsrange _callconv _safety _impEnt _nm _tyExpr ) `seq` (True) =
    (sem_Decl_FFI _hsrange _callconv _safety _impEnt _nm (sem_TyExpr _tyExpr ) )
sem_Decl (Decl_FusionConv _hsrange _conNm _absNm )  | (Decl_FusionConv _hsrange _conNm _absNm ) `seq` (True) =
    (sem_Decl_FusionConv _hsrange _conNm _absNm )
sem_Decl (Decl_FusionDecl _hsrange _fuseNm )  | (Decl_FusionDecl _hsrange _fuseNm ) `seq` (True) =
    (sem_Decl_FusionDecl _hsrange _fuseNm )
sem_Decl (Decl_GenerRep _hsrange _maxArity _tyNm _conNmL _recFldNmL )  | (Decl_GenerRep _hsrange _maxArity _tyNm _conNmL _recFldNmL ) `seq` (True) =
    (sem_Decl_GenerRep _hsrange _maxArity _tyNm _conNmL _recFldNmL )
sem_Decl (Decl_Instance _hsrange _mbNmElim _instVariant _tyPrExpr _decls )  | (Decl_Instance _hsrange _mbNmElim _instVariant _tyPrExpr _decls ) `seq` (True) =
    (sem_Decl_Instance _hsrange _mbNmElim _instVariant (sem_TyExpr _tyPrExpr ) (sem_Decls _decls ) )
sem_Decl (Decl_InstanceIntro _hsrange _mbNmElim _expr _prExpr )  | (Decl_InstanceIntro _hsrange _mbNmElim _expr _prExpr ) `seq` (True) =
    (sem_Decl_InstanceIntro _hsrange _mbNmElim (sem_Expr _expr ) (sem_PrExpr _prExpr ) )
sem_Decl (Decl_KiSig _hsrange _nm _kiExpr )  | (Decl_KiSig _hsrange _nm _kiExpr ) `seq` (True) =
    (sem_Decl_KiSig _hsrange _nm (sem_KiExpr _kiExpr ) )
sem_Decl (Decl_TySig _hsrange _nm _tyExpr )  | (Decl_TySig _hsrange _nm _tyExpr ) `seq` (True) =
    (sem_Decl_TySig _hsrange _nm (sem_TyExpr _tyExpr ) )
sem_Decl (Decl_Type _hsrange _tyNm _tyExpr )  | (Decl_Type _hsrange _tyNm _tyExpr ) `seq` (True) =
    (sem_Decl_Type _hsrange _tyNm (sem_TyExpr _tyExpr ) )
sem_Decl (Decl_Val _hsrange _patExpr _expr )  | (Decl_Val _hsrange _patExpr _expr ) `seq` (True) =
    (sem_Decl_Val _hsrange (sem_PatExpr _patExpr ) (sem_Expr _expr ) )

-- Decls -------------------------------------------------------
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls list  | list `seq` (True) =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )

-- Expr --------------------------------------------------------
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_Ann _hsrange _ann _expr )  | (Expr_Ann _hsrange _ann _expr ) `seq` (True) =
    (sem_Expr_Ann _hsrange (sem_ExprAnn _ann ) (sem_Expr _expr ) )
sem_Expr (Expr_App _hsrange _func _arg )  | (Expr_App _hsrange _func _arg ) `seq` (True) =
    (sem_Expr_App _hsrange (sem_Expr _func ) (sem_Expr _arg ) )
sem_Expr (Expr_AppImpl _hsrange _func _argPr _arg )  | (Expr_AppImpl _hsrange _func _argPr _arg ) `seq` (True) =
    (sem_Expr_AppImpl _hsrange (sem_Expr _func ) (sem_PrExpr _argPr ) (sem_Expr _arg ) )
sem_Expr (Expr_AppImpred _hsrange _func _arg )  | (Expr_AppImpred _hsrange _func _arg ) `seq` (True) =
    (sem_Expr_AppImpred _hsrange (sem_Expr _func ) (sem_Expr _arg ) )
sem_Expr (Expr_AppTop _hsrange _expr )  | (Expr_AppTop _hsrange _expr ) `seq` (True) =
    (sem_Expr_AppTop _hsrange (sem_Expr _expr ) )
sem_Expr (Expr_CConst _hsrange _char )  | (Expr_CConst _hsrange _char ) `seq` (True) =
    (sem_Expr_CConst _hsrange _char )
sem_Expr (Expr_Case _hsrange _expr _alts _mbCaseIds _caseFailS _isTupOfArg )  | (Expr_Case _hsrange _expr _alts _mbCaseIds _caseFailS _isTupOfArg ) `seq` (True) =
    (sem_Expr_Case _hsrange (sem_Expr _expr ) (sem_CaseAlts _alts ) _mbCaseIds _caseFailS _isTupOfArg )
sem_Expr (Expr_CaseAltFail _hsrange _caseId )  | (Expr_CaseAltFail _hsrange _caseId ) `seq` (True) =
    (sem_Expr_CaseAltFail _hsrange _caseId )
sem_Expr (Expr_Con _hsrange _nm )  | (Expr_Con _hsrange _nm ) `seq` (True) =
    (sem_Expr_Con _hsrange _nm )
sem_Expr (Expr_DataFields _hsrange _dataFieldExpr )  | (Expr_DataFields _hsrange _dataFieldExpr ) `seq` (True) =
    (sem_Expr_DataFields _hsrange (sem_DataFieldExpr _dataFieldExpr ) )
sem_Expr (Expr_DynVar _hsrange _nm )  | (Expr_DynVar _hsrange _nm ) `seq` (True) =
    (sem_Expr_DynVar _hsrange _nm )
sem_Expr (Expr_IConst _hsrange _int )  | (Expr_IConst _hsrange _int ) `seq` (True) =
    (sem_Expr_IConst _hsrange _int )
sem_Expr (Expr_IIConst _hsrange _integer )  | (Expr_IIConst _hsrange _integer ) `seq` (True) =
    (sem_Expr_IIConst _hsrange _integer )
sem_Expr (Expr_Lam _hsrange _arg _body )  | (Expr_Lam _hsrange _arg _body ) `seq` (True) =
    (sem_Expr_Lam _hsrange (sem_PatExpr _arg ) (sem_Expr _body ) )
sem_Expr (Expr_LamImpl _hsrange _argPr _arg _body )  | (Expr_LamImpl _hsrange _argPr _arg _body ) `seq` (True) =
    (sem_Expr_LamImpl _hsrange (sem_PrExpr _argPr ) (sem_PatExpr _arg ) (sem_Expr _body ) )
sem_Expr (Expr_Let _hsrange _isStrict _decls _body )  | (Expr_Let _hsrange _isStrict _decls _body ) `seq` (True) =
    (sem_Expr_Let _hsrange _isStrict (sem_Decls _decls ) (sem_Expr _body ) )
sem_Expr (Expr_Parens _hsrange _expr )  | (Expr_Parens _hsrange _expr ) `seq` (True) =
    (sem_Expr_Parens _hsrange (sem_Expr _expr ) )
sem_Expr (Expr_Rec _hsrange _recExpr )  | (Expr_Rec _hsrange _recExpr ) `seq` (True) =
    (sem_Expr_Rec _hsrange (sem_RecExpr _recExpr ) )
sem_Expr (Expr_SConst _hsrange _str )  | (Expr_SConst _hsrange _str ) `seq` (True) =
    (sem_Expr_SConst _hsrange _str )
sem_Expr (Expr_Sel _hsrange _expr _lbl )  | (Expr_Sel _hsrange _expr _lbl ) `seq` (True) =
    (sem_Expr_Sel _hsrange (sem_Expr _expr ) _lbl )
sem_Expr (Expr_TypeAs _hsrange _tyExpr _expr )  | (Expr_TypeAs _hsrange _tyExpr _expr ) `seq` (True) =
    (sem_Expr_TypeAs _hsrange (sem_TyExpr _tyExpr ) (sem_Expr _expr ) )
sem_Expr (Expr_Undefined _hsrange )  | (Expr_Undefined _hsrange ) `seq` (True) =
    (sem_Expr_Undefined _hsrange )
sem_Expr (Expr_Var _hsrange _nm )  | (Expr_Var _hsrange _nm ) `seq` (True) =
    (sem_Expr_Var _hsrange _nm )

-- ExprAnn -----------------------------------------------------
-- cata
sem_ExprAnn :: ExprAnn  ->
               T_ExprAnn 
sem_ExprAnn (ExprAnn_Empty )  | (ExprAnn_Empty ) `seq` (True) =
    (sem_ExprAnn_Empty )

-- FuncDep -----------------------------------------------------
-- cata
sem_FuncDep :: FuncDep  ->
               T_FuncDep 
sem_FuncDep (FuncDep_Dep _hsrange _fromTvs _toTvs )  | (FuncDep_Dep _hsrange _fromTvs _toTvs ) `seq` (True) =
    (sem_FuncDep_Dep _hsrange (sem_TyVars _fromTvs ) (sem_TyVars _toTvs ) )

-- FuncDeps ----------------------------------------------------
-- cata
sem_FuncDeps :: FuncDeps  ->
                T_FuncDeps 
sem_FuncDeps list  | list `seq` (True) =
    (Prelude.foldr sem_FuncDeps_Cons sem_FuncDeps_Nil (Prelude.map sem_FuncDep list) )

-- KiExpr ------------------------------------------------------
-- cata
sem_KiExpr :: KiExpr  ->
              T_KiExpr 
sem_KiExpr (KiExpr_Ann _hsrange _ann _kiExpr )  | (KiExpr_Ann _hsrange _ann _kiExpr ) `seq` (True) =
    (sem_KiExpr_Ann _hsrange (sem_KiExprAnn _ann ) (sem_KiExpr _kiExpr ) )
sem_KiExpr (KiExpr_App _hsrange _func _arg )  | (KiExpr_App _hsrange _func _arg ) `seq` (True) =
    (sem_KiExpr_App _hsrange (sem_KiExpr _func ) (sem_KiExpr _arg ) )
sem_KiExpr (KiExpr_AppTop _hsrange _kiExpr )  | (KiExpr_AppTop _hsrange _kiExpr ) `seq` (True) =
    (sem_KiExpr_AppTop _hsrange (sem_KiExpr _kiExpr ) )
sem_KiExpr (KiExpr_Con _hsrange _nm )  | (KiExpr_Con _hsrange _nm ) `seq` (True) =
    (sem_KiExpr_Con _hsrange _nm )
sem_KiExpr (KiExpr_Parens _hsrange _kiExpr )  | (KiExpr_Parens _hsrange _kiExpr ) `seq` (True) =
    (sem_KiExpr_Parens _hsrange (sem_KiExpr _kiExpr ) )
sem_KiExpr (KiExpr_Var _hsrange _nm )  | (KiExpr_Var _hsrange _nm ) `seq` (True) =
    (sem_KiExpr_Var _hsrange _nm )

-- KiExprAnn ---------------------------------------------------
-- cata
sem_KiExprAnn :: KiExprAnn  ->
                 T_KiExprAnn 
sem_KiExprAnn (KiExprAnn_Empty )  | (KiExprAnn_Empty ) `seq` (True) =
    (sem_KiExprAnn_Empty )

-- MbTyExpr ----------------------------------------------------
-- cata
sem_MbTyExpr :: MbTyExpr  ->
                T_MbTyExpr 
sem_MbTyExpr (Prelude.Just x )  | (Prelude.Just x ) `seq` (True) =
    (sem_MbTyExpr_Just (sem_TyExpr x ) )
sem_MbTyExpr Prelude.Nothing  | Prelude.Nothing `seq` (True) =
    sem_MbTyExpr_Nothing

-- PatExpr -----------------------------------------------------
-- cata
sem_PatExpr :: PatExpr  ->
               T_PatExpr 
sem_PatExpr (PatExpr_Ann _hsrange _ann _patExpr )  | (PatExpr_Ann _hsrange _ann _patExpr ) `seq` (True) =
    (sem_PatExpr_Ann _hsrange (sem_PatExprAnn _ann ) (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_App _hsrange _func _arg )  | (PatExpr_App _hsrange _func _arg ) `seq` (True) =
    (sem_PatExpr_App _hsrange (sem_PatExpr _func ) (sem_PatExpr _arg ) )
sem_PatExpr (PatExpr_AppTop _hsrange _patExpr )  | (PatExpr_AppTop _hsrange _patExpr ) `seq` (True) =
    (sem_PatExpr_AppTop _hsrange (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_Bang _hsrange _patExpr )  | (PatExpr_Bang _hsrange _patExpr ) `seq` (True) =
    (sem_PatExpr_Bang _hsrange (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_CConst _hsrange _char )  | (PatExpr_CConst _hsrange _char ) `seq` (True) =
    (sem_PatExpr_CConst _hsrange _char )
sem_PatExpr (PatExpr_Con _hsrange _nm )  | (PatExpr_Con _hsrange _nm ) `seq` (True) =
    (sem_PatExpr_Con _hsrange _nm )
sem_PatExpr (PatExpr_DataFields _hsrange _dataFieldPatExpr )  | (PatExpr_DataFields _hsrange _dataFieldPatExpr ) `seq` (True) =
    (sem_PatExpr_DataFields _hsrange (sem_DataFieldPatExpr _dataFieldPatExpr ) )
sem_PatExpr (PatExpr_Expr _hsrange _expr _mbConst )  | (PatExpr_Expr _hsrange _expr _mbConst ) `seq` (True) =
    (sem_PatExpr_Expr _hsrange (sem_Expr _expr ) _mbConst )
sem_PatExpr (PatExpr_IConst _hsrange _int )  | (PatExpr_IConst _hsrange _int ) `seq` (True) =
    (sem_PatExpr_IConst _hsrange _int )
sem_PatExpr (PatExpr_Irrefutable _hsrange _patExpr )  | (PatExpr_Irrefutable _hsrange _patExpr ) `seq` (True) =
    (sem_PatExpr_Irrefutable _hsrange (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_Parens _hsrange _patExpr )  | (PatExpr_Parens _hsrange _patExpr ) `seq` (True) =
    (sem_PatExpr_Parens _hsrange (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_Rec _hsrange _recPatExpr )  | (PatExpr_Rec _hsrange _recPatExpr ) `seq` (True) =
    (sem_PatExpr_Rec _hsrange (sem_RecPatExpr _recPatExpr ) )
sem_PatExpr (PatExpr_SConst _hsrange _str )  | (PatExpr_SConst _hsrange _str ) `seq` (True) =
    (sem_PatExpr_SConst _hsrange _str )
sem_PatExpr (PatExpr_TypeAs _hsrange _tyExpr _patExpr )  | (PatExpr_TypeAs _hsrange _tyExpr _patExpr ) `seq` (True) =
    (sem_PatExpr_TypeAs _hsrange (sem_TyExpr _tyExpr ) (sem_PatExpr _patExpr ) )
sem_PatExpr (PatExpr_Var _hsrange _nm )  | (PatExpr_Var _hsrange _nm ) `seq` (True) =
    (sem_PatExpr_Var _hsrange _nm )
sem_PatExpr (PatExpr_VarAs _hsrange _nm _patExpr )  | (PatExpr_VarAs _hsrange _nm _patExpr ) `seq` (True) =
    (sem_PatExpr_VarAs _hsrange _nm (sem_PatExpr _patExpr ) )

-- PatExprAnn --------------------------------------------------
-- cata
sem_PatExprAnn :: PatExprAnn  ->
                  T_PatExprAnn 
sem_PatExprAnn (PatExprAnn_Empty )  | (PatExprAnn_Empty ) `seq` (True) =
    (sem_PatExprAnn_Empty )

-- PrExpr ------------------------------------------------------
-- cata
sem_PrExpr :: PrExpr  ->
              T_PrExpr 
sem_PrExpr (PrExpr_Arrow _hsrange _arg _res )  | (PrExpr_Arrow _hsrange _arg _res ) `seq` (True) =
    (sem_PrExpr_Arrow _hsrange (sem_PrExpr _arg ) (sem_PrExpr _res ) )
sem_PrExpr (PrExpr_Class _hsrange _nm _tyExprs )  | (PrExpr_Class _hsrange _nm _tyExprs ) `seq` (True) =
    (sem_PrExpr_Class _hsrange _nm (sem_TyExprs _tyExprs ) )
sem_PrExpr (PrExpr_DynVar _hsrange _nm _tyExpr )  | (PrExpr_DynVar _hsrange _nm _tyExpr ) `seq` (True) =
    (sem_PrExpr_DynVar _hsrange _nm (sem_TyExpr _tyExpr ) )
sem_PrExpr (PrExpr_Eq _hsrange _tyExpr1 _tyExpr2 )  | (PrExpr_Eq _hsrange _tyExpr1 _tyExpr2 ) `seq` (True) =
    (sem_PrExpr_Eq _hsrange (sem_TyExpr _tyExpr1 ) (sem_TyExpr _tyExpr2 ) )
sem_PrExpr (PrExpr_Forall _hsrange _tyVar _prExpr )  | (PrExpr_Forall _hsrange _tyVar _prExpr ) `seq` (True) =
    (sem_PrExpr_Forall _hsrange _tyVar (sem_PrExpr _prExpr ) )
sem_PrExpr (PrExpr_Lacks _hsrange _rowTyExpr _nm )  | (PrExpr_Lacks _hsrange _rowTyExpr _nm ) `seq` (True) =
    (sem_PrExpr_Lacks _hsrange (sem_RowTyExpr _rowTyExpr ) _nm )

-- PrExprs -----------------------------------------------------
-- cata
sem_PrExprs :: PrExprs  ->
               T_PrExprs 
sem_PrExprs list  | list `seq` (True) =
    (Prelude.foldr sem_PrExprs_Cons sem_PrExprs_Nil (Prelude.map sem_PrExpr list) )

-- RecExpr -----------------------------------------------------
-- cata
sem_RecExpr :: RecExpr  ->
               T_RecExpr 
sem_RecExpr (RecExpr_Empty _hsrange )  | (RecExpr_Empty _hsrange ) `seq` (True) =
    (sem_RecExpr_Empty _hsrange )
sem_RecExpr (RecExpr_Expr _hsrange _expr )  | (RecExpr_Expr _hsrange _expr ) `seq` (True) =
    (sem_RecExpr_Expr _hsrange (sem_Expr _expr ) )
sem_RecExpr (RecExpr_Ext _hsrange _recExpr _mbNm _expr )  | (RecExpr_Ext _hsrange _recExpr _mbNm _expr ) `seq` (True) =
    (sem_RecExpr_Ext _hsrange (sem_RecExpr _recExpr ) _mbNm (sem_Expr _expr ) )
sem_RecExpr (RecExpr_Upd _hsrange _recExpr _nm _expr )  | (RecExpr_Upd _hsrange _recExpr _nm _expr ) `seq` (True) =
    (sem_RecExpr_Upd _hsrange (sem_RecExpr _recExpr ) _nm (sem_Expr _expr ) )

-- RecPatExpr --------------------------------------------------
-- cata
sem_RecPatExpr :: RecPatExpr  ->
                  T_RecPatExpr 
sem_RecPatExpr (RecPatExpr_Empty _hsrange )  | (RecPatExpr_Empty _hsrange ) `seq` (True) =
    (sem_RecPatExpr_Empty _hsrange )
sem_RecPatExpr (RecPatExpr_Expr _hsrange _patExpr )  | (RecPatExpr_Expr _hsrange _patExpr ) `seq` (True) =
    (sem_RecPatExpr_Expr _hsrange (sem_PatExpr _patExpr ) )
sem_RecPatExpr (RecPatExpr_Ext _hsrange _recPatExpr _mbNm _patExpr )  | (RecPatExpr_Ext _hsrange _recPatExpr _mbNm _patExpr ) `seq` (True) =
    (sem_RecPatExpr_Ext _hsrange (sem_RecPatExpr _recPatExpr ) _mbNm (sem_PatExpr _patExpr ) )

-- RowTyExpr ---------------------------------------------------
-- cata
sem_RowTyExpr :: RowTyExpr  ->
                 T_RowTyExpr 
sem_RowTyExpr (RowTyExpr_Empty _hsrange )  | (RowTyExpr_Empty _hsrange ) `seq` (True) =
    (sem_RowTyExpr_Empty _hsrange )
sem_RowTyExpr (RowTyExpr_Ext _hsrange _rowTyExpr _mbNm _tyExpr )  | (RowTyExpr_Ext _hsrange _rowTyExpr _mbNm _tyExpr ) `seq` (True) =
    (sem_RowTyExpr_Ext _hsrange (sem_RowTyExpr _rowTyExpr ) _mbNm (sem_TyExpr _tyExpr ) )
sem_RowTyExpr (RowTyExpr_Var _hsrange _nm )  | (RowTyExpr_Var _hsrange _nm ) `seq` (True) =
    (sem_RowTyExpr_Var _hsrange _nm )

-- TyExpr ------------------------------------------------------
-- cata
sem_TyExpr :: TyExpr  ->
              T_TyExpr 
sem_TyExpr (TyExpr_Ann _hsrange _ann _tyExpr )  | (TyExpr_Ann _hsrange _ann _tyExpr ) `seq` (True) =
    (sem_TyExpr_Ann _hsrange (sem_TyExprAnn _ann ) (sem_TyExpr _tyExpr ) )
sem_TyExpr (TyExpr_App _hsrange _func _arg )  | (TyExpr_App _hsrange _func _arg ) `seq` (True) =
    (sem_TyExpr_App _hsrange (sem_TyExpr _func ) (sem_TyExpr _arg ) )
sem_TyExpr (TyExpr_AppTop _hsrange _tyExpr )  | (TyExpr_AppTop _hsrange _tyExpr ) `seq` (True) =
    (sem_TyExpr_AppTop _hsrange (sem_TyExpr _tyExpr ) )
sem_TyExpr (TyExpr_Con _hsrange _nm )  | (TyExpr_Con _hsrange _nm ) `seq` (True) =
    (sem_TyExpr_Con _hsrange _nm )
sem_TyExpr (TyExpr_Impls _hsrange )  | (TyExpr_Impls _hsrange ) `seq` (True) =
    (sem_TyExpr_Impls _hsrange )
sem_TyExpr (TyExpr_Lam _hsrange _tyVar _tyExpr )  | (TyExpr_Lam _hsrange _tyVar _tyExpr ) `seq` (True) =
    (sem_TyExpr_Lam _hsrange _tyVar (sem_TyExpr _tyExpr ) )
sem_TyExpr (TyExpr_Mono _hsrange )  | (TyExpr_Mono _hsrange ) `seq` (True) =
    (sem_TyExpr_Mono _hsrange )
sem_TyExpr (TyExpr_NoImpls _hsrange )  | (TyExpr_NoImpls _hsrange ) `seq` (True) =
    (sem_TyExpr_NoImpls _hsrange )
sem_TyExpr (TyExpr_Parens _hsrange _tyExpr )  | (TyExpr_Parens _hsrange _tyExpr ) `seq` (True) =
    (sem_TyExpr_Parens _hsrange (sem_TyExpr _tyExpr ) )
sem_TyExpr (TyExpr_Pred _hsrange _prExpr )  | (TyExpr_Pred _hsrange _prExpr ) `seq` (True) =
    (sem_TyExpr_Pred _hsrange (sem_PrExpr _prExpr ) )
sem_TyExpr (TyExpr_Quant _hsrange _qu _tyVar _tyExpr )  | (TyExpr_Quant _hsrange _qu _tyVar _tyExpr ) `seq` (True) =
    (sem_TyExpr_Quant _hsrange _qu _tyVar (sem_TyExpr _tyExpr ) )
sem_TyExpr (TyExpr_Row _hsrange _rowTyExpr )  | (TyExpr_Row _hsrange _rowTyExpr ) `seq` (True) =
    (sem_TyExpr_Row _hsrange (sem_RowTyExpr _rowTyExpr ) )
sem_TyExpr (TyExpr_Var _hsrange _nm )  | (TyExpr_Var _hsrange _nm ) `seq` (True) =
    (sem_TyExpr_Var _hsrange _nm )
sem_TyExpr (TyExpr_VarWild _hsrange _nm )  | (TyExpr_VarWild _hsrange _nm ) `seq` (True) =
    (sem_TyExpr_VarWild _hsrange _nm )
sem_TyExpr (TyExpr_Wild _hsrange )  | (TyExpr_Wild _hsrange ) `seq` (True) =
    (sem_TyExpr_Wild _hsrange )

-- TyExprAnn ---------------------------------------------------
-- cata
sem_TyExprAnn :: TyExprAnn  ->
                 T_TyExprAnn 
sem_TyExprAnn (TyExprAnn_Empty )  | (TyExprAnn_Empty ) `seq` (True) =
    (sem_TyExprAnn_Empty )
sem_TyExprAnn (TyExprAnn_Strictness _strictness )  | (TyExprAnn_Strictness _strictness ) `seq` (True) =
    (sem_TyExprAnn_Strictness _strictness )

-- TyExprs -----------------------------------------------------
-- cata
sem_TyExprs :: TyExprs  ->
               T_TyExprs 
sem_TyExprs list  | list `seq` (True) =
    (Prelude.foldr sem_TyExprs_Cons sem_TyExprs_Nil (Prelude.map sem_TyExpr list) )

-- TyVar -------------------------------------------------------
-- cata
sem_TyVar :: TyVar  ->
             T_TyVar 
sem_TyVar (TyVar_Var _hsrange _nm )  | (TyVar_Var _hsrange _nm ) `seq` (True) =
    (sem_TyVar_Var _hsrange _nm )

-- TyVars ------------------------------------------------------
-- cata
sem_TyVars :: TyVars  ->
              T_TyVars 
sem_TyVars list  | list `seq` (True) =
    (Prelude.foldr sem_TyVars_Cons sem_TyVars_Nil (Prelude.map sem_TyVar list) )


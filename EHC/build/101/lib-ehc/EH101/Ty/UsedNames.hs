

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/UsedNames.ag)
module EH101.Ty.UsedNames(tyUsedNames) where

import Data.List
import EH101.Base.Common
import EH101.Ty
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.Module











tyUsedNames :: HsName -> Ty -> ModEntRelFilterMp
tyUsedNames moduleNm ty
  = mentrelFilterMp_Syn_TyAGItf t
  where t =  wrap_TyAGItf
                 (sem_TyAGItf (TyAGItf_AGItf ty))
                 (Inh_TyAGItf
                   { moduleNm_Inh_TyAGItf = moduleNm
                   })



{-|
There are some conventions/restrictions on the structure of types that are not enforced
by the abstract syntax:

Encoding of prove-constraints:
  concrete syntax:
    {! impls !} -> ty
  abstract syntax:
    Ty_App (Ty_App (Ty_Con "->") (Ty_Impls impls)) ty

Encoding of assume-constraints:
  concrete syntax:
    (ty, {! pred1 !}, ..., {! predn !})
  abstract syntax:
    Ty_Ext (... (Ty_Ext ty (prod m+1) (Ty_Pred pred_1) ) ...) (prod m+n) (Ty_Pred pred_n)

  In other words: the predicates are at the outset of a product, pred_n "more outermost"
  than pred_{n-1}.

-}


{-|
The basic alternatives encode the following:
- Con: data type constructors, including tuple constructors
- App: application to 1 argument, for example 'a -> b' is encoded as (App (App -> a) b)
- Any: representing Bot/Top depending on context: (1) unknown expected type, (2) error type
- Var: type variables, including a category: plain tyvars, fixed tyvars (aka skolems)

-}
-- Impls -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         moduleNm             : HsName
      synthesized attribute:
         mentrelFilterMp      : ModEntRelFilterMp
   alternatives:
      alternative Cons:
         child iv             : {ImplsVarId}
         child pr             : Pred 
         child pv             : {PredOccId}
         child prange         : {Range}
         child proveOccs      : {[ImplsProveOcc]}
         child tl             : Impls 
      alternative Nil:
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
-}
-- cata
sem_Impls :: Impls  ->
             T_Impls 
sem_Impls (Impls_Cons _iv _pr _pv _prange _proveOccs _tl )  =
    (sem_Impls_Cons _iv (sem_Pred _pr ) _pv _prange _proveOccs (sem_Impls _tl ) )
sem_Impls (Impls_Nil )  =
    (sem_Impls_Nil )
sem_Impls (Impls_Tail _iv _proveOccs )  =
    (sem_Impls_Tail _iv _proveOccs )
-- semantic domain
type T_Impls  = HsName ->
                ( ModEntRelFilterMp)
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tlOmoduleNm ->
          (case (_lhsImoduleNm) of
           { _prOmoduleNm ->
           (case (tl_ _tlOmoduleNm ) of
            { ( _tlImentrelFilterMp) ->
                (case (pr_ _prOmoduleNm ) of
                 { ( _prImentrelFilterMp) ->
                     (case (_prImentrelFilterMp `mentrelFilterMpUnion` _tlImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
-- Label -------------------------------------------------------
{-
   alternatives:
      alternative Lab:
         child nm             : {HsName}
      alternative Var:
         child lv             : {LabelVarId}
-}
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label (Label_Lab _nm )  =
    (sem_Label_Lab _nm )
sem_Label (Label_Var _lv )  =
    (sem_Label_Var _lv )
-- semantic domain
type T_Label  = ( )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    ( )
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    ( )
-- LabelAGItf --------------------------------------------------
{-
   alternatives:
      alternative AGItf:
         child lab            : Label 
-}
-- cata
sem_LabelAGItf :: LabelAGItf  ->
                  T_LabelAGItf 
sem_LabelAGItf (LabelAGItf_AGItf _lab )  =
    (sem_LabelAGItf_AGItf (sem_Label _lab ) )
-- semantic domain
type T_LabelAGItf  = ( )
sem_LabelAGItf_AGItf :: T_Label  ->
                        T_LabelAGItf 
sem_LabelAGItf_AGItf lab_  =
    ( )
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         moduleNm             : HsName
      synthesized attribute:
         mentrelFilterMp      : ModEntRelFilterMp
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
      alternative Class:
         child ty             : Ty 
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
      alternative Pred:
         child ty             : Ty 
      alternative Preds:
         child seq            : PredSeq 
      alternative Var:
         child pv             : {TyVarId}
-}
-- cata
sem_Pred :: Pred  ->
            T_Pred 
sem_Pred (Pred_Arrow _args _res )  =
    (sem_Pred_Arrow (sem_PredSeq _args ) (sem_Pred _res ) )
sem_Pred (Pred_Class _ty )  =
    (sem_Pred_Class (sem_Ty _ty ) )
sem_Pred (Pred_Eq _tyL _tyR )  =
    (sem_Pred_Eq (sem_Ty _tyL ) (sem_Ty _tyR ) )
sem_Pred (Pred_Lacks _ty _lab )  =
    (sem_Pred_Lacks (sem_Ty _ty ) (sem_Label _lab ) )
sem_Pred (Pred_Pred _ty )  =
    (sem_Pred_Pred (sem_Ty _ty ) )
sem_Pred (Pred_Preds _seq )  =
    (sem_Pred_Preds (sem_PredSeq _seq ) )
sem_Pred (Pred_Var _pv )  =
    (sem_Pred_Var _pv )
-- semantic domain
type T_Pred  = HsName ->
               ( ModEntRelFilterMp)
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _resOmoduleNm ->
          (case (_lhsImoduleNm) of
           { _argsOmoduleNm ->
           (case (res_ _resOmoduleNm ) of
            { ( _resImentrelFilterMp) ->
                (case (args_ _argsOmoduleNm ) of
                 { ( _argsImentrelFilterMp) ->
                     (case (_argsImentrelFilterMp `mentrelFilterMpUnion` _resImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyROmoduleNm ->
          (case (_lhsImoduleNm) of
           { _tyLOmoduleNm ->
           (case (tyR_ _tyROmoduleNm ) of
            { ( _tyRImentrelFilterMp) ->
                (case (tyL_ _tyLOmoduleNm ) of
                 { ( _tyLImentrelFilterMp) ->
                     (case (_tyLImentrelFilterMp `mentrelFilterMpUnion` _tyRImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _seqOmoduleNm ->
          (case (seq_ _seqOmoduleNm ) of
           { ( _seqImentrelFilterMp) ->
               (case (_seqImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         moduleNm             : HsName
      synthesized attribute:
         mentrelFilterMp      : ModEntRelFilterMp
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
      alternative Nil:
      alternative Var:
         child av             : {TyVarId}
-}
-- cata
sem_PredSeq :: PredSeq  ->
               T_PredSeq 
sem_PredSeq (PredSeq_Cons _hd _tl )  =
    (sem_PredSeq_Cons (sem_Pred _hd ) (sem_PredSeq _tl ) )
sem_PredSeq (PredSeq_Nil )  =
    (sem_PredSeq_Nil )
sem_PredSeq (PredSeq_Var _av )  =
    (sem_PredSeq_Var _av )
-- semantic domain
type T_PredSeq  = HsName ->
                  ( ModEntRelFilterMp)
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tlOmoduleNm ->
          (case (_lhsImoduleNm) of
           { _hdOmoduleNm ->
           (case (tl_ _tlOmoduleNm ) of
            { ( _tlImentrelFilterMp) ->
                (case (hd_ _hdOmoduleNm ) of
                 { ( _hdImentrelFilterMp) ->
                     (case (_hdImentrelFilterMp `mentrelFilterMpUnion` _tlImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         moduleNm             : HsName
      synthesized attribute:
         mentrelFilterMp      : ModEntRelFilterMp
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
      alternative Any:
      alternative App:
         child func           : Ty 
         child arg            : Ty 
      alternative Con:
         child nm             : {HsName}
      alternative Dbg:
         child info           : {String}
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
      alternative Impls:
         child impls          : Impls 
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
      alternative Pred:
         child pr             : Pred 
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
-}
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Ty_Ann _ann _ty )  =
    (sem_Ty_Ann (sem_TyAnn _ann ) (sem_Ty _ty ) )
sem_Ty (Ty_Any )  =
    (sem_Ty_Any )
sem_Ty (Ty_App _func _arg )  =
    (sem_Ty_App (sem_Ty _func ) (sem_Ty _arg ) )
sem_Ty (Ty_Con _nm )  =
    (sem_Ty_Con _nm )
sem_Ty (Ty_Dbg _info )  =
    (sem_Ty_Dbg _info )
sem_Ty (Ty_Ext _ty _nm _extTy )  =
    (sem_Ty_Ext (sem_Ty _ty ) _nm (sem_Ty _extTy ) )
sem_Ty (Ty_Impls _impls )  =
    (sem_Ty_Impls (sem_Impls _impls ) )
sem_Ty (Ty_Lam _tv _ty )  =
    (sem_Ty_Lam _tv (sem_Ty _ty ) )
sem_Ty (Ty_Pred _pr )  =
    (sem_Ty_Pred (sem_Pred _pr ) )
sem_Ty (Ty_TBind _qu _tv _l1 _ty )  =
    (sem_Ty_TBind (sem_TyQu _qu ) _tv _l1 (sem_Ty _ty ) )
sem_Ty (Ty_Var _tv _categ )  =
    (sem_Ty_Var _tv (sem_TyVarCateg _categ ) )
-- semantic domain
type T_Ty  = HsName ->
             ( ModEntRelFilterMp)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _argOmoduleNm ->
          (case (_lhsImoduleNm) of
           { _funcOmoduleNm ->
           (case (arg_ _argOmoduleNm ) of
            { ( _argImentrelFilterMp) ->
                (case (func_ _funcOmoduleNm ) of
                 { ( _funcImentrelFilterMp) ->
                     (case (_funcImentrelFilterMp `mentrelFilterMpUnion` _argImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsImoduleNm ->
         (case (mentrelFilterMpSingleton [_lhsImoduleNm] IdOcc_Type nm_) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _extTyOmoduleNm ->
          (case (_lhsImoduleNm) of
           { _tyOmoduleNm ->
           (case (extTy_ _extTyOmoduleNm ) of
            { ( _extTyImentrelFilterMp) ->
                (case (ty_ _tyOmoduleNm ) of
                 { ( _tyImentrelFilterMp) ->
                     (case (_tyImentrelFilterMp `mentrelFilterMpUnion` _extTyImentrelFilterMp) of
                      { _lhsOmentrelFilterMp ->
                      ( _lhsOmentrelFilterMp) }) }) }) }) }))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _implsOmoduleNm ->
          (case (impls_ _implsOmoduleNm ) of
           { ( _implsImentrelFilterMp) ->
               (case (_implsImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _prOmoduleNm ->
          (case (pr_ _prOmoduleNm ) of
           { ( _prImentrelFilterMp) ->
               (case (_prImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsImoduleNm ->
         (case (Map.empty) of
          { _lhsOmentrelFilterMp ->
          ( _lhsOmentrelFilterMp) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         moduleNm             : HsName
      synthesized attribute:
         mentrelFilterMp      : ModEntRelFilterMp
   alternatives:
      alternative AGItf:
         child ty             : Ty 
-}
-- cata
sem_TyAGItf :: TyAGItf  ->
               T_TyAGItf 
sem_TyAGItf (TyAGItf_AGItf _ty )  =
    (sem_TyAGItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_TyAGItf  = HsName ->
                  ( ModEntRelFilterMp)
data Inh_TyAGItf  = Inh_TyAGItf {moduleNm_Inh_TyAGItf :: !(HsName)}
data Syn_TyAGItf  = Syn_TyAGItf {mentrelFilterMp_Syn_TyAGItf :: !(ModEntRelFilterMp)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsImoduleNm )  =
    (let ( _lhsOmentrelFilterMp) = sem _lhsImoduleNm 
     in  (Syn_TyAGItf _lhsOmentrelFilterMp ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsImoduleNm ->
         (case (_lhsImoduleNm) of
          { _tyOmoduleNm ->
          (case (ty_ _tyOmoduleNm ) of
           { ( _tyImentrelFilterMp) ->
               (case (_tyImentrelFilterMp) of
                { _lhsOmentrelFilterMp ->
                ( _lhsOmentrelFilterMp) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   alternatives:
      alternative Empty:
      alternative Mono:
      alternative Strictness:
         child s              : {Strictness}
-}
-- cata
sem_TyAnn :: TyAnn  ->
             T_TyAnn 
sem_TyAnn (TyAnn_Empty )  =
    (sem_TyAnn_Empty )
sem_TyAnn (TyAnn_Mono )  =
    (sem_TyAnn_Mono )
sem_TyAnn (TyAnn_Strictness _s )  =
    (sem_TyAnn_Strictness _s )
-- semantic domain
type T_TyAnn  = ( )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    ( )
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    ( )
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    ( )
-- TyQu --------------------------------------------------------
{-
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
      alternative Forall:
         child mlev           : {MetaLev}
      alternative Plain:
         child mlev           : {MetaLev}
-}
-- cata
sem_TyQu :: TyQu  ->
            T_TyQu 
sem_TyQu (TyQu_Exists _mlev )  =
    (sem_TyQu_Exists _mlev )
sem_TyQu (TyQu_Forall _mlev )  =
    (sem_TyQu_Forall _mlev )
sem_TyQu (TyQu_Plain _mlev )  =
    (sem_TyQu_Plain _mlev )
-- semantic domain
type T_TyQu  = ( )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    ( )
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    ( )
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    ( )
-- TyVarCateg --------------------------------------------------
{-
   alternatives:
      alternative Fixed:
      alternative Meta:
      alternative Plain:
-}
-- cata
sem_TyVarCateg :: TyVarCateg  ->
                  T_TyVarCateg 
sem_TyVarCateg (TyVarCateg_Fixed )  =
    (sem_TyVarCateg_Fixed )
sem_TyVarCateg (TyVarCateg_Meta )  =
    (sem_TyVarCateg_Meta )
sem_TyVarCateg (TyVarCateg_Plain )  =
    (sem_TyVarCateg_Plain )
-- semantic domain
type T_TyVarCateg  = ( )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    ( )
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    ( )
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    ( )
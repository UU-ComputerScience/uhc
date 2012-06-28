

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Ftv.ag)
module EH101.Ty.Ftv(tyFtv
, tyFtvMp
, implsFtvMp) where

import Data.List
import EH101.Base.Common
import EH101.Ty
import qualified Data.Set as Set
import qualified Data.Map as Map









tyFtv :: Ty -> Set.Set TyVarId
tyFtv = Map.keysSet . Map.filter (\i -> tvpurposeIsTy (tvinfoPurpose i)) . tyFtvMp



tyFtvMp :: Ty -> TvCatMp
tyFtvMp ty
  =  let  t =  wrap_TyAGItf
                 (sem_TyAGItf (TyAGItf_AGItf ty))
                 (Inh_TyAGItf)
     in   tvMp_Syn_TyAGItf t



implsFtvMp :: Impls -> TvCatMp
implsFtvMp i = tyFtvMp (Ty_Impls i)



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
      synthesized attribute:
         tvMp                 : TvCatMp
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
type T_Impls  = ( TvCatMp)
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (case (tl_ ) of
     { ( _tlItvMp) ->
         (case (pr_ ) of
          { ( _prItvMp) ->
              (case (_prItvMp `Map.union` _tlItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (case (Map.empty) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (case (iv_ `Map.singleton` mkTvInfoPlain TvPurpose_Impls) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
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
      synthesized attribute:
         tvMp                 : TvCatMp
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
type T_Pred  = ( TvCatMp)
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (case (res_ ) of
     { ( _resItvMp) ->
         (case (args_ ) of
          { ( _argsItvMp) ->
              (case (_argsItvMp `Map.union` _resItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (_tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (case (tyR_ ) of
     { ( _tyRItvMp) ->
         (case (tyL_ ) of
          { ( _tyLItvMp) ->
              (case (_tyLItvMp `Map.union` _tyRItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (_tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (_tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (case (seq_ ) of
     { ( _seqItvMp) ->
         (case (_seqItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (case (pv_ `Map.singleton` mkTvInfoPlain TvPurpose_Pred) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tvMp                 : TvCatMp
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
type T_PredSeq  = ( TvCatMp)
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlItvMp) ->
         (case (hd_ ) of
          { ( _hdItvMp) ->
              (case (_hdItvMp `Map.union` _tlItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (case (Map.empty) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (case (av_ `Map.singleton` mkTvInfoTy TyVarCateg_Plain) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tvMp                 : TvCatMp
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
type T_Ty  = ( TvCatMp)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (_tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (case (Map.empty) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (case (arg_ ) of
     { ( _argItvMp) ->
         (case (func_ ) of
          { ( _funcItvMp) ->
              (case (_funcItvMp `Map.union` _argItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (case (Map.empty) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (case (Map.empty) of
     { _lhsOtvMp ->
     ( _lhsOtvMp) })
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (case (extTy_ ) of
     { ( _extTyItvMp) ->
         (case (ty_ ) of
          { ( _tyItvMp) ->
              (case (_tyItvMp `Map.union` _extTyItvMp) of
               { _lhsOtvMp ->
               ( _lhsOtvMp) }) }) })
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (case (impls_ ) of
     { ( _implsItvMp) ->
         (case (_implsItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (tv_ `Map.delete` _tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (case (pr_ ) of
     { ( _prItvMp) ->
         (case (_prItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (tv_ `Map.delete` _tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (case (categ_ ) of
     { ( _categIself) ->
         (case (tv_ `Map.singleton` mkTvInfoTy _categIself) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tvMp                 : TvCatMp
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
type T_TyAGItf  = ( TvCatMp)
data Inh_TyAGItf  = Inh_TyAGItf {}
data Syn_TyAGItf  = Syn_TyAGItf {tvMp_Syn_TyAGItf :: !(TvCatMp)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf )  =
    (let ( _lhsOtvMp) = sem 
     in  (Syn_TyAGItf _lhsOtvMp ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (case (ty_ ) of
     { ( _tyItvMp) ->
         (case (_tyItvMp) of
          { _lhsOtvMp ->
          ( _lhsOtvMp) }) })
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
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local self        : _
      alternative Meta:
         visit 0:
            local self        : _
      alternative Plain:
         visit 0:
            local self        : _
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
type T_TyVarCateg  = ( TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (case (TyVarCateg_Fixed) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (case (TyVarCateg_Meta) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (case (TyVarCateg_Plain) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
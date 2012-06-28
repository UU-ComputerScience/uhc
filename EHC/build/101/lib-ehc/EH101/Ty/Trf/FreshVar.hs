

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/FreshVar.ag)
module EH101.Ty.Trf.FreshVar(tyFreshVar) where

import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable





tyFreshVar' :: (TyVarId -> Bool) -> UID -> VarMp -> Ty -> (Ty,VarMp)
tyFreshVar' allowFresh uniq tvVarMp ty
  =  let  t =  wrap_TyAGItf
                 (sem_TyAGItf (TyAGItf_AGItf ty))
                 (Inh_TyAGItf {tvVarMp_Inh_TyAGItf = tvVarMp, allowFresh_Inh_TyAGItf = allowFresh, gUniq_Inh_TyAGItf = uniq})
     in   (repl_Syn_TyAGItf t,tvVarMp_Syn_TyAGItf t)

tyFreshVar :: (TyVarId -> Bool) -> UID -> Ty -> Ty
tyFreshVar allowFresh uniq ty =  fst $ tyFreshVar' allowFresh uniq emptyVarMp ty



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
         allowFresh           : TyVarId -> Bool
      chained attributes:
         gUniq                : UID
         tvVarMp              : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child iv             : {ImplsVarId}
         child pr             : Pred 
         child pv             : {PredOccId}
         child prange         : {Range}
         child proveOccs      : {[ImplsProveOcc]}
         child tl             : Impls 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 0:
            local repl        : _
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
type T_Impls  = (TyVarId -> Bool) ->
                UID ->
                VarMp ->
                ( UID,Impls ,VarMp)
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _prOgUniq ->
          (case (_lhsItvVarMp) of
           { _prOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _prOallowFresh ->
            (case (pr_ _prOallowFresh _prOgUniq _prOtvVarMp ) of
             { ( _prIgUniq,_prIrepl,_prItvVarMp) ->
                 (case (_prIgUniq) of
                  { _tlOgUniq ->
                  (case (_prItvVarMp) of
                   { _tlOtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _tlOallowFresh ->
                    (case (tl_ _tlOallowFresh _tlOgUniq _tlOtvVarMp ) of
                     { ( _tlIgUniq,_tlIrepl,_tlItvVarMp) ->
                         (case (_tlIgUniq) of
                          { _lhsOgUniq ->
                          (case (Impls_Cons iv_ _prIrepl pv_ prange_ proveOccs_ _tlIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_tlItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Impls_Nil) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Impls_Tail iv_ proveOccs_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local repl        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
            local repl        : _
-}
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label (Label_Lab _nm )  =
    (sem_Label_Lab _nm )
sem_Label (Label_Var _lv )  =
    (sem_Label_Var _lv )
-- semantic domain
type T_Label  = (TyVarId -> Bool) ->
                ( Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (\ _lhsIallowFresh ->
         (case (Label_Lab nm_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (\ _lhsIallowFresh ->
         (case (Label_Var lv_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
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
         allowFresh           : TyVarId -> Bool
      chained attributes:
         gUniq                : UID
         tvVarMp              : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 0:
            local repl        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local repl        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local repl        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local repl        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local repl        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 0:
            local repl        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 0:
            local repl        : _
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
type T_Pred  = (TyVarId -> Bool) ->
               UID ->
               VarMp ->
               ( UID,Pred ,VarMp)
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _argsOgUniq ->
          (case (_lhsItvVarMp) of
           { _argsOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _argsOallowFresh ->
            (case (args_ _argsOallowFresh _argsOgUniq _argsOtvVarMp ) of
             { ( _argsIgUniq,_argsIrepl,_argsItvVarMp) ->
                 (case (_argsIgUniq) of
                  { _resOgUniq ->
                  (case (_argsItvVarMp) of
                   { _resOtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _resOallowFresh ->
                    (case (res_ _resOallowFresh _resOgUniq _resOtvVarMp ) of
                     { ( _resIgUniq,_resIrepl,_resItvVarMp) ->
                         (case (_resIgUniq) of
                          { _lhsOgUniq ->
                          (case (Pred_Arrow _argsIrepl _resIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_resItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (Pred_Class _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_tyItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyLOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyLOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyLOallowFresh ->
            (case (tyL_ _tyLOallowFresh _tyLOgUniq _tyLOtvVarMp ) of
             { ( _tyLIgUniq,_tyLIrepl,_tyLItvVarMp) ->
                 (case (_tyLIgUniq) of
                  { _tyROgUniq ->
                  (case (_tyLItvVarMp) of
                   { _tyROtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _tyROallowFresh ->
                    (case (tyR_ _tyROallowFresh _tyROgUniq _tyROtvVarMp ) of
                     { ( _tyRIgUniq,_tyRIrepl,_tyRItvVarMp) ->
                         (case (_tyRIgUniq) of
                          { _lhsOgUniq ->
                          (case (Pred_Eq _tyLIrepl _tyRIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_tyRItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (_lhsIallowFresh) of
                   { _labOallowFresh ->
                   (case (lab_ _labOallowFresh ) of
                    { ( _labIrepl) ->
                        (case (Pred_Lacks _tyIrepl _labIrepl) of
                         { _repl ->
                         (case (_repl) of
                          { _lhsOrepl ->
                          (case (_tyItvVarMp) of
                           { _lhsOtvVarMp ->
                           ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (Pred_Pred _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_tyItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _seqOgUniq ->
          (case (_lhsItvVarMp) of
           { _seqOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _seqOallowFresh ->
            (case (seq_ _seqOallowFresh _seqOgUniq _seqOtvVarMp ) of
             { ( _seqIgUniq,_seqIrepl,_seqItvVarMp) ->
                 (case (_seqIgUniq) of
                  { _lhsOgUniq ->
                  (case (Pred_Preds _seqIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_seqItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Pred_Var pv_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      chained attributes:
         gUniq                : UID
         tvVarMp              : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
      alternative Var:
         child av             : {TyVarId}
         visit 0:
            local repl        : _
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
type T_PredSeq  = (TyVarId -> Bool) ->
                  UID ->
                  VarMp ->
                  ( UID,PredSeq ,VarMp)
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsItvVarMp) of
           { _hdOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _hdOallowFresh ->
            (case (hd_ _hdOallowFresh _hdOgUniq _hdOtvVarMp ) of
             { ( _hdIgUniq,_hdIrepl,_hdItvVarMp) ->
                 (case (_hdIgUniq) of
                  { _tlOgUniq ->
                  (case (_hdItvVarMp) of
                   { _tlOtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _tlOallowFresh ->
                    (case (tl_ _tlOallowFresh _tlOgUniq _tlOtvVarMp ) of
                     { ( _tlIgUniq,_tlIrepl,_tlItvVarMp) ->
                         (case (_tlIgUniq) of
                          { _lhsOgUniq ->
                          (case (PredSeq_Cons _hdIrepl _tlIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_tlItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (PredSeq_Nil) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (PredSeq_Var av_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      chained attributes:
         gUniq                : UID
         tvVarMp              : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 0:
            local repl        : _
      alternative Any:
         visit 0:
            local repl        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local repl        : _
      alternative Con:
         child nm             : {HsName}
         visit 0:
            local repl        : _
      alternative Dbg:
         child info           : {String}
         visit 0:
            local repl        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local repl        : _
      alternative Impls:
         child impls          : Impls 
         visit 0:
            local repl        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 0:
            local repl        : _
      alternative Pred:
         child pr             : Pred 
         visit 0:
            local repl        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 0:
            local repl        : _
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 0:
            local _tup1       : _
            local repl        : _
            local lUniq       : _
            local _tup2       : {(Ty,VarMp)}
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
type T_Ty  = (TyVarId -> Bool) ->
             UID ->
             VarMp ->
             ( UID,Ty ,VarMp)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (_lhsIallowFresh) of
                   { _annOallowFresh ->
                   (case (ann_ _annOallowFresh ) of
                    { ( _annIrepl) ->
                        (case (Ty_Ann _annIrepl _tyIrepl) of
                         { _repl ->
                         (case (_repl) of
                          { _lhsOrepl ->
                          (case (_tyItvVarMp) of
                           { _lhsOtvVarMp ->
                           ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Ty_Any) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _funcOgUniq ->
          (case (_lhsItvVarMp) of
           { _funcOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _funcOallowFresh ->
            (case (func_ _funcOallowFresh _funcOgUniq _funcOtvVarMp ) of
             { ( _funcIgUniq,_funcIrepl,_funcItvVarMp) ->
                 (case (_funcIgUniq) of
                  { _argOgUniq ->
                  (case (_funcItvVarMp) of
                   { _argOtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _argOallowFresh ->
                    (case (arg_ _argOallowFresh _argOgUniq _argOtvVarMp ) of
                     { ( _argIgUniq,_argIrepl,_argItvVarMp) ->
                         (case (_argIgUniq) of
                          { _lhsOgUniq ->
                          (case (Ty_App _funcIrepl _argIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_argItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Ty_Con nm_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (Ty_Dbg info_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            (case (_lhsItvVarMp) of
             { _lhsOtvVarMp ->
             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _extTyOgUniq ->
                  (case (_tyItvVarMp) of
                   { _extTyOtvVarMp ->
                   (case (_lhsIallowFresh) of
                    { _extTyOallowFresh ->
                    (case (extTy_ _extTyOallowFresh _extTyOgUniq _extTyOtvVarMp ) of
                     { ( _extTyIgUniq,_extTyIrepl,_extTyItvVarMp) ->
                         (case (_extTyIgUniq) of
                          { _lhsOgUniq ->
                          (case (Ty_Ext _tyIrepl nm_ _extTyIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            (case (_extTyItvVarMp) of
                             { _lhsOtvVarMp ->
                             ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _implsOgUniq ->
          (case (_lhsItvVarMp) of
           { _implsOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _implsOallowFresh ->
            (case (impls_ _implsOallowFresh _implsOgUniq _implsOtvVarMp ) of
             { ( _implsIgUniq,_implsIrepl,_implsItvVarMp) ->
                 (case (_implsIgUniq) of
                  { _lhsOgUniq ->
                  (case (Ty_Impls _implsIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_implsItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (Ty_Lam tv_ _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_tyItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _prOgUniq ->
          (case (_lhsItvVarMp) of
           { _prOtvVarMp ->
           (case (_lhsIallowFresh) of
            { _prOallowFresh ->
            (case (pr_ _prOallowFresh _prOgUniq _prOtvVarMp ) of
             { ( _prIgUniq,_prIrepl,_prItvVarMp) ->
                 (case (_prIgUniq) of
                  { _lhsOgUniq ->
                  (case (Ty_Pred _prIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    (case (_prItvVarMp) of
                     { _lhsOtvVarMp ->
                     ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsItvVarMp) of
           { _tyOtvVarMp ->
           (case (\v -> v /= tv_ && _lhsIallowFresh v) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIgUniq) of
                  { _lhsOgUniq ->
                  (case (_lhsIallowFresh) of
                   { _quOallowFresh ->
                   (case (qu_ _quOallowFresh ) of
                    { ( _quIrepl) ->
                        (case (Ty_TBind _quIrepl tv_ l1_ _tyIrepl) of
                         { _repl ->
                         (case (_repl) of
                          { _lhsOrepl ->
                          (case (_tyItvVarMp) of
                           { _lhsOtvVarMp ->
                           ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (mkNewUID _lhsIgUniq) of
          { __tup1 ->
          (case (__tup1) of
           { (_lhsOgUniq,_) ->
           (case (_lhsIallowFresh) of
            { _categOallowFresh ->
            (case (categ_ _categOallowFresh ) of
             { ( _categIrepl) ->
                 (case (Ty_Var tv_ _categIrepl) of
                  { _repl ->
                  (case (__tup1) of
                   { (_,_lUniq) ->
                   (case (if _lhsIallowFresh tv_
                          then  case varmpTyLookup tv_ _lhsItvVarMp of
                                  Just t   -> (t,_lhsItvVarMp)
                                  Nothing  -> (t,(tv_ `varmpTyUnit` t) `varUpd` _lhsItvVarMp)
                                           where t = Ty_Var _lUniq _categIrepl
                          else  (_repl,_lhsItvVarMp)) of
                    { __tup2 ->
                    (case (__tup2) of
                     { (_lhsOrepl,_) ->
                     (case (__tup2) of
                      { (_,_lhsOtvVarMp) ->
                      ( _lhsOgUniq,_lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }) }) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowFresh           : TyVarId -> Bool
         gUniq                : UID
      chained attribute:
         tvVarMp              : VarMp
      synthesized attribute:
         repl                 : Ty 
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
type T_TyAGItf  = (TyVarId -> Bool) ->
                  UID ->
                  VarMp ->
                  ( Ty ,VarMp)
data Inh_TyAGItf  = Inh_TyAGItf {allowFresh_Inh_TyAGItf :: !((TyVarId -> Bool)),gUniq_Inh_TyAGItf :: !(UID),tvVarMp_Inh_TyAGItf :: !(VarMp)}
data Syn_TyAGItf  = Syn_TyAGItf {repl_Syn_TyAGItf :: !(Ty ),tvVarMp_Syn_TyAGItf :: !(VarMp)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIallowFresh _lhsIgUniq _lhsItvVarMp )  =
    (let ( _lhsOrepl,_lhsOtvVarMp) = sem _lhsIallowFresh _lhsIgUniq _lhsItvVarMp 
     in  (Syn_TyAGItf _lhsOrepl _lhsOtvVarMp ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIallowFresh
       _lhsIgUniq
       _lhsItvVarMp ->
         (case (_lhsItvVarMp) of
          { _tyOtvVarMp ->
          (case (_lhsIgUniq) of
           { _tyOgUniq ->
           (case (_lhsIallowFresh) of
            { _tyOallowFresh ->
            (case (ty_ _tyOallowFresh _tyOgUniq _tyOtvVarMp ) of
             { ( _tyIgUniq,_tyIrepl,_tyItvVarMp) ->
                 (case (_tyIrepl) of
                  { _lhsOrepl ->
                  (case (_tyItvVarMp) of
                   { _lhsOtvVarMp ->
                   ( _lhsOrepl,_lhsOtvVarMp) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local repl        : _
      alternative Mono:
         visit 0:
            local repl        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local repl        : _
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
type T_TyAnn  = (TyVarId -> Bool) ->
                ( TyAnn )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (\ _lhsIallowFresh ->
         (case (TyAnn_Empty) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (\ _lhsIallowFresh ->
         (case (TyAnn_Mono) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (\ _lhsIallowFresh ->
         (case (TyAnn_Strictness s_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- TyQu --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
            local repl        : _
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
type T_TyQu  = (TyVarId -> Bool) ->
               ( TyQu )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (\ _lhsIallowFresh ->
         (case (TyQu_Exists mlev_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (\ _lhsIallowFresh ->
         (case (TyQu_Forall mlev_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (\ _lhsIallowFresh ->
         (case (TyQu_Plain mlev_) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allowFresh           : TyVarId -> Bool
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Fixed:
         visit 0:
            local repl        : _
      alternative Meta:
         visit 0:
            local repl        : _
      alternative Plain:
         visit 0:
            local repl        : _
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
type T_TyVarCateg  = (TyVarId -> Bool) ->
                     ( TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (\ _lhsIallowFresh ->
         (case (TyVarCateg_Fixed) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (\ _lhsIallowFresh ->
         (case (TyVarCateg_Meta) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (\ _lhsIallowFresh ->
         (case (TyVarCateg_Plain) of
          { _repl ->
          (case (_repl) of
           { _lhsOrepl ->
           ( _lhsOrepl) }) }))


-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/Instantiate.ag)
module EH101.Ty.Trf.Instantiate(tyInst1Quants, tyInst1Exists, instCoConst, instContra, instCo) where

import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable
import EH101.Base.Debug
import EH.Util.Pretty
import Data.List
import qualified Data.Set as Set















tyInst :: UID -> Bool -> HowToInst -> Ty -> (Ty,TyVarIdL,[InstTo])
tyInst uniq onlyExists howToInst ty
  = (repl_Syn_TyAGItf t, replTvL_Syn_TyAGItf t, instToL_Syn_TyAGItf t)
  where t = wrap_TyAGItf
              (sem_TyAGItf (TyAGItf_AGItf ty))
              (Inh_TyAGItf
                 { gUniq_Inh_TyAGItf 		= uniq
                 , onlyExists_Inh_TyAGItf 	= onlyExists
                 , howToInst_Inh_TyAGItf 	= howToInst
                 })

tyInst1Quants :: UID -> HowToInst -> Ty -> (Ty,TyVarIdL,[InstTo])
tyInst1Quants uniq howToInst ty = tyInst uniq False howToInst ty



tyInst1Exists :: UID -> Ty -> Ty
tyInst1Exists uniq ty
  = t
  where (t,_,_) = tyInst uniq True instCoConst ty



type HowToInst = TyQu -> TyVarId -> Ty

instCoConst, instContra, instCo :: HowToInst
instCoConst  q v = if tyquIsForall q then Ty_Var v TyVarCateg_Plain else mkTyCon ("C_" ++ show v)
instContra   q v = if tyquIsForall q then Ty_Var v TyVarCateg_Fixed else Ty_Var v TyVarCateg_Plain
instCo       q v = if tyquIsForall q then Ty_Var v TyVarCateg_Plain else Ty_Var v TyVarCateg_Fixed



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
      inherited attributes:
         allow                : Bool
         howToInst            : HowToInst
         mbQ                  : Maybe TyQu
         onlyExists           : Bool
      chained attributes:
         gUniq                : UID
         gathSubst            : VarMp
   visit 1:
      inherited attribute:
         replSubst            : VarMp
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
         visit 1:
            local repl        : _
      alternative Nil:
         visit 1:
            local repl        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 1:
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
type T_Impls  = Bool ->
                UID ->
                VarMp ->
                HowToInst ->
                (Maybe TyQu) ->
                Bool ->
                ( UID,VarMp,T_Impls_1 )
type T_Impls_1  = VarMp ->
                  ( Impls )
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _prOgUniq ->
          (case (_lhsIonlyExists) of
           { _prOonlyExists ->
           (case (_lhsImbQ) of
            { _prOmbQ ->
            (case (_lhsIhowToInst) of
             { _prOhowToInst ->
             (case (_lhsIgathSubst) of
              { _prOgathSubst ->
              (case (_lhsIallow) of
               { _prOallow ->
               (case (pr_ _prOallow _prOgUniq _prOgathSubst _prOhowToInst _prOmbQ _prOonlyExists ) of
                { ( _prIgUniq,_prIgathSubst,pr_1) ->
                    (case (_prIgUniq) of
                     { _tlOgUniq ->
                     (case (_lhsIonlyExists) of
                      { _tlOonlyExists ->
                      (case (_lhsImbQ) of
                       { _tlOmbQ ->
                       (case (_lhsIhowToInst) of
                        { _tlOhowToInst ->
                        (case (_prIgathSubst) of
                         { _tlOgathSubst ->
                         (case (_lhsIallow) of
                          { _tlOallow ->
                          (case (tl_ _tlOallow _tlOgUniq _tlOgathSubst _tlOhowToInst _tlOmbQ _tlOonlyExists ) of
                           { ( _tlIgUniq,_tlIgathSubst,tl_1) ->
                               (case (_tlIgUniq) of
                                { _lhsOgUniq ->
                                (case (_tlIgathSubst) of
                                 { _lhsOgathSubst ->
                                 (case ((let sem_Impls_Cons_1 :: T_Impls_1 
                                             sem_Impls_Cons_1  =
                                                 (\ _lhsIreplSubst ->
                                                      (case (_lhsIreplSubst) of
                                                       { _tlOreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _prOreplSubst ->
                                                        (case (tl_1 _tlOreplSubst ) of
                                                         { ( _tlIrepl) ->
                                                             (case (pr_1 _prOreplSubst ) of
                                                              { ( _prIrepl) ->
                                                                  (case (Impls_Cons iv_ _prIrepl pv_ prange_ proveOccs_ _tlIrepl) of
                                                                   { _repl ->
                                                                   (case (_repl) of
                                                                    { _lhsOrepl ->
                                                                    ( _lhsOrepl) }) }) }) }) }) }))
                                         in  sem_Impls_Cons_1)) of
                                  { ( sem_Impls_1) ->
                                  ( _lhsOgUniq,_lhsOgathSubst,sem_Impls_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Impls_Nil_1 :: T_Impls_1 
                       sem_Impls_Nil_1  =
                           (\ _lhsIreplSubst ->
                                (case (Impls_Nil) of
                                 { _repl ->
                                 (case (_repl) of
                                  { _lhsOrepl ->
                                  ( _lhsOrepl) }) }))
                   in  sem_Impls_Nil_1)) of
            { ( sem_Impls_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Impls_1) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Impls_Tail_1 :: T_Impls_1 
                       sem_Impls_Tail_1  =
                           (\ _lhsIreplSubst ->
                                (case (Impls_Tail iv_ proveOccs_) of
                                 { _repl ->
                                 (case (_repl) of
                                  { _lhsOrepl ->
                                  ( _lhsOrepl) }) }))
                   in  sem_Impls_Tail_1)) of
            { ( sem_Impls_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Impls_1) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
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
type T_Label  = ( Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (case (Label_Lab nm_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (case (Label_Var lv_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
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
      inherited attributes:
         allow                : Bool
         howToInst            : HowToInst
         mbQ                  : Maybe TyQu
         onlyExists           : Bool
      chained attributes:
         gUniq                : UID
         gathSubst            : VarMp
   visit 1:
      inherited attribute:
         replSubst            : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 1:
            local repl        : _
      alternative Class:
         child ty             : Ty 
         visit 1:
            local repl        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 1:
            local repl        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 1:
            local repl        : _
      alternative Pred:
         child ty             : Ty 
         visit 1:
            local repl        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 1:
            local repl        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 1:
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
type T_Pred  = Bool ->
               UID ->
               VarMp ->
               HowToInst ->
               (Maybe TyQu) ->
               Bool ->
               ( UID,VarMp,T_Pred_1 )
type T_Pred_1  = VarMp ->
                 ( Pred )
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _argsOgUniq ->
          (case (_lhsIonlyExists) of
           { _argsOonlyExists ->
           (case (_lhsImbQ) of
            { _argsOmbQ ->
            (case (_lhsIhowToInst) of
             { _argsOhowToInst ->
             (case (_lhsIgathSubst) of
              { _argsOgathSubst ->
              (case (_lhsIallow) of
               { _argsOallow ->
               (case (args_ _argsOallow _argsOgUniq _argsOgathSubst _argsOhowToInst _argsOmbQ _argsOonlyExists ) of
                { ( _argsIgUniq,_argsIgathSubst,args_1) ->
                    (case (_argsIgUniq) of
                     { _resOgUniq ->
                     (case (_lhsIonlyExists) of
                      { _resOonlyExists ->
                      (case (_lhsImbQ) of
                       { _resOmbQ ->
                       (case (_lhsIhowToInst) of
                        { _resOhowToInst ->
                        (case (_argsIgathSubst) of
                         { _resOgathSubst ->
                         (case (_lhsIallow) of
                          { _resOallow ->
                          (case (res_ _resOallow _resOgUniq _resOgathSubst _resOhowToInst _resOmbQ _resOonlyExists ) of
                           { ( _resIgUniq,_resIgathSubst,res_1) ->
                               (case (_resIgUniq) of
                                { _lhsOgUniq ->
                                (case (_resIgathSubst) of
                                 { _lhsOgathSubst ->
                                 (case ((let sem_Pred_Arrow_1 :: T_Pred_1 
                                             sem_Pred_Arrow_1  =
                                                 (\ _lhsIreplSubst ->
                                                      (case (_lhsIreplSubst) of
                                                       { _resOreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _argsOreplSubst ->
                                                        (case (res_1 _resOreplSubst ) of
                                                         { ( _resIrepl) ->
                                                             (case (args_1 _argsOreplSubst ) of
                                                              { ( _argsIrepl) ->
                                                                  (case (Pred_Arrow _argsIrepl _resIrepl) of
                                                                   { _repl ->
                                                                   (case (_repl) of
                                                                    { _lhsOrepl ->
                                                                    ( _lhsOrepl) }) }) }) }) }) }))
                                         in  sem_Pred_Arrow_1)) of
                                  { ( sem_Pred_1) ->
                                  ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (_lhsIallow) of
               { _tyOallow ->
               (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                    (case (_tyIgUniq) of
                     { _lhsOgUniq ->
                     (case (_tyIgathSubst) of
                      { _lhsOgathSubst ->
                      (case ((let sem_Pred_Class_1 :: T_Pred_1 
                                  sem_Pred_Class_1  =
                                      (\ _lhsIreplSubst ->
                                           (case (_lhsIreplSubst) of
                                            { _tyOreplSubst ->
                                            (case (ty_1 _tyOreplSubst ) of
                                             { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                 (case (Pred_Class _tyIrepl) of
                                                  { _repl ->
                                                  (case (_repl) of
                                                   { _lhsOrepl ->
                                                   ( _lhsOrepl) }) }) }) }))
                              in  sem_Pred_Class_1)) of
                       { ( sem_Pred_1) ->
                       ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyLOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyLOonlyExists ->
           (case (_lhsImbQ) of
            { _tyLOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyLOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyLOgathSubst ->
              (case (_lhsIallow) of
               { _tyLOallow ->
               (case (tyL_ _tyLOallow _tyLOgUniq _tyLOgathSubst _tyLOhowToInst _tyLOmbQ _tyLOonlyExists ) of
                { ( _tyLIgUniq,_tyLIgathSubst,tyL_1) ->
                    (case (_tyLIgUniq) of
                     { _tyROgUniq ->
                     (case (_lhsIonlyExists) of
                      { _tyROonlyExists ->
                      (case (_lhsImbQ) of
                       { _tyROmbQ ->
                       (case (_lhsIhowToInst) of
                        { _tyROhowToInst ->
                        (case (_tyLIgathSubst) of
                         { _tyROgathSubst ->
                         (case (_lhsIallow) of
                          { _tyROallow ->
                          (case (tyR_ _tyROallow _tyROgUniq _tyROgathSubst _tyROhowToInst _tyROmbQ _tyROonlyExists ) of
                           { ( _tyRIgUniq,_tyRIgathSubst,tyR_1) ->
                               (case (_tyRIgUniq) of
                                { _lhsOgUniq ->
                                (case (_tyRIgathSubst) of
                                 { _lhsOgathSubst ->
                                 (case ((let sem_Pred_Eq_1 :: T_Pred_1 
                                             sem_Pred_Eq_1  =
                                                 (\ _lhsIreplSubst ->
                                                      (case (_lhsIreplSubst) of
                                                       { _tyROreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _tyLOreplSubst ->
                                                        (case (tyR_1 _tyROreplSubst ) of
                                                         { ( _tyRIinstToL,_tyRIrepl,_tyRIreplTvL) ->
                                                             (case (tyL_1 _tyLOreplSubst ) of
                                                              { ( _tyLIinstToL,_tyLIrepl,_tyLIreplTvL) ->
                                                                  (case (Pred_Eq _tyLIrepl _tyRIrepl) of
                                                                   { _repl ->
                                                                   (case (_repl) of
                                                                    { _lhsOrepl ->
                                                                    ( _lhsOrepl) }) }) }) }) }) }))
                                         in  sem_Pred_Eq_1)) of
                                  { ( sem_Pred_1) ->
                                  ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (_lhsIallow) of
               { _tyOallow ->
               (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                    (case (_tyIgUniq) of
                     { _lhsOgUniq ->
                     (case (_tyIgathSubst) of
                      { _lhsOgathSubst ->
                      (case ((let sem_Pred_Lacks_1 :: T_Pred_1 
                                  sem_Pred_Lacks_1  =
                                      (\ _lhsIreplSubst ->
                                           (case (_lhsIreplSubst) of
                                            { _tyOreplSubst ->
                                            (case (lab_ ) of
                                             { ( _labIrepl) ->
                                                 (case (ty_1 _tyOreplSubst ) of
                                                  { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                      (case (Pred_Lacks _tyIrepl _labIrepl) of
                                                       { _repl ->
                                                       (case (_repl) of
                                                        { _lhsOrepl ->
                                                        ( _lhsOrepl) }) }) }) }) }))
                              in  sem_Pred_Lacks_1)) of
                       { ( sem_Pred_1) ->
                       ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (_lhsIallow) of
               { _tyOallow ->
               (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                    (case (_tyIgUniq) of
                     { _lhsOgUniq ->
                     (case (_tyIgathSubst) of
                      { _lhsOgathSubst ->
                      (case ((let sem_Pred_Pred_1 :: T_Pred_1 
                                  sem_Pred_Pred_1  =
                                      (\ _lhsIreplSubst ->
                                           (case (_lhsIreplSubst) of
                                            { _tyOreplSubst ->
                                            (case (ty_1 _tyOreplSubst ) of
                                             { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                 (case (Pred_Pred _tyIrepl) of
                                                  { _repl ->
                                                  (case (_repl) of
                                                   { _lhsOrepl ->
                                                   ( _lhsOrepl) }) }) }) }))
                              in  sem_Pred_Pred_1)) of
                       { ( sem_Pred_1) ->
                       ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _seqOgUniq ->
          (case (_lhsIonlyExists) of
           { _seqOonlyExists ->
           (case (_lhsImbQ) of
            { _seqOmbQ ->
            (case (_lhsIhowToInst) of
             { _seqOhowToInst ->
             (case (_lhsIgathSubst) of
              { _seqOgathSubst ->
              (case (_lhsIallow) of
               { _seqOallow ->
               (case (seq_ _seqOallow _seqOgUniq _seqOgathSubst _seqOhowToInst _seqOmbQ _seqOonlyExists ) of
                { ( _seqIgUniq,_seqIgathSubst,seq_1) ->
                    (case (_seqIgUniq) of
                     { _lhsOgUniq ->
                     (case (_seqIgathSubst) of
                      { _lhsOgathSubst ->
                      (case ((let sem_Pred_Preds_1 :: T_Pred_1 
                                  sem_Pred_Preds_1  =
                                      (\ _lhsIreplSubst ->
                                           (case (_lhsIreplSubst) of
                                            { _seqOreplSubst ->
                                            (case (seq_1 _seqOreplSubst ) of
                                             { ( _seqIrepl) ->
                                                 (case (Pred_Preds _seqIrepl) of
                                                  { _repl ->
                                                  (case (_repl) of
                                                   { _lhsOrepl ->
                                                   ( _lhsOrepl) }) }) }) }))
                              in  sem_Pred_Preds_1)) of
                       { ( sem_Pred_1) ->
                       ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Pred_Var_1 :: T_Pred_1 
                       sem_Pred_Var_1  =
                           (\ _lhsIreplSubst ->
                                (case (Pred_Var pv_) of
                                 { _repl ->
                                 (case (_repl) of
                                  { _lhsOrepl ->
                                  ( _lhsOrepl) }) }))
                   in  sem_Pred_Var_1)) of
            { ( sem_Pred_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Pred_1) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allow                : Bool
         howToInst            : HowToInst
         mbQ                  : Maybe TyQu
         onlyExists           : Bool
      chained attributes:
         gUniq                : UID
         gathSubst            : VarMp
   visit 1:
      inherited attribute:
         replSubst            : VarMp
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 1:
            local repl        : _
      alternative Nil:
         visit 1:
            local repl        : _
      alternative Var:
         child av             : {TyVarId}
         visit 1:
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
type T_PredSeq  = Bool ->
                  UID ->
                  VarMp ->
                  HowToInst ->
                  (Maybe TyQu) ->
                  Bool ->
                  ( UID,VarMp,T_PredSeq_1 )
type T_PredSeq_1  = VarMp ->
                    ( PredSeq )
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _hdOgUniq ->
          (case (_lhsIonlyExists) of
           { _hdOonlyExists ->
           (case (_lhsImbQ) of
            { _hdOmbQ ->
            (case (_lhsIhowToInst) of
             { _hdOhowToInst ->
             (case (_lhsIgathSubst) of
              { _hdOgathSubst ->
              (case (_lhsIallow) of
               { _hdOallow ->
               (case (hd_ _hdOallow _hdOgUniq _hdOgathSubst _hdOhowToInst _hdOmbQ _hdOonlyExists ) of
                { ( _hdIgUniq,_hdIgathSubst,hd_1) ->
                    (case (_hdIgUniq) of
                     { _tlOgUniq ->
                     (case (_lhsIonlyExists) of
                      { _tlOonlyExists ->
                      (case (_lhsImbQ) of
                       { _tlOmbQ ->
                       (case (_lhsIhowToInst) of
                        { _tlOhowToInst ->
                        (case (_hdIgathSubst) of
                         { _tlOgathSubst ->
                         (case (_lhsIallow) of
                          { _tlOallow ->
                          (case (tl_ _tlOallow _tlOgUniq _tlOgathSubst _tlOhowToInst _tlOmbQ _tlOonlyExists ) of
                           { ( _tlIgUniq,_tlIgathSubst,tl_1) ->
                               (case (_tlIgUniq) of
                                { _lhsOgUniq ->
                                (case (_tlIgathSubst) of
                                 { _lhsOgathSubst ->
                                 (case ((let sem_PredSeq_Cons_1 :: T_PredSeq_1 
                                             sem_PredSeq_Cons_1  =
                                                 (\ _lhsIreplSubst ->
                                                      (case (_lhsIreplSubst) of
                                                       { _tlOreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _hdOreplSubst ->
                                                        (case (tl_1 _tlOreplSubst ) of
                                                         { ( _tlIrepl) ->
                                                             (case (hd_1 _hdOreplSubst ) of
                                                              { ( _hdIrepl) ->
                                                                  (case (PredSeq_Cons _hdIrepl _tlIrepl) of
                                                                   { _repl ->
                                                                   (case (_repl) of
                                                                    { _lhsOrepl ->
                                                                    ( _lhsOrepl) }) }) }) }) }) }))
                                         in  sem_PredSeq_Cons_1)) of
                                  { ( sem_PredSeq_1) ->
                                  ( _lhsOgUniq,_lhsOgathSubst,sem_PredSeq_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_PredSeq_Nil_1 :: T_PredSeq_1 
                       sem_PredSeq_Nil_1  =
                           (\ _lhsIreplSubst ->
                                (case (PredSeq_Nil) of
                                 { _repl ->
                                 (case (_repl) of
                                  { _lhsOrepl ->
                                  ( _lhsOrepl) }) }))
                   in  sem_PredSeq_Nil_1)) of
            { ( sem_PredSeq_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_PredSeq_1) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_PredSeq_Var_1 :: T_PredSeq_1 
                       sem_PredSeq_Var_1  =
                           (\ _lhsIreplSubst ->
                                (case (PredSeq_Var av_) of
                                 { _repl ->
                                 (case (_repl) of
                                  { _lhsOrepl ->
                                  ( _lhsOrepl) }) }))
                   in  sem_PredSeq_Var_1)) of
            { ( sem_PredSeq_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_PredSeq_1) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allow                : Bool
         howToInst            : HowToInst
         mbQ                  : Maybe TyQu
         onlyExists           : Bool
      chained attributes:
         gUniq                : UID
         gathSubst            : VarMp
   visit 1:
      inherited attribute:
         replSubst            : VarMp
      synthesized attributes:
         instToL              : [InstTo]
         repl                 : SELF 
         replTvL              : TyVarIdL
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative Any:
         visit 1:
            local repl        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative Con:
         child nm             : {HsName}
         visit 1:
            local repl        : _
      alternative Dbg:
         child info           : {String}
         visit 1:
            local repl        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative Impls:
         child impls          : Impls 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative Pred:
         child pr             : Pred 
         visit 0:
            local allow       : _
         visit 1:
            local repl        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 0:
            local _tup1       : _
            local allow       : {Bool}
            local lUniq       : _
            local tvNew       : {UID}
            local _tup2       : {(Ty,VarMp)}
         visit 1:
            local repl        : _
            local l1Subst     : _
            local instTo      : _
            local _tup3       : {(Ty,TyVarIdL,[InstTo])}
            intra tvNew       : {UID}
            intra allow       : {Bool}
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 1:
            local repl        : _
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
type T_Ty  = Bool ->
             UID ->
             VarMp ->
             HowToInst ->
             (Maybe TyQu) ->
             Bool ->
             ( UID,VarMp,T_Ty_1 )
type T_Ty_1  = VarMp ->
               ( ([InstTo]),Ty ,TyVarIdL)
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _tyOallow ->
                (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                 { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                     (case (_tyIgUniq) of
                      { _lhsOgUniq ->
                      (case (_tyIgathSubst) of
                       { _lhsOgathSubst ->
                       (case ((let sem_Ty_Ann_1 :: T_Ty_1 
                                   sem_Ty_Ann_1  =
                                       (\ _lhsIreplSubst ->
                                            (case (_lhsIreplSubst) of
                                             { _tyOreplSubst ->
                                             (case (ty_1 _tyOreplSubst ) of
                                              { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                  (case (_tyIinstToL) of
                                                   { _lhsOinstToL ->
                                                   (case (ann_ ) of
                                                    { ( _annIrepl) ->
                                                        (case (Ty_Ann _annIrepl _tyIrepl) of
                                                         { _repl ->
                                                         (case (_repl) of
                                                          { _lhsOrepl ->
                                                          (case (_tyIreplTvL) of
                                                           { _lhsOreplTvL ->
                                                           ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }) }))
                               in  sem_Ty_Ann_1)) of
                        { ( sem_Ty_1) ->
                        ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Ty_Any_1 :: T_Ty_1 
                       sem_Ty_Any_1  =
                           (\ _lhsIreplSubst ->
                                (case ([]) of
                                 { _lhsOinstToL ->
                                 (case (Ty_Any) of
                                  { _repl ->
                                  (case (_repl) of
                                   { _lhsOrepl ->
                                   (case ([]) of
                                    { _lhsOreplTvL ->
                                    ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }))
                   in  sem_Ty_Any_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _funcOgUniq ->
          (case (_lhsIonlyExists) of
           { _funcOonlyExists ->
           (case (_lhsImbQ) of
            { _funcOmbQ ->
            (case (_lhsIhowToInst) of
             { _funcOhowToInst ->
             (case (_lhsIgathSubst) of
              { _funcOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _funcOallow ->
                (case (func_ _funcOallow _funcOgUniq _funcOgathSubst _funcOhowToInst _funcOmbQ _funcOonlyExists ) of
                 { ( _funcIgUniq,_funcIgathSubst,func_1) ->
                     (case (_funcIgUniq) of
                      { _argOgUniq ->
                      (case (_lhsIonlyExists) of
                       { _argOonlyExists ->
                       (case (_lhsImbQ) of
                        { _argOmbQ ->
                        (case (_lhsIhowToInst) of
                         { _argOhowToInst ->
                         (case (_funcIgathSubst) of
                          { _argOgathSubst ->
                          (case (_allow) of
                           { _argOallow ->
                           (case (arg_ _argOallow _argOgUniq _argOgathSubst _argOhowToInst _argOmbQ _argOonlyExists ) of
                            { ( _argIgUniq,_argIgathSubst,arg_1) ->
                                (case (_argIgUniq) of
                                 { _lhsOgUniq ->
                                 (case (_argIgathSubst) of
                                  { _lhsOgathSubst ->
                                  (case ((let sem_Ty_App_1 :: T_Ty_1 
                                              sem_Ty_App_1  =
                                                  (\ _lhsIreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _argOreplSubst ->
                                                        (case (_lhsIreplSubst) of
                                                         { _funcOreplSubst ->
                                                         (case (arg_1 _argOreplSubst ) of
                                                          { ( _argIinstToL,_argIrepl,_argIreplTvL) ->
                                                              (case (func_1 _funcOreplSubst ) of
                                                               { ( _funcIinstToL,_funcIrepl,_funcIreplTvL) ->
                                                                   (case (_funcIinstToL ++ _argIinstToL) of
                                                                    { _lhsOinstToL ->
                                                                    (case (Ty_App _funcIrepl _argIrepl) of
                                                                     { _repl ->
                                                                     (case (_repl) of
                                                                      { _lhsOrepl ->
                                                                      (case (_funcIreplTvL ++ _argIreplTvL) of
                                                                       { _lhsOreplTvL ->
                                                                       ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }) }) }))
                                          in  sem_Ty_App_1)) of
                                   { ( sem_Ty_1) ->
                                   ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Ty_Con_1 :: T_Ty_1 
                       sem_Ty_Con_1  =
                           (\ _lhsIreplSubst ->
                                (case ([]) of
                                 { _lhsOinstToL ->
                                 (case (Ty_Con nm_) of
                                  { _repl ->
                                  (case (_repl) of
                                   { _lhsOrepl ->
                                   (case ([]) of
                                    { _lhsOreplTvL ->
                                    ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }))
                   in  sem_Ty_Con_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Ty_Dbg_1 :: T_Ty_1 
                       sem_Ty_Dbg_1  =
                           (\ _lhsIreplSubst ->
                                (case ([]) of
                                 { _lhsOinstToL ->
                                 (case (Ty_Dbg info_) of
                                  { _repl ->
                                  (case (_repl) of
                                   { _lhsOrepl ->
                                   (case ([]) of
                                    { _lhsOreplTvL ->
                                    ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }))
                   in  sem_Ty_Dbg_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _tyOallow ->
                (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                 { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                     (case (_tyIgUniq) of
                      { _extTyOgUniq ->
                      (case (_lhsIonlyExists) of
                       { _extTyOonlyExists ->
                       (case (_lhsImbQ) of
                        { _extTyOmbQ ->
                        (case (_lhsIhowToInst) of
                         { _extTyOhowToInst ->
                         (case (_tyIgathSubst) of
                          { _extTyOgathSubst ->
                          (case (_allow) of
                           { _extTyOallow ->
                           (case (extTy_ _extTyOallow _extTyOgUniq _extTyOgathSubst _extTyOhowToInst _extTyOmbQ _extTyOonlyExists ) of
                            { ( _extTyIgUniq,_extTyIgathSubst,extTy_1) ->
                                (case (_extTyIgUniq) of
                                 { _lhsOgUniq ->
                                 (case (_extTyIgathSubst) of
                                  { _lhsOgathSubst ->
                                  (case ((let sem_Ty_Ext_1 :: T_Ty_1 
                                              sem_Ty_Ext_1  =
                                                  (\ _lhsIreplSubst ->
                                                       (case (_lhsIreplSubst) of
                                                        { _extTyOreplSubst ->
                                                        (case (_lhsIreplSubst) of
                                                         { _tyOreplSubst ->
                                                         (case (extTy_1 _extTyOreplSubst ) of
                                                          { ( _extTyIinstToL,_extTyIrepl,_extTyIreplTvL) ->
                                                              (case (ty_1 _tyOreplSubst ) of
                                                               { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                                   (case (_tyIinstToL ++ _extTyIinstToL) of
                                                                    { _lhsOinstToL ->
                                                                    (case (Ty_Ext _tyIrepl nm_ _extTyIrepl) of
                                                                     { _repl ->
                                                                     (case (_repl) of
                                                                      { _lhsOrepl ->
                                                                      (case (_tyIreplTvL ++ _extTyIreplTvL) of
                                                                       { _lhsOreplTvL ->
                                                                       ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }) }) }))
                                          in  sem_Ty_Ext_1)) of
                                   { ( sem_Ty_1) ->
                                   ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _implsOgUniq ->
          (case (_lhsIonlyExists) of
           { _implsOonlyExists ->
           (case (_lhsImbQ) of
            { _implsOmbQ ->
            (case (_lhsIhowToInst) of
             { _implsOhowToInst ->
             (case (_lhsIgathSubst) of
              { _implsOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _implsOallow ->
                (case (impls_ _implsOallow _implsOgUniq _implsOgathSubst _implsOhowToInst _implsOmbQ _implsOonlyExists ) of
                 { ( _implsIgUniq,_implsIgathSubst,impls_1) ->
                     (case (_implsIgUniq) of
                      { _lhsOgUniq ->
                      (case (_implsIgathSubst) of
                       { _lhsOgathSubst ->
                       (case ((let sem_Ty_Impls_1 :: T_Ty_1 
                                   sem_Ty_Impls_1  =
                                       (\ _lhsIreplSubst ->
                                            (case ([]) of
                                             { _lhsOinstToL ->
                                             (case (_lhsIreplSubst) of
                                              { _implsOreplSubst ->
                                              (case (impls_1 _implsOreplSubst ) of
                                               { ( _implsIrepl) ->
                                                   (case (Ty_Impls _implsIrepl) of
                                                    { _repl ->
                                                    (case (_repl) of
                                                     { _lhsOrepl ->
                                                     (case ([]) of
                                                      { _lhsOreplTvL ->
                                                      ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }))
                               in  sem_Ty_Impls_1)) of
                        { ( sem_Ty_1) ->
                        ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _tyOgUniq ->
          (case (_lhsIonlyExists) of
           { _tyOonlyExists ->
           (case (_lhsImbQ) of
            { _tyOmbQ ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (_lhsIgathSubst) of
              { _tyOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _tyOallow ->
                (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                 { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                     (case (_tyIgUniq) of
                      { _lhsOgUniq ->
                      (case (_tyIgathSubst) of
                       { _lhsOgathSubst ->
                       (case ((let sem_Ty_Lam_1 :: T_Ty_1 
                                   sem_Ty_Lam_1  =
                                       (\ _lhsIreplSubst ->
                                            (case (_lhsIreplSubst) of
                                             { _tyOreplSubst ->
                                             (case (ty_1 _tyOreplSubst ) of
                                              { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                  (case (_tyIinstToL) of
                                                   { _lhsOinstToL ->
                                                   (case (Ty_Lam tv_ _tyIrepl) of
                                                    { _repl ->
                                                    (case (_repl) of
                                                     { _lhsOrepl ->
                                                     (case (_tyIreplTvL) of
                                                      { _lhsOreplTvL ->
                                                      ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }))
                               in  sem_Ty_Lam_1)) of
                        { ( sem_Ty_1) ->
                        ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _prOgUniq ->
          (case (_lhsIonlyExists) of
           { _prOonlyExists ->
           (case (_lhsImbQ) of
            { _prOmbQ ->
            (case (_lhsIhowToInst) of
             { _prOhowToInst ->
             (case (_lhsIgathSubst) of
              { _prOgathSubst ->
              (case (False) of
               { _allow ->
               (case (_allow) of
                { _prOallow ->
                (case (pr_ _prOallow _prOgUniq _prOgathSubst _prOhowToInst _prOmbQ _prOonlyExists ) of
                 { ( _prIgUniq,_prIgathSubst,pr_1) ->
                     (case (_prIgUniq) of
                      { _lhsOgUniq ->
                      (case (_prIgathSubst) of
                       { _lhsOgathSubst ->
                       (case ((let sem_Ty_Pred_1 :: T_Ty_1 
                                   sem_Ty_Pred_1  =
                                       (\ _lhsIreplSubst ->
                                            (case ([]) of
                                             { _lhsOinstToL ->
                                             (case (_lhsIreplSubst) of
                                              { _prOreplSubst ->
                                              (case (pr_1 _prOreplSubst ) of
                                               { ( _prIrepl) ->
                                                   (case (Ty_Pred _prIrepl) of
                                                    { _repl ->
                                                    (case (_repl) of
                                                     { _lhsOrepl ->
                                                     (case ([]) of
                                                      { _lhsOreplTvL ->
                                                      ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }))
                               in  sem_Ty_Pred_1)) of
                        { ( sem_Ty_1) ->
                        ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (mkNewUID _lhsIgUniq) of
          { __tup1 ->
          (case (__tup1) of
           { (_tyOgUniq,_) ->
           (case (_lhsIonlyExists) of
            { _tyOonlyExists ->
            (case (_lhsIhowToInst) of
             { _tyOhowToInst ->
             (case (qu_ ) of
              { ( _quIrepl) ->
                  (case (_lhsIallow
                             && not (tyquIsForall _quIrepl && _lhsIonlyExists)
                             && maybe True (== _quIrepl) _lhsImbQ) of
                   { _allow ->
                   (case (_allow) of
                    { _tyOallow ->
                    (case (__tup1) of
                     { (_,_lUniq) ->
                     (case (_lUniq) of
                      { _tvNew ->
                      (case (if _allow
                             then let t = _lhsIhowToInst _quIrepl _tvNew
                                  in ( t
                                     , (varmpMetaLevTyUnit (tyquMetaLev _quIrepl) tv_ t)
                                         `varmpPlus` _lhsIgathSubst
                                     )
                             else (Ty_Any,_lhsIgathSubst)) of
                       { __tup2 ->
                       (case (__tup2) of
                        { (_,_tyOgathSubst) ->
                        (case (Just _quIrepl) of
                         { _tyOmbQ ->
                         (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                          { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                              (case (_tyIgUniq) of
                               { _lhsOgUniq ->
                               (case (_tyIgathSubst) of
                                { _lhsOgathSubst ->
                                (case ((let sem_Ty_TBind_1 :: T_Ty_1 
                                            sem_Ty_TBind_1  =
                                                (\ _lhsIreplSubst ->
                                                     (case (_lhsIreplSubst) of
                                                      { _tyOreplSubst ->
                                                      (case (ty_1 _tyOreplSubst ) of
                                                       { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                                                           (case (Ty_TBind _quIrepl tv_ l1_ _tyIrepl) of
                                                            { _repl ->
                                                            (case (varmpDecMetaLev _lhsIreplSubst `varUpd` l1_) of
                                                             { _l1Subst ->
                                                             (case (InstTo_Qu _quIrepl tv_ _tvNew _l1Subst) of
                                                              { _instTo ->
                                                              (case (if _allow
                                                                     then (_tyIrepl,_tvNew : _tyIreplTvL, _instTo : _tyIinstToL)
                                                                     else (_repl,[],[])) of
                                                               { __tup3 ->
                                                               (case (__tup3) of
                                                                { (_,_,_lhsOinstToL) ->
                                                                (case (__tup3) of
                                                                 { (_lhsOrepl,_,_) ->
                                                                 (case (__tup3) of
                                                                  { (_,_lhsOreplTvL,_) ->
                                                                  ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }) }) }) }))
                                        in  sem_Ty_TBind_1)) of
                                 { ( sem_Ty_1) ->
                                 ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsIallow
       _lhsIgUniq
       _lhsIgathSubst
       _lhsIhowToInst
       _lhsImbQ
       _lhsIonlyExists ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case (_lhsIgathSubst) of
           { _lhsOgathSubst ->
           (case ((let sem_Ty_Var_1 :: T_Ty_1 
                       sem_Ty_Var_1  =
                           (\ _lhsIreplSubst ->
                                (case ([]) of
                                 { _lhsOinstToL ->
                                 (case (categ_ ) of
                                  { ( _categIrepl) ->
                                      (case (Ty_Var tv_ _categIrepl) of
                                       { _repl ->
                                       (case (maybe _repl id (varmpTyLookup tv_ _lhsIreplSubst)) of
                                        { _lhsOrepl ->
                                        (case ([]) of
                                         { _lhsOreplTvL ->
                                         ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }))
                   in  sem_Ty_Var_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOgUniq,_lhsOgathSubst,sem_Ty_1) }) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gUniq                : UID
         howToInst            : HowToInst
         onlyExists           : Bool
      synthesized attributes:
         instToL              : [InstTo]
         repl                 : Ty 
         replTvL              : TyVarIdL
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
type T_TyAGItf  = UID ->
                  HowToInst ->
                  Bool ->
                  ( ([InstTo]),Ty ,TyVarIdL)
data Inh_TyAGItf  = Inh_TyAGItf {gUniq_Inh_TyAGItf :: !(UID),howToInst_Inh_TyAGItf :: !(HowToInst),onlyExists_Inh_TyAGItf :: !(Bool)}
data Syn_TyAGItf  = Syn_TyAGItf {instToL_Syn_TyAGItf :: !(([InstTo])),repl_Syn_TyAGItf :: !(Ty ),replTvL_Syn_TyAGItf :: !(TyVarIdL)}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIgUniq _lhsIhowToInst _lhsIonlyExists )  =
    (let ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) = sem _lhsIgUniq _lhsIhowToInst _lhsIonlyExists 
     in  (Syn_TyAGItf _lhsOinstToL _lhsOrepl _lhsOreplTvL ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIgUniq
       _lhsIhowToInst
       _lhsIonlyExists ->
         (case (_lhsIonlyExists) of
          { _tyOonlyExists ->
          (case (_lhsIhowToInst) of
           { _tyOhowToInst ->
           (case (_lhsIgUniq) of
            { _tyOgUniq ->
            (case (emptyVarMp) of
             { _tyOgathSubst ->
             (case (Nothing) of
              { _tyOmbQ ->
              (case (True) of
               { _tyOallow ->
               (case (ty_ _tyOallow _tyOgUniq _tyOgathSubst _tyOhowToInst _tyOmbQ _tyOonlyExists ) of
                { ( _tyIgUniq,_tyIgathSubst,ty_1) ->
                    (case (_tyIgathSubst) of
                     { _tyOreplSubst ->
                     (case (ty_1 _tyOreplSubst ) of
                      { ( _tyIinstToL,_tyIrepl,_tyIreplTvL) ->
                          (case (_tyIinstToL) of
                           { _lhsOinstToL ->
                           (case (_tyIrepl) of
                            { _lhsOrepl ->
                            (case (_tyIreplTvL) of
                             { _lhsOreplTvL ->
                             ( _lhsOinstToL,_lhsOrepl,_lhsOreplTvL) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
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
type T_TyAnn  = ( TyAnn )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (case (TyAnn_Empty) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (case (TyAnn_Mono) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (case (TyAnn_Strictness s_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
-- TyQu --------------------------------------------------------
{-
   visit 0:
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
type T_TyQu  = ( TyQu )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (case (TyQu_Exists mlev_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (case (TyQu_Forall mlev_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (case (TyQu_Plain mlev_) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
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
type T_TyVarCateg  = ( TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (case (TyVarCateg_Fixed) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (case (TyVarCateg_Meta) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (case (TyVarCateg_Plain) of
     { _repl ->
     (case (_repl) of
      { _lhsOrepl ->
      ( _lhsOrepl) }) })
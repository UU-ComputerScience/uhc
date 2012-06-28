

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/Trf/Subst.ag)
module EH101.Ty.Trf.Subst(tyAppVarLookup, tyAppVarLookup2) where

import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH.Util.Utils
import qualified Data.Map as Map
import EH101.VarLookup
import EH101.Base.Debug
import EH.Util.Pretty
import EH101.Ty.Pretty













tyAppVarLookup' :: VarLookup m TyVarId VarMpInfo => SubstOpts -> m -> TVUseMp -> Ty -> (Ty,VarMp)
tyAppVarLookup' opts m usemp ty
  = (repl_Syn_TyAGItf t,cycVarMp_Syn_TyAGItf t)
  where t = wrap_TyAGItf
              (sem_TyAGItf (TyAGItf_AGItf ty))
              (Inh_TyAGItf
                 { substOpts_Inh_TyAGItf 	= opts
                 , tvUseMp_Inh_TyAGItf 		= usemp
                 , lkup_Inh_TyAGItf 		= mkLkup m
                 })

tyAppVarLookup :: VarLookup m TyVarId VarMpInfo => m -> Ty -> Ty
tyAppVarLookup m ty
  = ty'
  where (ty',_) = tyAppVarLookup' defaultOpts m Map.empty ty

tyAppVarLookup2 :: VarLookup m TyVarId VarMpInfo => m -> Ty -> (Ty,VarMp)
tyAppVarLookup2 m ty
  = tyAppVarLookup' defaultOpts m Map.empty ty



data SubstOpts
  = SubstOpts

defaultOpts :: SubstOpts
defaultOpts = SubstOpts



type Lkup = MetaLev -> TyVarId -> Maybe VarMpInfo

mkLkup :: VarLookup m TyVarId VarMpInfo => m -> Lkup
mkLkup m = \mlev v -> {- trm "Subst.lkup" (\r -> mlev >#< v >#< r) $ -} varlookupWithMetaLev mlev v m



data TVUse
  = TVFree      -- is still free
  | TVBound     -- is already bound
  | TVSubst     -- is being substituted (for occur check implementation)

type TVUseMp = Map.Map TyVarId TVUse

tvUse :: TyVarId -> TVUseMp -> TVUse
tvUse tv = maybe TVFree id . Map.lookup tv



type IsBound = TyVarId -> Bool



tvRepl3 :: TyVarId -> TVUse -> (TyVarId -> x -> VarMp) -> x -> x -> x -> VarMp -> (x,VarMp)
tvRepl3 tv tvuse mk repl replv replho cycmpho
  = case tvuse of
      TVFree  -> (replho,cycmpho)
      TVBound -> (repl,emptyVarMp)
      TVSubst -> (repl,mk tv replv)



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
         lkup                 : Lkup
         substOpts            : SubstOpts
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
            local tvUse       : _
            local _tup1       : _
            local isRepl      : _
            local needRepl    : _
            local replv       : _
            inst  repl'       : Impls 
            local _tup2       : {(Impls,VarMp)}
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
type T_Impls  = Lkup ->
                SubstOpts ->
                TVUseMp ->
                ( VarMp,Impls )
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tlOtvUseMp ->
          (case (_lhsIlkup) of
           { _tlOlkup ->
           (case (_lhsItvUseMp) of
            { _prOtvUseMp ->
            (case (_lhsIlkup) of
             { _prOlkup ->
             (case (_lhsIsubstOpts) of
              { _tlOsubstOpts ->
              (case (tl_ _tlOlkup _tlOsubstOpts _tlOtvUseMp ) of
               { ( _tlIcycVarMp,_tlIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _prOsubstOpts ->
                    (case (pr_ _prOlkup _prOsubstOpts _prOtvUseMp ) of
                     { ( _prIcycVarMp,_prIrepl) ->
                         (case (_prIcycVarMp `varmpPlus` _tlIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (Impls_Cons iv_ _prIrepl pv_ prange_ proveOccs_ _tlIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (Impls_Nil) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (Impls_Tail iv_ proveOccs_) of
          { _repl ->
          (case (tvUse iv_ _lhsItvUseMp) of
           { _tvUse ->
           (case (maybe (_repl,False) (\t -> (t,True)) $ vmiMbImpls $? _lhsIlkup 0 iv_) of
            { __tup1 ->
            (case (__tup1) of
             { (_,_isRepl) ->
             (case (case _tvUse of
                       TVFree -> _isRepl
                       _      -> False) of
              { _needRepl ->
              (case (__tup1) of
               { (_replv,_) ->
               (case (if _needRepl then _replv else Impls_Nil) of
                { repl'_val_ ->
                (case ((sem_Impls repl'_val_ )) of
                 { repl'_inst_ ->
                 (case (_lhsIlkup) of
                  { _repl'Olkup ->
                  (case (Map.insert iv_ TVSubst _lhsItvUseMp) of
                   { _repl'OtvUseMp ->
                   (case (_lhsIsubstOpts) of
                    { _repl'OsubstOpts ->
                    (case (repl'_inst_ _repl'Olkup _repl'OsubstOpts _repl'OtvUseMp ) of
                     { ( _repl'IcycVarMp,_repl'Irepl) ->
                         (case (if _isRepl
                                then let replrepl = case _repl'Irepl of
                                                      Impls_Tail i occs -> Impls_Tail i (occs ++ proveOccs_)
                                                      r                 -> r
                                     in  tvRepl3 iv_ _tvUse varmpImplsUnit _repl _replv replrepl _repl'IcycVarMp
                                else (_repl,emptyVarMp)) of
                          { __tup2 ->
                          (case (__tup2) of
                           { (_,_lhsOcycVarMp) ->
                           (case (__tup2) of
                            { (_lhsOrepl,_) ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
            local tvUse       : _
            local _tup3       : _
            local isRepl      : _
            local needRepl    : _
            local replv       : _
            inst  repl'       : Label 
            local _tup4       : {(Label,VarMp)}
-}
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label (Label_Lab _nm )  =
    (sem_Label_Lab _nm )
sem_Label (Label_Var _lv )  =
    (sem_Label_Var _lv )
-- semantic domain
type T_Label  = Lkup ->
                TVUseMp ->
                ( VarMp,Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (\ _lhsIlkup
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (Label_Lab nm_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (\ _lhsIlkup
       _lhsItvUseMp ->
         (case (Label_Var lv_) of
          { _repl ->
          (case (tvUse lv_ _lhsItvUseMp) of
           { _tvUse ->
           (case (maybe (_repl,False) (\t -> (t,True)) $ vmiMbLabel $? _lhsIlkup 0 lv_) of
            { __tup3 ->
            (case (__tup3) of
             { (_,_isRepl) ->
             (case (case _tvUse of
                       TVFree -> _isRepl
                       _      -> False) of
              { _needRepl ->
              (case (__tup3) of
               { (_replv,_) ->
               (case (if _needRepl then _replv else Label_Lab (HsName_Base "")) of
                { repl'_val_ ->
                (case ((sem_Label repl'_val_ )) of
                 { repl'_inst_ ->
                 (case (_lhsIlkup) of
                  { _repl'Olkup ->
                  (case (Map.insert lv_ TVSubst _lhsItvUseMp) of
                   { _repl'OtvUseMp ->
                   (case (repl'_inst_ _repl'Olkup _repl'OtvUseMp ) of
                    { ( _repl'IcycVarMp,_repl'Irepl) ->
                        (case (if _isRepl
                               then tvRepl3 lv_ _tvUse varmpLabelUnit _repl _replv _repl'Irepl _repl'IcycVarMp
                               else (_repl,emptyVarMp)) of
                         { __tup4 ->
                         (case (__tup4) of
                          { (_,_lhsOcycVarMp) ->
                          (case (__tup4) of
                           { (_lhsOrepl,_) ->
                           ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- LabelAGItf --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         tvUseMp              : TVUseMp
      synthesized attribute:
         repl                 : Label 
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
type T_LabelAGItf  = Lkup ->
                     TVUseMp ->
                     ( Label )
sem_LabelAGItf_AGItf :: T_Label  ->
                        T_LabelAGItf 
sem_LabelAGItf_AGItf lab_  =
    (\ _lhsIlkup
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _labOtvUseMp ->
          (case (_lhsIlkup) of
           { _labOlkup ->
           (case (lab_ _labOlkup _labOtvUseMp ) of
            { ( _labIcycVarMp,_labIrepl) ->
                (case (_labIrepl) of
                 { _lhsOrepl ->
                 ( _lhsOrepl) }) }) }) }))
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         substOpts            : SubstOpts
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
            local tvUse       : _
            local _tup5       : _
            local isRepl      : _
            local needRepl    : _
            local replv       : _
            inst  repl'       : Pred 
            local _tup6       : {(Pred,VarMp)}
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
type T_Pred  = Lkup ->
               SubstOpts ->
               TVUseMp ->
               ( VarMp,Pred )
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _resOtvUseMp ->
          (case (_lhsIlkup) of
           { _resOlkup ->
           (case (_lhsItvUseMp) of
            { _argsOtvUseMp ->
            (case (_lhsIlkup) of
             { _argsOlkup ->
             (case (_lhsIsubstOpts) of
              { _resOsubstOpts ->
              (case (res_ _resOlkup _resOsubstOpts _resOtvUseMp ) of
               { ( _resIcycVarMp,_resIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _argsOsubstOpts ->
                    (case (args_ _argsOlkup _argsOsubstOpts _argsOtvUseMp ) of
                     { ( _argsIcycVarMp,_argsIrepl) ->
                         (case (_argsIcycVarMp `varmpPlus` _resIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (Pred_Arrow _argsIrepl _resIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tyOtvUseMp ->
          (case (_lhsIlkup) of
           { _tyOlkup ->
           (case (_lhsIsubstOpts) of
            { _tyOsubstOpts ->
            (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
             { ( _tyIcycVarMp,_tyIrepl) ->
                 (case (_tyIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Pred_Class _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tyROtvUseMp ->
          (case (_lhsIlkup) of
           { _tyROlkup ->
           (case (_lhsItvUseMp) of
            { _tyLOtvUseMp ->
            (case (_lhsIlkup) of
             { _tyLOlkup ->
             (case (_lhsIsubstOpts) of
              { _tyROsubstOpts ->
              (case (tyR_ _tyROlkup _tyROsubstOpts _tyROtvUseMp ) of
               { ( _tyRIcycVarMp,_tyRIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _tyLOsubstOpts ->
                    (case (tyL_ _tyLOlkup _tyLOsubstOpts _tyLOtvUseMp ) of
                     { ( _tyLIcycVarMp,_tyLIrepl) ->
                         (case (_tyLIcycVarMp `varmpPlus` _tyRIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (Pred_Eq _tyLIrepl _tyRIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _labOtvUseMp ->
          (case (_lhsIlkup) of
           { _labOlkup ->
           (case (_lhsItvUseMp) of
            { _tyOtvUseMp ->
            (case (_lhsIlkup) of
             { _tyOlkup ->
             (case (lab_ _labOlkup _labOtvUseMp ) of
              { ( _labIcycVarMp,_labIrepl) ->
                  (case (_lhsIsubstOpts) of
                   { _tyOsubstOpts ->
                   (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
                    { ( _tyIcycVarMp,_tyIrepl) ->
                        (case (_tyIcycVarMp `varmpPlus` _labIcycVarMp) of
                         { _lhsOcycVarMp ->
                         (case (Pred_Lacks _tyIrepl _labIrepl) of
                          { _repl ->
                          (case (_repl) of
                           { _lhsOrepl ->
                           ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tyOtvUseMp ->
          (case (_lhsIlkup) of
           { _tyOlkup ->
           (case (_lhsIsubstOpts) of
            { _tyOsubstOpts ->
            (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
             { ( _tyIcycVarMp,_tyIrepl) ->
                 (case (_tyIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Pred_Pred _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _seqOtvUseMp ->
          (case (_lhsIlkup) of
           { _seqOlkup ->
           (case (_lhsIsubstOpts) of
            { _seqOsubstOpts ->
            (case (seq_ _seqOlkup _seqOsubstOpts _seqOtvUseMp ) of
             { ( _seqIcycVarMp,_seqIrepl) ->
                 (case (_seqIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Pred_Preds _seqIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (Pred_Var pv_) of
          { _repl ->
          (case (tvUse pv_ _lhsItvUseMp) of
           { _tvUse ->
           (case (maybe (_repl,False) (\t -> (t,True)) $ vmiMbPred $? _lhsIlkup 0 pv_) of
            { __tup5 ->
            (case (__tup5) of
             { (_,_isRepl) ->
             (case (case _tvUse of
                       TVFree -> _isRepl
                       _      -> False) of
              { _needRepl ->
              (case (__tup5) of
               { (_replv,_) ->
               (case (if _needRepl then _replv else Pred_Pred Ty_Any) of
                { repl'_val_ ->
                (case ((sem_Pred repl'_val_ )) of
                 { repl'_inst_ ->
                 (case (_lhsIlkup) of
                  { _repl'Olkup ->
                  (case (Map.insert pv_ TVSubst _lhsItvUseMp) of
                   { _repl'OtvUseMp ->
                   (case (_lhsIsubstOpts) of
                    { _repl'OsubstOpts ->
                    (case (repl'_inst_ _repl'Olkup _repl'OsubstOpts _repl'OtvUseMp ) of
                     { ( _repl'IcycVarMp,_repl'Irepl) ->
                         (case (if _isRepl
                                then tvRepl3 pv_ _tvUse varmpPredUnit _repl _replv _repl'Irepl _repl'IcycVarMp
                                else (_repl,emptyVarMp)) of
                          { __tup6 ->
                          (case (__tup6) of
                           { (_,_lhsOcycVarMp) ->
                           (case (__tup6) of
                            { (_lhsOrepl,_) ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         substOpts            : SubstOpts
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
            local tvUse       : _
            local _tup7       : _
            local isRepl      : _
            local needRepl    : _
            local replv       : _
            inst  repl'       : PredSeq 
            local _tup8       : {(PredSeq,VarMp)}
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
type T_PredSeq  = Lkup ->
                  SubstOpts ->
                  TVUseMp ->
                  ( VarMp,PredSeq )
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tlOtvUseMp ->
          (case (_lhsIlkup) of
           { _tlOlkup ->
           (case (_lhsItvUseMp) of
            { _hdOtvUseMp ->
            (case (_lhsIlkup) of
             { _hdOlkup ->
             (case (_lhsIsubstOpts) of
              { _tlOsubstOpts ->
              (case (tl_ _tlOlkup _tlOsubstOpts _tlOtvUseMp ) of
               { ( _tlIcycVarMp,_tlIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _hdOsubstOpts ->
                    (case (hd_ _hdOlkup _hdOsubstOpts _hdOtvUseMp ) of
                     { ( _hdIcycVarMp,_hdIrepl) ->
                         (case (_hdIcycVarMp `varmpPlus` _tlIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (PredSeq_Cons _hdIrepl _tlIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (PredSeq_Nil) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (PredSeq_Var av_) of
          { _repl ->
          (case (tvUse av_ _lhsItvUseMp) of
           { _tvUse ->
           (case (maybe (_repl,False) (\t -> (t,True)) $ vmiMbPredSeq $? _lhsIlkup 0 av_) of
            { __tup7 ->
            (case (__tup7) of
             { (_,_isRepl) ->
             (case (case _tvUse of
                       TVFree -> _isRepl
                       _      -> False) of
              { _needRepl ->
              (case (__tup7) of
               { (_replv,_) ->
               (case (if _needRepl then _replv else PredSeq_Nil) of
                { repl'_val_ ->
                (case ((sem_PredSeq repl'_val_ )) of
                 { repl'_inst_ ->
                 (case (_lhsIlkup) of
                  { _repl'Olkup ->
                  (case (Map.insert av_ TVSubst _lhsItvUseMp) of
                   { _repl'OtvUseMp ->
                   (case (_lhsIsubstOpts) of
                    { _repl'OsubstOpts ->
                    (case (repl'_inst_ _repl'Olkup _repl'OsubstOpts _repl'OtvUseMp ) of
                     { ( _repl'IcycVarMp,_repl'Irepl) ->
                         (case (if _isRepl
                                then tvRepl3 av_ _tvUse varmpPredSeqUnit _repl _replv _repl'Irepl _repl'IcycVarMp
                                else (_repl,emptyVarMp)) of
                          { __tup8 ->
                          (case (__tup8) of
                           { (_,_lhsOcycVarMp) ->
                           (case (__tup8) of
                            { (_lhsOrepl,_) ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         substOpts            : SubstOpts
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
            inst  l1'         : Ty 
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 0:
            local repl        : _
            local tvUse       : _
            local _tup9       : _
            local isRepl      : _
            local needRepl    : _
            local replv       : _
            inst  repl'       : Ty 
            local _tup10      : {(Ty,VarMp)}
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
type T_Ty  = Lkup ->
             SubstOpts ->
             TVUseMp ->
             ( VarMp,Ty )
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tyOtvUseMp ->
          (case (_lhsIlkup) of
           { _tyOlkup ->
           (case (_lhsIsubstOpts) of
            { _tyOsubstOpts ->
            (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
             { ( _tyIcycVarMp,_tyIrepl) ->
                 (case (_lhsItvUseMp) of
                  { _annOtvUseMp ->
                  (case (ann_ _annOtvUseMp ) of
                   { ( _annIcycVarMp,_annIrepl) ->
                       (case (_annIcycVarMp `varmpPlus` _tyIcycVarMp) of
                        { _lhsOcycVarMp ->
                        (case (Ty_Ann _annIrepl _tyIrepl) of
                         { _repl ->
                         (case (tyCanonAnn _repl) of
                          { _lhsOrepl ->
                          ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }))
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (Ty_Any) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _argOtvUseMp ->
          (case (_lhsIlkup) of
           { _argOlkup ->
           (case (_lhsItvUseMp) of
            { _funcOtvUseMp ->
            (case (_lhsIlkup) of
             { _funcOlkup ->
             (case (_lhsIsubstOpts) of
              { _argOsubstOpts ->
              (case (arg_ _argOlkup _argOsubstOpts _argOtvUseMp ) of
               { ( _argIcycVarMp,_argIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _funcOsubstOpts ->
                    (case (func_ _funcOlkup _funcOsubstOpts _funcOtvUseMp ) of
                     { ( _funcIcycVarMp,_funcIrepl) ->
                         (case (_funcIcycVarMp `varmpPlus` _argIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (Ty_App _funcIrepl _argIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (Ty_Con nm_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (Ty_Dbg info_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _extTyOtvUseMp ->
          (case (_lhsIlkup) of
           { _extTyOlkup ->
           (case (_lhsItvUseMp) of
            { _tyOtvUseMp ->
            (case (_lhsIlkup) of
             { _tyOlkup ->
             (case (_lhsIsubstOpts) of
              { _extTyOsubstOpts ->
              (case (extTy_ _extTyOlkup _extTyOsubstOpts _extTyOtvUseMp ) of
               { ( _extTyIcycVarMp,_extTyIrepl) ->
                   (case (_lhsIsubstOpts) of
                    { _tyOsubstOpts ->
                    (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
                     { ( _tyIcycVarMp,_tyIrepl) ->
                         (case (_tyIcycVarMp `varmpPlus` _extTyIcycVarMp) of
                          { _lhsOcycVarMp ->
                          (case (Ty_Ext _tyIrepl nm_ _extTyIrepl) of
                           { _repl ->
                           (case (_repl) of
                            { _lhsOrepl ->
                            ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _implsOtvUseMp ->
          (case (_lhsIlkup) of
           { _implsOlkup ->
           (case (_lhsIsubstOpts) of
            { _implsOsubstOpts ->
            (case (impls_ _implsOlkup _implsOsubstOpts _implsOtvUseMp ) of
             { ( _implsIcycVarMp,_implsIrepl) ->
                 (case (_implsIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Ty_Impls _implsIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (Map.insert tv_ TVBound _lhsItvUseMp) of
          { _tyOtvUseMp ->
          (case (\mlev v -> if v == tv_ then Nothing else _lhsIlkup mlev v) of
           { _tyOlkup ->
           (case (_lhsIsubstOpts) of
            { _tyOsubstOpts ->
            (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
             { ( _tyIcycVarMp,_tyIrepl) ->
                 (case (_tyIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Ty_Lam tv_ _tyIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _prOtvUseMp ->
          (case (_lhsIlkup) of
           { _prOlkup ->
           (case (_lhsIsubstOpts) of
            { _prOsubstOpts ->
            (case (pr_ _prOlkup _prOsubstOpts _prOtvUseMp ) of
             { ( _prIcycVarMp,_prIrepl) ->
                 (case (_prIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (Ty_Pred _prIrepl) of
                   { _repl ->
                   (case (_repl) of
                    { _lhsOrepl ->
                    ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }))
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (l1_) of
          { l1'_val_ ->
          (case ((sem_Ty l1'_val_ )) of
           { l1'_inst_ ->
           (case (Map.empty) of
            { _l1'OtvUseMp ->
            (case (Map.insert tv_ TVBound _lhsItvUseMp) of
             { _tyOtvUseMp ->
             (case (\mlev v -> _lhsIlkup (mlev+1) v) of
              { _l1'Olkup ->
              (case (\mlev v -> if v == tv_ then Nothing else _lhsIlkup mlev v) of
               { _tyOlkup ->
               (case (_lhsIsubstOpts) of
                { _l1'OsubstOpts ->
                (case (l1'_inst_ _l1'Olkup _l1'OsubstOpts _l1'OtvUseMp ) of
                 { ( _l1'IcycVarMp,_l1'Irepl) ->
                     (case (_lhsIsubstOpts) of
                      { _tyOsubstOpts ->
                      (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
                       { ( _tyIcycVarMp,_tyIrepl) ->
                           (case (varmpIncMetaLev _l1'IcycVarMp `varmpPlus` _tyIcycVarMp) of
                            { _lhsOcycVarMp ->
                            (case (_lhsItvUseMp) of
                             { _quOtvUseMp ->
                             (case (qu_ _quOtvUseMp ) of
                              { ( _quIcycVarMp,_quIrepl) ->
                                  (case (Ty_TBind _quIrepl tv_ _l1'Irepl _tyIrepl) of
                                   { _lhsOrepl ->
                                   ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _categOtvUseMp ->
          (case (categ_ _categOtvUseMp ) of
           { ( _categIcycVarMp,_categIrepl) ->
               (case (Ty_Var tv_ _categIrepl) of
                { _repl ->
                (case (tvUse tv_ _lhsItvUseMp) of
                 { _tvUse ->
                 (case (maybe (_repl,False) (\t -> (t,True)) $ vmiMbTy $? _lhsIlkup 0 tv_) of
                  { __tup9 ->
                  (case (__tup9) of
                   { (_,_isRepl) ->
                   (case (case _tvUse of
                             TVFree -> _isRepl
                             _      -> False) of
                    { _needRepl ->
                    (case (__tup9) of
                     { (_replv,_) ->
                     (case (if _needRepl then _replv else Ty_Any) of
                      { repl'_val_ ->
                      (case ((sem_Ty repl'_val_ )) of
                       { repl'_inst_ ->
                       (case (_lhsIlkup) of
                        { _repl'Olkup ->
                        (case (Map.insert tv_ TVSubst _lhsItvUseMp) of
                         { _repl'OtvUseMp ->
                         (case (_lhsIsubstOpts) of
                          { _repl'OsubstOpts ->
                          (case (repl'_inst_ _repl'Olkup _repl'OsubstOpts _repl'OtvUseMp ) of
                           { ( _repl'IcycVarMp,_repl'Irepl) ->
                               (case (if _isRepl
                                      then tvRepl3 tv_ _tvUse varmpTyUnit _repl _replv _repl'Irepl _repl'IcycVarMp
                                      else (_repl,emptyVarMp)) of
                                { __tup10 ->
                                (case (__tup10) of
                                 { (_,_lhsOcycVarMp) ->
                                 (case (__tup10) of
                                  { (_lhsOrepl,_) ->
                                  ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkup                 : Lkup
         substOpts            : SubstOpts
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
type T_TyAGItf  = Lkup ->
                  SubstOpts ->
                  TVUseMp ->
                  ( VarMp,Ty )
data Inh_TyAGItf  = Inh_TyAGItf {lkup_Inh_TyAGItf :: !(Lkup),substOpts_Inh_TyAGItf :: !(SubstOpts),tvUseMp_Inh_TyAGItf :: !(TVUseMp)}
data Syn_TyAGItf  = Syn_TyAGItf {cycVarMp_Syn_TyAGItf :: !(VarMp),repl_Syn_TyAGItf :: !(Ty )}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIlkup _lhsIsubstOpts _lhsItvUseMp )  =
    (let ( _lhsOcycVarMp,_lhsOrepl) = sem _lhsIlkup _lhsIsubstOpts _lhsItvUseMp 
     in  (Syn_TyAGItf _lhsOcycVarMp _lhsOrepl ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIlkup
       _lhsIsubstOpts
       _lhsItvUseMp ->
         (case (_lhsItvUseMp) of
          { _tyOtvUseMp ->
          (case (_lhsIlkup) of
           { _tyOlkup ->
           (case (_lhsIsubstOpts) of
            { _tyOsubstOpts ->
            (case (ty_ _tyOlkup _tyOsubstOpts _tyOtvUseMp ) of
             { ( _tyIcycVarMp,_tyIrepl) ->
                 (case (_tyIcycVarMp) of
                  { _lhsOcycVarMp ->
                  (case (_tyIrepl) of
                   { _lhsOrepl ->
                   ( _lhsOcycVarMp,_lhsOrepl) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
type T_TyAnn  = TVUseMp ->
                ( VarMp,TyAnn )
sem_TyAnn_Empty :: T_TyAnn 
sem_TyAnn_Empty  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyAnn_Empty) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyAnn_Mono) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyAnn_Strictness s_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
-- TyQu --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
type T_TyQu  = TVUseMp ->
               ( VarMp,TyQu )
sem_TyQu_Exists :: MetaLev ->
                   T_TyQu 
sem_TyQu_Exists mlev_  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyQu_Exists mlev_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyQu_Forall mlev_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyQu_Plain mlev_) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
-- TyVarCateg --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         tvUseMp              : TVUseMp
      synthesized attributes:
         cycVarMp             : VarMp
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
type T_TyVarCateg  = TVUseMp ->
                     ( VarMp,TyVarCateg )
sem_TyVarCateg_Fixed :: T_TyVarCateg 
sem_TyVarCateg_Fixed  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyVarCateg_Fixed) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyVarCateg_Meta :: T_TyVarCateg 
sem_TyVarCateg_Meta  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyVarCateg_Meta) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
sem_TyVarCateg_Plain :: T_TyVarCateg 
sem_TyVarCateg_Plain  =
    (\ _lhsItvUseMp ->
         (case (emptyVarMp) of
          { _lhsOcycVarMp ->
          (case (TyVarCateg_Plain) of
           { _repl ->
           (case (_repl) of
            { _lhsOrepl ->
            ( _lhsOcycVarMp,_lhsOrepl) }) }) }))
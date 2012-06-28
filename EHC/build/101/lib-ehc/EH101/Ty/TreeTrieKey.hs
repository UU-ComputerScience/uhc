

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty/TreeTrieKey.ag)
module EH101.Ty.TreeTrieKey where

import EH101.Base.Common
import EH101.Base.HsName
import EH101.Base.Builtin
import EH101.Ty
import qualified Data.Set as Set
import Data.List as Lst
import EH.Util.FastSeq as Seq
import EH101.Base.TreeTrie
import EH101.CHR.Key







tyTreeTrieKey :: TTKeyableOpts -> Ty -> TreeTrieKey Key
tyTreeTrieKey opts ty
  =  let  t =  wrap_TyAGItf
                 (sem_TyAGItf (TyAGItf_AGItf ty))
                 (Inh_TyAGItf {opts_Inh_TyAGItf = opts})
     in   (key_Syn_TyAGItf t)



instance TTKeyable Ty where
  toTTKey' o = tyTreeTrieKey o

instance TTKeyable Pred where
  toTTKey' o pr = tyTreeTrieKey o $ mkTyPr pr

instance TTKeyable PredScope where
  toTTKey' o (PredScope_Var v) | ttkoptsVarsAsWild o = ttkSingleton TT1K_Any
                               | otherwise           = ttkSingleton (TT1K_One $ Key_UID v)
  toTTKey' o (PredScope_Lev l) = ttkSingleton $ TT1K_One $ Key_Str $ show l

instance TTKeyable CHRPredOccCxt where
  toTTKey' o (CHRPredOccCxt_Scope1 sc) = toTTKey' o sc -- for now

instance TTKeyable PredOcc where
  toTTKey' o po = toTTKey' o (poPr po)

instance TTKeyable CHRPredOcc where
  -- toTTKey' o po = ttkAdd (TT1K_One $ Key_Str "occ") [toTTKey' o (cpoCxt po), toTTKey' o (cpoPr po)]
  toTTKeyParentChildren' o po = (TT1K_One $ Key_Str "occ", ttkChildren [toTTKey' o (cpoCxt po), toTTKey' o (cpoPr po)])




instance TTKeyable PredOccId where
  toTTKey' o (PredOccId     i) = ttkSingleton $ TT1K_One $ Key_UID i



labelTreeTrieKey :: TTKeyableOpts -> Label -> TreeTrieKey Key
labelTreeTrieKey opts label
  =  let  t =  wrap_LabelAGItf
                 (sem_LabelAGItf (LabelAGItf_AGItf label))
                 (Inh_LabelAGItf {opts_Inh_LabelAGItf = opts})
     in   (key_Syn_LabelAGItf t)



instance TTKeyable LabelOffset where
  toTTKey' o (LabelOffset_Var v) | ttkoptsVarsAsWild o = ttkSingleton TT1K_Any
                                 | otherwise           = ttkSingleton (TT1K_One $ Key_UID v)
  toTTKey' o (LabelOffset_Off l) = ttkSingleton $ TT1K_One $ Key_Str $ show l

instance TTKeyable Label where
  toTTKey' = labelTreeTrieKey



(_,u1,u2,u3,u4) = mkNewLevUID4 uidStart
v1 = mkTyVar u1
v2 = mkTyVar u2
v3 = mkTyVar u3
v4 = mkTyVar u4

t1 = tyInt
t2 = mkArrow [t1] t1
t3 = mkArrow [t2] t2
t4 = mkArrow [v1] v2
t5 = mkArrow [v3] v4
t6 = mkArrow [t4] t5
t7 = mkArrow [v1] t5
t8 = mkArrow [t4] v3


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


data TyQuCtxt = TyQuCtxtArrow | TyQuCtxtProd | TyQuCtxtOnTop | TyQuCtxtOther deriving (Show,Eq)

-- Impls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TTKeyableOpts
         tyCtxt               : TyQuCtxt
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child iv             : {ImplsVarId}
         child pr             : Pred 
         child pv             : {PredOccId}
         child prange         : {Range}
         child proveOccs      : {[ImplsProveOcc]}
         child tl             : Impls 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
      alternative Tail:
         child iv             : {ImplsVarId}
         child proveOccs      : {[ImplsProveOcc]}
         visit 0:
            local self        : _
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
                Bool ->
                TTKeyableOpts ->
                TyQuCtxt ->
                ( Impls )
sem_Impls_Cons :: ImplsVarId ->
                  T_Pred  ->
                  PredOccId ->
                  Range ->
                  ([ImplsProveOcc]) ->
                  T_Impls  ->
                  T_Impls 
sem_Impls_Cons iv_ pr_ pv_ prange_ proveOccs_ tl_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsItyCtxt) of
          { _tlOtyCtxt ->
          (case (_lhsIopts) of
           { _tlOopts ->
           (case (_lhsIisRow) of
            { _tlOisRow ->
            (case (_lhsIisAtTop) of
             { _tlOisAtTop ->
             (case (tl_ _tlOisAtTop _tlOisRow _tlOopts _tlOtyCtxt ) of
              { ( _tlIself) ->
                  (case (_lhsItyCtxt) of
                   { _prOtyCtxt ->
                   (case (_lhsIopts) of
                    { _prOopts ->
                    (case (_lhsIisRow) of
                     { _prOisRow ->
                     (case (_lhsIisAtTop) of
                      { _prOisAtTop ->
                      (case (pr_ _prOisAtTop _prOisRow _prOopts _prOtyCtxt ) of
                       { ( _prIkey,_prIself) ->
                           (case (Impls_Cons iv_ _prIself pv_ prange_ proveOccs_ _tlIself) of
                            { _self ->
                            (case (_self) of
                             { _lhsOself ->
                             ( _lhsOself) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Impls_Nil :: T_Impls 
sem_Impls_Nil  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (Impls_Nil) of
          { _self ->
          (case (_self) of
           { _lhsOself ->
           ( _lhsOself) }) }))
sem_Impls_Tail :: ImplsVarId ->
                  ([ImplsProveOcc]) ->
                  T_Impls 
sem_Impls_Tail iv_ proveOccs_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (Impls_Tail iv_ proveOccs_) of
          { _self ->
          (case (_self) of
           { _lhsOself ->
           ( _lhsOself) }) }))
-- Label -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : TTKeyableOpts
      synthesized attributes:
         bkey                 : TreeTrie1Key Key
         key                  : TreeTrieKey Key
         self                 : SELF 
   alternatives:
      alternative Lab:
         child nm             : {HsName}
         visit 0:
            local bkey        : _
            local key         : _
            local self        : _
      alternative Var:
         child lv             : {LabelVarId}
         visit 0:
            local bkey        : _
            local key         : _
            local self        : _
-}
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label (Label_Lab _nm )  =
    (sem_Label_Lab _nm )
sem_Label (Label_Var _lv )  =
    (sem_Label_Var _lv )
-- semantic domain
type T_Label  = TTKeyableOpts ->
                ( (TreeTrie1Key Key),(TreeTrieKey Key),Label )
sem_Label_Lab :: HsName ->
                 T_Label 
sem_Label_Lab nm_  =
    (\ _lhsIopts ->
         (case (TT1K_One (Key_HNm nm_)) of
          { _bkey ->
          (case (_bkey) of
           { _lhsObkey ->
           (case (ttkSingleton _bkey) of
            { _key ->
            (case (_key) of
             { _lhsOkey ->
             (case (Label_Lab nm_) of
              { _self ->
              (case (_self) of
               { _lhsOself ->
               ( _lhsObkey,_lhsOkey,_lhsOself) }) }) }) }) }) }))
sem_Label_Var :: LabelVarId ->
                 T_Label 
sem_Label_Var lv_  =
    (\ _lhsIopts ->
         (case (if ttkoptsVarsAsWild _lhsIopts
                then TT1K_Any
                else TT1K_One (Key_UID lv_)) of
          { _bkey ->
          (case (_bkey) of
           { _lhsObkey ->
           (case (ttkSingleton _bkey) of
            { _key ->
            (case (_key) of
             { _lhsOkey ->
             (case (Label_Var lv_) of
              { _self ->
              (case (_self) of
               { _lhsOself ->
               ( _lhsObkey,_lhsOkey,_lhsOself) }) }) }) }) }) }))
-- LabelAGItf --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : TTKeyableOpts
      synthesized attribute:
         key                  : TreeTrieKey Key
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
type T_LabelAGItf  = TTKeyableOpts ->
                     ( (TreeTrieKey Key))
data Inh_LabelAGItf  = Inh_LabelAGItf {opts_Inh_LabelAGItf :: !(TTKeyableOpts)}
data Syn_LabelAGItf  = Syn_LabelAGItf {key_Syn_LabelAGItf :: !((TreeTrieKey Key))}
wrap_LabelAGItf :: T_LabelAGItf  ->
                   Inh_LabelAGItf  ->
                   Syn_LabelAGItf 
wrap_LabelAGItf sem (Inh_LabelAGItf _lhsIopts )  =
    (let ( _lhsOkey) = sem _lhsIopts 
     in  (Syn_LabelAGItf _lhsOkey ))
sem_LabelAGItf_AGItf :: T_Label  ->
                        T_LabelAGItf 
sem_LabelAGItf_AGItf lab_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _labOopts ->
          (case (lab_ _labOopts ) of
           { ( _labIbkey,_labIkey,_labIself) ->
               (case (_labIkey) of
                { _lhsOkey ->
                ( _lhsOkey) }) }) }))
-- Pred --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TTKeyableOpts
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         key                  : TreeTrieKey Key
         self                 : SELF 
   alternatives:
      alternative Arrow:
         child args           : PredSeq 
         child res            : Pred 
         visit 0:
            local appSpinePos : _
            local bkey        : _
            local key         : _
            local self        : _
      alternative Class:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local self        : _
      alternative Eq:
         child tyL            : Ty 
         child tyR            : Ty 
         visit 0:
            local appSpinePos : _
            local bkey        : _
            local key         : _
            local self        : _
      alternative Lacks:
         child ty             : Ty 
         child lab            : Label 
         visit 0:
            local appSpinePos : _
            local bkey        : _
            local key         : _
            local self        : _
      alternative Pred:
         child ty             : Ty 
         visit 0:
            local appSpinePos : _
            local self        : _
      alternative Preds:
         child seq            : PredSeq 
         visit 0:
            local appSpinePos : _
            local bkey        : _
            local key         : _
            local self        : _
      alternative Var:
         child pv             : {TyVarId}
         visit 0:
            local bkey        : _
            local key         : _
            local self        : _
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
               Bool ->
               TTKeyableOpts ->
               TyQuCtxt ->
               ( (TreeTrieKey Key),Pred )
sem_Pred_Arrow :: T_PredSeq  ->
                  T_Pred  ->
                  T_Pred 
sem_Pred_Arrow args_ res_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _argsOopts ->
          (case (0) of
           { _appSpinePos ->
           (case (_appSpinePos) of
            { _argsOappSpinePos ->
            (case (TT1K_One (Key_Str "=>")) of
             { _bkey ->
             (case (_lhsItyCtxt) of
              { _argsOtyCtxt ->
              (case (_lhsIisRow) of
               { _argsOisRow ->
               (case (_lhsIisAtTop) of
                { _argsOisAtTop ->
                (case (args_ _argsOappSpinePos _argsOisAtTop _argsOisRow _argsOopts _argsOtyCtxt ) of
                 { ( _argsIchildKeyL,_argsIkey,_argsIself) ->
                     (case (ttkAdd _bkey [_argsIkey]) of
                      { _key ->
                      (case (_key) of
                       { _lhsOkey ->
                       (case (_lhsItyCtxt) of
                        { _resOtyCtxt ->
                        (case (_lhsIopts) of
                         { _resOopts ->
                         (case (_lhsIisRow) of
                          { _resOisRow ->
                          (case (_lhsIisAtTop) of
                           { _resOisAtTop ->
                           (case (res_ _resOisAtTop _resOisRow _resOopts _resOtyCtxt ) of
                            { ( _resIkey,_resIself) ->
                                (case (Pred_Arrow _argsIself _resIself) of
                                 { _self ->
                                 (case (_self) of
                                  { _lhsOself ->
                                  ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Class :: T_Ty  ->
                  T_Pred 
sem_Pred_Class ty_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _tyOopts ->
          (case (0) of
           { _appSpinePos ->
           (case (_appSpinePos) of
            { _tyOappSpinePos ->
            (case (ty_ ) of
             { ( _tyIappFunNm,ty_1) ->
                 (case (_lhsItyCtxt) of
                  { _tyOtyCtxt ->
                  (case (_lhsIisRow) of
                   { _tyOisRow ->
                   (case (_lhsIisAtTop) of
                    { _tyOisAtTop ->
                    (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                     { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                         (case (_tyIkey) of
                          { _lhsOkey ->
                          (case (Pred_Class _tyIself) of
                           { _self ->
                           (case (_self) of
                            { _lhsOself ->
                            ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Eq :: T_Ty  ->
               T_Ty  ->
               T_Pred 
sem_Pred_Eq tyL_ tyR_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _tyROopts ->
          (case (0) of
           { _appSpinePos ->
           (case (_appSpinePos) of
            { _tyROappSpinePos ->
            (case (_lhsIopts) of
             { _tyLOopts ->
             (case (_appSpinePos) of
              { _tyLOappSpinePos ->
              (case (TT1K_One (Key_HNm hsnEqTilde)) of
               { _bkey ->
               (case (tyR_ ) of
                { ( _tyRIappFunNm,tyR_1) ->
                    (case (_lhsItyCtxt) of
                     { _tyROtyCtxt ->
                     (case (_lhsIisRow) of
                      { _tyROisRow ->
                      (case (_lhsIisAtTop) of
                       { _tyROisAtTop ->
                       (case (tyR_1 _tyROappSpinePos _tyROisAtTop _tyROisRow _tyROopts _tyROtyCtxt ) of
                        { ( _tyRIbkey,_tyRIchildKeyL,_tyRIisArrow,_tyRIisFixed,_tyRIisPred,_tyRIkey,_tyRIself) ->
                            (case (tyL_ ) of
                             { ( _tyLIappFunNm,tyL_1) ->
                                 (case (_lhsItyCtxt) of
                                  { _tyLOtyCtxt ->
                                  (case (_lhsIisRow) of
                                   { _tyLOisRow ->
                                   (case (_lhsIisAtTop) of
                                    { _tyLOisAtTop ->
                                    (case (tyL_1 _tyLOappSpinePos _tyLOisAtTop _tyLOisRow _tyLOopts _tyLOtyCtxt ) of
                                     { ( _tyLIbkey,_tyLIchildKeyL,_tyLIisArrow,_tyLIisFixed,_tyLIisPred,_tyLIkey,_tyLIself) ->
                                         (case (ttkAdd _bkey [_tyLIkey,_tyRIkey]) of
                                          { _key ->
                                          (case (_key) of
                                           { _lhsOkey ->
                                           (case (Pred_Eq _tyLIself _tyRIself) of
                                            { _self ->
                                            (case (_self) of
                                             { _lhsOself ->
                                             ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Lacks :: T_Ty  ->
                  T_Label  ->
                  T_Pred 
sem_Pred_Lacks ty_ lab_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _labOopts ->
          (case (_lhsIopts) of
           { _tyOopts ->
           (case (0) of
            { _appSpinePos ->
            (case (_appSpinePos) of
             { _tyOappSpinePos ->
             (case (TT1K_One (Key_Str "\\")) of
              { _bkey ->
              (case (lab_ _labOopts ) of
               { ( _labIbkey,_labIkey,_labIself) ->
                   (case (ty_ ) of
                    { ( _tyIappFunNm,ty_1) ->
                        (case (_lhsItyCtxt) of
                         { _tyOtyCtxt ->
                         (case (_lhsIisRow) of
                          { _tyOisRow ->
                          (case (_lhsIisAtTop) of
                           { _tyOisAtTop ->
                           (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                            { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                                (case (ttkAdd _bkey [ttkAdd _labIbkey _tyIchildKeyL]) of
                                 { _key ->
                                 (case (_key) of
                                  { _lhsOkey ->
                                  (case (Pred_Lacks _tyIself _labIself) of
                                   { _self ->
                                   (case (_self) of
                                    { _lhsOself ->
                                    ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Pred :: T_Ty  ->
                 T_Pred 
sem_Pred_Pred ty_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _tyOopts ->
          (case (0) of
           { _appSpinePos ->
           (case (_appSpinePos) of
            { _tyOappSpinePos ->
            (case (ty_ ) of
             { ( _tyIappFunNm,ty_1) ->
                 (case (_lhsItyCtxt) of
                  { _tyOtyCtxt ->
                  (case (_lhsIisRow) of
                   { _tyOisRow ->
                   (case (_lhsIisAtTop) of
                    { _tyOisAtTop ->
                    (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                     { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                         (case (_tyIkey) of
                          { _lhsOkey ->
                          (case (Pred_Pred _tyIself) of
                           { _self ->
                           (case (_self) of
                            { _lhsOself ->
                            ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Preds :: T_PredSeq  ->
                  T_Pred 
sem_Pred_Preds seq_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _seqOopts ->
          (case (0) of
           { _appSpinePos ->
           (case (_appSpinePos) of
            { _seqOappSpinePos ->
            (case (TT1K_One (Key_Str "seq")) of
             { _bkey ->
             (case (_lhsItyCtxt) of
              { _seqOtyCtxt ->
              (case (_lhsIisRow) of
               { _seqOisRow ->
               (case (_lhsIisAtTop) of
                { _seqOisAtTop ->
                (case (seq_ _seqOappSpinePos _seqOisAtTop _seqOisRow _seqOopts _seqOtyCtxt ) of
                 { ( _seqIchildKeyL,_seqIkey,_seqIself) ->
                     (case (ttkAdd _bkey [_seqIkey]) of
                      { _key ->
                      (case (_key) of
                       { _lhsOkey ->
                       (case (Pred_Preds _seqIself) of
                        { _self ->
                        (case (_self) of
                         { _lhsOself ->
                         ( _lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }))
sem_Pred_Var :: TyVarId ->
                T_Pred 
sem_Pred_Var pv_  =
    (\ _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (if ttkoptsVarsAsWild _lhsIopts
                then TT1K_Any
                else TT1K_One (Key_UID pv_)) of
          { _bkey ->
          (case (ttkSingleton _bkey) of
           { _key ->
           (case (_key) of
            { _lhsOkey ->
            (case (Pred_Var pv_) of
             { _self ->
             (case (_self) of
              { _lhsOself ->
              ( _lhsOkey,_lhsOself) }) }) }) }) }))
-- PredSeq -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appSpinePos          : Int
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TTKeyableOpts
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         childKeyL            : [TreeTrieKey Key]
         key                  : TreeTrieKey Key
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Pred 
         child tl             : PredSeq 
         visit 0:
            local childKeyL   : _
            local isSpineRoot : {Bool}
            local bkey        : _
            local key         : _
            local self        : _
      alternative Nil:
         visit 0:
            local childKeyL   : _
            local bkey        : _
            local key         : _
            local self        : _
      alternative Var:
         child av             : {TyVarId}
         visit 0:
            local childKeyL   : _
            local bkey        : _
            local key         : _
            local self        : _
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
type T_PredSeq  = Int ->
                  Bool ->
                  Bool ->
                  TTKeyableOpts ->
                  TyQuCtxt ->
                  ( ([TreeTrieKey Key]),(TreeTrieKey Key),PredSeq )
sem_PredSeq_Cons :: T_Pred  ->
                    T_PredSeq  ->
                    T_PredSeq 
sem_PredSeq_Cons hd_ tl_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case (_lhsIopts) of
          { _tlOopts ->
          (case (_lhsIopts) of
           { _hdOopts ->
           (case (_lhsItyCtxt) of
            { _tlOtyCtxt ->
            (case (_lhsIisRow) of
             { _tlOisRow ->
             (case (_lhsIisAtTop) of
              { _tlOisAtTop ->
              (case (_lhsIappSpinePos + 1) of
               { _tlOappSpinePos ->
               (case (tl_ _tlOappSpinePos _tlOisAtTop _tlOisRow _tlOopts _tlOtyCtxt ) of
                { ( _tlIchildKeyL,_tlIkey,_tlIself) ->
                    (case (_lhsItyCtxt) of
                     { _hdOtyCtxt ->
                     (case (_lhsIisRow) of
                      { _hdOisRow ->
                      (case (_lhsIisAtTop) of
                       { _hdOisAtTop ->
                       (case (hd_ _hdOisAtTop _hdOisRow _hdOopts _hdOtyCtxt ) of
                        { ( _hdIkey,_hdIself) ->
                            (case (_hdIkey : _tlIchildKeyL) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (_lhsIappSpinePos == 0) of
                               { _isSpineRoot ->
                               (case (TT1K_One (Key_Str ":")) of
                                { _bkey ->
                                (case (if _isSpineRoot
                                       then ttkAdd _bkey _childKeyL
                                       else []) of
                                 { _key ->
                                 (case (_key) of
                                  { _lhsOkey ->
                                  (case (PredSeq_Cons _hdIself _tlIself) of
                                   { _self ->
                                   (case (_self) of
                                    { _lhsOself ->
                                    ( _lhsOchildKeyL,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_PredSeq_Nil :: T_PredSeq 
sem_PredSeq_Nil  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case ([]) of
          { _childKeyL ->
          (case (_childKeyL) of
           { _lhsOchildKeyL ->
           (case (TT1K_One (Key_Str "[]")) of
            { _bkey ->
            (case (ttkSingleton _bkey) of
             { _key ->
             (case (_key) of
              { _lhsOkey ->
              (case (PredSeq_Nil) of
               { _self ->
               (case (_self) of
                { _lhsOself ->
                ( _lhsOchildKeyL,_lhsOkey,_lhsOself) }) }) }) }) }) }) }))
sem_PredSeq_Var :: TyVarId ->
                   T_PredSeq 
sem_PredSeq_Var av_  =
    (\ _lhsIappSpinePos
       _lhsIisAtTop
       _lhsIisRow
       _lhsIopts
       _lhsItyCtxt ->
         (case ([]) of
          { _childKeyL ->
          (case (_childKeyL) of
           { _lhsOchildKeyL ->
           (case (if ttkoptsVarsAsWild _lhsIopts
                  then TT1K_Any
                  else TT1K_One (Key_UID av_)) of
            { _bkey ->
            (case (ttkSingleton _bkey) of
             { _key ->
             (case (_key) of
              { _lhsOkey ->
              (case (PredSeq_Var av_) of
               { _self ->
               (case (_self) of
                { _lhsOself ->
                ( _lhsOchildKeyL,_lhsOkey,_lhsOself) }) }) }) }) }) }) }))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         appFunNm             : HsName
   visit 1:
      inherited attributes:
         appSpinePos          : Int
         isAtTop              : Bool
         isRow                : Bool
         opts                 : TTKeyableOpts
         tyCtxt               : TyQuCtxt
      synthesized attributes:
         bkey                 : TreeTrie1Key Key
         childKeyL            : [TreeTrieKey Key]
         isArrow              : Bool
         isFixed              : Bool
         isPred               : Bool
         key                  : TreeTrieKey Key
         self                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : TyAnn 
         child ty             : Ty 
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local self        : _
      alternative Any:
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local self        : _
      alternative App:
         child func           : Ty 
         child arg            : Ty 
         visit 0:
            local appFunNm    : {HsName}
         visit 1:
            local appIsRec    : {Bool}
            local appIsLikeProd : {Bool}
            local appIsArrow  : {Bool}
            local tyCtxt      : _
            local isAtTop     : _
            local bkey        : _
            local isSpineRoot : {Bool}
            local appIsSum    : _
            local appIsRecOrSum : _
            local argIsRow    : {Bool}
            local childKeyL   : _
            local isArrowRoot : {Bool}
            local isArrowArg  : {Bool}
            local key         : _
            local self        : _
      alternative Con:
         child nm             : {HsName}
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local self        : _
      alternative Dbg:
         child info           : {String}
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local self        : _
      alternative Ext:
         child ty             : Ty 
         child nm             : {HsName}
         child extTy          : Ty 
         visit 0:
            local appFunNm    : {HsName}
         visit 1:
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local self        : _
            local isSpineRoot : _
            local bkey        : _
            local childKeyL   : _
            local key         : _
      alternative Impls:
         child impls          : Impls 
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local self        : _
      alternative Lam:
         child tv             : {TyVarId}
         child ty             : Ty 
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local self        : _
      alternative Pred:
         child pr             : Pred 
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local self        : _
      alternative TBind:
         child qu             : TyQu 
         child tv             : {TyVarId}
         child l1             : {Ty}
         child ty             : Ty 
         visit 1:
            local bkey        : _
            local tyCtxt      : _
            local isRow       : _
            local isAtTop     : _
            local childKeyL   : _
            local key         : _
            local self        : _
      alternative Var:
         child tv             : {TyVarId}
         child categ          : TyVarCateg 
         visit 1:
            local bkey        : _
            local childKeyL   : _
            local key         : _
            local self        : _
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
type T_Ty  = ( HsName,T_Ty_1 )
type T_Ty_1  = Int ->
               Bool ->
               Bool ->
               TTKeyableOpts ->
               TyQuCtxt ->
               ( (TreeTrie1Key Key),([TreeTrieKey Key]),Bool,Bool,Bool,(TreeTrieKey Key),Ty )
sem_Ty_Ann :: T_TyAnn  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ann ann_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Ann_1 :: T_Ty_1 
                 sem_Ty_Ann_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (_lhsIopts) of
                                  { _tyOopts ->
                                  (case (_lhsIappSpinePos) of
                                   { _tyOappSpinePos ->
                                   (case (ty_ ) of
                                    { ( _tyIappFunNm,ty_1) ->
                                        (case (TyQuCtxtOther) of
                                         { _tyCtxt ->
                                         (case (_tyCtxt) of
                                          { _tyOtyCtxt ->
                                          (case (False) of
                                           { _isRow ->
                                           (case (_isRow) of
                                            { _tyOisRow ->
                                            (case (False) of
                                             { _isAtTop ->
                                             (case (_isAtTop) of
                                              { _tyOisAtTop ->
                                              (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                                               { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                                                   (case (_tyIkey) of
                                                    { _lhsOkey ->
                                                    (case (ann_ ) of
                                                     { ( _annIself) ->
                                                         (case (Ty_Ann _annIself _tyIself) of
                                                          { _self ->
                                                          (case (_self) of
                                                           { _lhsOself ->
                                                           ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Ann_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Any :: T_Ty 
sem_Ty_Any  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Any_1 :: T_Ty_1 
                 sem_Ty_Any_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (ttkSingleton _bkey) of
                                  { _key ->
                                  (case (_key) of
                                   { _lhsOkey ->
                                   (case (Ty_Any) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Any_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App func_ arg_  =
    (case (func_ ) of
     { ( _funcIappFunNm,func_1) ->
         (case (_funcIappFunNm) of
          { _appFunNm ->
          (case (_appFunNm) of
           { _lhsOappFunNm ->
           (case ((let sem_Ty_App_1 :: T_Ty_1 
                       sem_Ty_App_1  =
                           (\ _lhsIappSpinePos
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIopts
                              _lhsItyCtxt ->
                                (case (_lhsIopts) of
                                 { _funcOopts ->
                                 (case (_lhsIappSpinePos + 1) of
                                  { _funcOappSpinePos ->
                                  (case (hsnIsRec _funcIappFunNm) of
                                   { _appIsRec ->
                                   (case (hsnIsProd _funcIappFunNm || _appIsRec) of
                                    { _appIsLikeProd ->
                                    (case (hsnIsArrow _funcIappFunNm) of
                                     { _appIsArrow ->
                                     (case (if      _appIsArrow     then TyQuCtxtArrow
                                            else if _appIsLikeProd  then TyQuCtxtProd
                                                                    else TyQuCtxtOther) of
                                      { _tyCtxt ->
                                      (case (_tyCtxt) of
                                       { _funcOtyCtxt ->
                                       (case (_lhsIisRow) of
                                        { _funcOisRow ->
                                        (case (False) of
                                         { _isAtTop ->
                                         (case (_isAtTop) of
                                          { _funcOisAtTop ->
                                          (case (func_1 _funcOappSpinePos _funcOisAtTop _funcOisRow _funcOopts _funcOtyCtxt ) of
                                           { ( _funcIbkey,_funcIchildKeyL,_funcIisArrow,_funcIisFixed,_funcIisPred,_funcIkey,_funcIself) ->
                                               (case (_funcIbkey) of
                                                { _bkey ->
                                                (case (_bkey) of
                                                 { _lhsObkey ->
                                                 (case (_lhsIopts) of
                                                  { _argOopts ->
                                                  (case (_lhsIappSpinePos == 0) of
                                                   { _isSpineRoot ->
                                                   (case (0) of
                                                    { _argOappSpinePos ->
                                                    (case (arg_ ) of
                                                     { ( _argIappFunNm,arg_1) ->
                                                         (case (_tyCtxt) of
                                                          { _argOtyCtxt ->
                                                          (case (_isAtTop) of
                                                           { _argOisAtTop ->
                                                           (case (hsnIsSum _funcIappFunNm) of
                                                            { _appIsSum ->
                                                            (case (_appIsRec || _appIsSum) of
                                                             { _appIsRecOrSum ->
                                                             (case (_isSpineRoot && _appIsRecOrSum) of
                                                              { _argIsRow ->
                                                              (case (_argIsRow) of
                                                               { _argOisRow ->
                                                               (case (arg_1 _argOappSpinePos _argOisAtTop _argOisRow _argOopts _argOtyCtxt ) of
                                                                { ( _argIbkey,_argIchildKeyL,_argIisArrow,_argIisFixed,_argIisPred,_argIkey,_argIself) ->
                                                                    (case (let cs = _argIkey : _funcIchildKeyL
                                                                           in  if _isSpineRoot
                                                                               then reverse cs
                                                                               else cs) of
                                                                     { _childKeyL ->
                                                                     (case (_childKeyL) of
                                                                      { _lhsOchildKeyL ->
                                                                      (case (_appIsArrow && _isSpineRoot) of
                                                                       { _isArrowRoot ->
                                                                       (case (_isArrowRoot) of
                                                                        { _lhsOisArrow ->
                                                                        (case (True) of
                                                                         { _lhsOisFixed ->
                                                                         (case (_appIsArrow && _lhsIappSpinePos == 1) of
                                                                          { _isArrowArg ->
                                                                          (case (if _isArrowArg then _argIisPred else False) of
                                                                           { _lhsOisPred ->
                                                                           (case (if _isSpineRoot
                                                                                  then ttkAdd _bkey _childKeyL
                                                                                  else []) of
                                                                            { _key ->
                                                                            (case (_key) of
                                                                             { _lhsOkey ->
                                                                             (case (Ty_App _funcIself _argIself) of
                                                                              { _self ->
                                                                              (case (_self) of
                                                                               { _lhsOself ->
                                                                               ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_App_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Con :: HsName ->
              T_Ty 
sem_Ty_Con nm_  =
    (case (nm_) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Con_1 :: T_Ty_1 
                 sem_Ty_Con_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_One (Key_HNm nm_)) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (ttkSingleton _bkey) of
                                  { _key ->
                                  (case (_key) of
                                   { _lhsOkey ->
                                   (case (Ty_Con nm_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Con_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Dbg :: String ->
              T_Ty 
sem_Ty_Dbg info_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Dbg_1 :: T_Ty_1 
                 sem_Ty_Dbg_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (ttkSingleton _bkey) of
                                  { _key ->
                                  (case (_key) of
                                   { _lhsOkey ->
                                   (case (Ty_Dbg info_) of
                                    { _self ->
                                    (case (_self) of
                                     { _lhsOself ->
                                     ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Dbg_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Ext :: T_Ty  ->
              HsName ->
              T_Ty  ->
              T_Ty 
sem_Ty_Ext ty_ nm_ extTy_  =
    (case (ty_ ) of
     { ( _tyIappFunNm,ty_1) ->
         (case (_tyIappFunNm) of
          { _appFunNm ->
          (case (_appFunNm) of
           { _lhsOappFunNm ->
           (case ((let sem_Ty_Ext_1 :: T_Ty_1 
                       sem_Ty_Ext_1  =
                           (\ _lhsIappSpinePos
                              _lhsIisAtTop
                              _lhsIisRow
                              _lhsIopts
                              _lhsItyCtxt ->
                                (case (extTy_ ) of
                                 { ( _extTyIappFunNm,extTy_1) ->
                                     (case (TyQuCtxtOther) of
                                      { _tyCtxt ->
                                      (case (_tyCtxt) of
                                       { _extTyOtyCtxt ->
                                       (case (_lhsIopts) of
                                        { _extTyOopts ->
                                        (case (False) of
                                         { _isRow ->
                                         (case (_isRow) of
                                          { _extTyOisRow ->
                                          (case (False) of
                                           { _isAtTop ->
                                           (case (_isAtTop) of
                                            { _extTyOisAtTop ->
                                            (case (0) of
                                             { _extTyOappSpinePos ->
                                             (case (extTy_1 _extTyOappSpinePos _extTyOisAtTop _extTyOisRow _extTyOopts _extTyOtyCtxt ) of
                                              { ( _extTyIbkey,_extTyIchildKeyL,_extTyIisArrow,_extTyIisFixed,_extTyIisPred,_extTyIkey,_extTyIself) ->
                                                  (case (_tyCtxt) of
                                                   { _tyOtyCtxt ->
                                                   (case (_lhsIopts) of
                                                    { _tyOopts ->
                                                    (case (_isRow) of
                                                     { _tyOisRow ->
                                                     (case (_isAtTop) of
                                                      { _tyOisAtTop ->
                                                      (case (_lhsIappSpinePos + 1) of
                                                       { _tyOappSpinePos ->
                                                       (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                                                        { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                                                            (case (Ty_Ext _tyIself nm_ _extTyIself) of
                                                             { _self ->
                                                             (case (_lhsIappSpinePos == 0) of
                                                              { _isSpineRoot ->
                                                              (case (if _isSpineRoot && _tyIisFixed
                                                                     then let (_,exts) = tyRowExts _self
                                                                          in  TT1K_One (Key_Str $ concat $ intersperse "," $ Lst.map show $ assocLKeys exts)
                                                                     else TT1K_Any) of
                                                               { _bkey ->
                                                               (case (_bkey) of
                                                                { _lhsObkey ->
                                                                (case (let cs = _extTyIkey : _tyIchildKeyL
                                                                       in  if _isSpineRoot
                                                                           then reverse cs
                                                                           else cs) of
                                                                 { _childKeyL ->
                                                                 (case (_childKeyL) of
                                                                  { _lhsOchildKeyL ->
                                                                  (case (False) of
                                                                   { _lhsOisArrow ->
                                                                   (case (_tyIisFixed) of
                                                                    { _lhsOisFixed ->
                                                                    (case (False) of
                                                                     { _lhsOisPred ->
                                                                     (case (if _isSpineRoot
                                                                            then ttkAdd _bkey _childKeyL
                                                                            else []) of
                                                                      { _key ->
                                                                      (case (_key) of
                                                                       { _lhsOkey ->
                                                                       (case (_self) of
                                                                        { _lhsOself ->
                                                                        ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Ty_Ext_1)) of
            { ( sem_Ty_1) ->
            ( _lhsOappFunNm,sem_Ty_1) }) }) }) })
sem_Ty_Impls :: T_Impls  ->
                T_Ty 
sem_Ty_Impls impls_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Impls_1 :: T_Ty_1 
                 sem_Ty_Impls_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (True) of
                                 { _lhsOisPred ->
                                 (case (ttkSingleton _bkey) of
                                  { _key ->
                                  (case (_key) of
                                   { _lhsOkey ->
                                   (case (TyQuCtxtOther) of
                                    { _tyCtxt ->
                                    (case (_tyCtxt) of
                                     { _implsOtyCtxt ->
                                     (case (_lhsIopts) of
                                      { _implsOopts ->
                                      (case (False) of
                                       { _isRow ->
                                       (case (_isRow) of
                                        { _implsOisRow ->
                                        (case (False) of
                                         { _isAtTop ->
                                         (case (_isAtTop) of
                                          { _implsOisAtTop ->
                                          (case (impls_ _implsOisAtTop _implsOisRow _implsOopts _implsOtyCtxt ) of
                                           { ( _implsIself) ->
                                               (case (Ty_Impls _implsIself) of
                                                { _self ->
                                                (case (_self) of
                                                 { _lhsOself ->
                                                 ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Impls_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Lam :: TyVarId ->
              T_Ty  ->
              T_Ty 
sem_Ty_Lam tv_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Lam_1 :: T_Ty_1 
                 sem_Ty_Lam_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (False) of
                                 { _lhsOisPred ->
                                 (case (ttkSingleton _bkey) of
                                  { _key ->
                                  (case (_key) of
                                   { _lhsOkey ->
                                   (case (ty_ ) of
                                    { ( _tyIappFunNm,ty_1) ->
                                        (case (TyQuCtxtOther) of
                                         { _tyCtxt ->
                                         (case (_tyCtxt) of
                                          { _tyOtyCtxt ->
                                          (case (_lhsIopts) of
                                           { _tyOopts ->
                                           (case (False) of
                                            { _isRow ->
                                            (case (_isRow) of
                                             { _tyOisRow ->
                                             (case (False) of
                                              { _isAtTop ->
                                              (case (_isAtTop) of
                                               { _tyOisAtTop ->
                                               (case (_lhsIappSpinePos) of
                                                { _tyOappSpinePos ->
                                                (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                                                 { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                                                     (case (Ty_Lam tv_ _tyIself) of
                                                      { _self ->
                                                      (case (_self) of
                                                       { _lhsOself ->
                                                       ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Lam_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Pred :: T_Pred  ->
               T_Ty 
sem_Ty_Pred pr_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Pred_1 :: T_Ty_1 
                 sem_Ty_Pred_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (TT1K_Any) of
                           { _bkey ->
                           (case (_bkey) of
                            { _lhsObkey ->
                            (case ([]) of
                             { _childKeyL ->
                             (case (_childKeyL) of
                              { _lhsOchildKeyL ->
                              (case (False) of
                               { _lhsOisArrow ->
                               (case (True) of
                                { _lhsOisFixed ->
                                (case (True) of
                                 { _lhsOisPred ->
                                 (case (_lhsIopts) of
                                  { _prOopts ->
                                  (case (TyQuCtxtOther) of
                                   { _tyCtxt ->
                                   (case (_tyCtxt) of
                                    { _prOtyCtxt ->
                                    (case (False) of
                                     { _isRow ->
                                     (case (_isRow) of
                                      { _prOisRow ->
                                      (case (False) of
                                       { _isAtTop ->
                                       (case (_isAtTop) of
                                        { _prOisAtTop ->
                                        (case (pr_ _prOisAtTop _prOisRow _prOopts _prOtyCtxt ) of
                                         { ( _prIkey,_prIself) ->
                                             (case (_prIkey) of
                                              { _lhsOkey ->
                                              (case (Ty_Pred _prIself) of
                                               { _self ->
                                               (case (_self) of
                                                { _lhsOself ->
                                                ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Pred_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_TBind :: T_TyQu  ->
                TyVarId ->
                Ty ->
                T_Ty  ->
                T_Ty 
sem_Ty_TBind qu_ tv_ l1_ ty_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_TBind_1 :: T_Ty_1 
                 sem_Ty_TBind_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (qu_ ) of
                           { ( _quIself) ->
                               (case (TT1K_One (Key_TyQu _quIself)) of
                                { _bkey ->
                                (case (_bkey) of
                                 { _lhsObkey ->
                                 (case (_lhsIopts) of
                                  { _tyOopts ->
                                  (case (0) of
                                   { _tyOappSpinePos ->
                                   (case (ty_ ) of
                                    { ( _tyIappFunNm,ty_1) ->
                                        (case (TyQuCtxtOther) of
                                         { _tyCtxt ->
                                         (case (_tyCtxt) of
                                          { _tyOtyCtxt ->
                                          (case (False) of
                                           { _isRow ->
                                           (case (_isRow) of
                                            { _tyOisRow ->
                                            (case (False) of
                                             { _isAtTop ->
                                             (case (_isAtTop) of
                                              { _tyOisAtTop ->
                                              (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                                               { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                                                   (case ([_tyIkey]) of
                                                    { _childKeyL ->
                                                    (case (_childKeyL) of
                                                     { _lhsOchildKeyL ->
                                                     (case (False) of
                                                      { _lhsOisArrow ->
                                                      (case (True) of
                                                       { _lhsOisFixed ->
                                                       (case (False) of
                                                        { _lhsOisPred ->
                                                        (case (ttkAdd _bkey _childKeyL) of
                                                         { _key ->
                                                         (case (_key) of
                                                          { _lhsOkey ->
                                                          (case (Ty_TBind _quIself tv_ l1_ _tyIself) of
                                                           { _self ->
                                                           (case (_self) of
                                                            { _lhsOself ->
                                                            ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_TBind_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
sem_Ty_Var :: TyVarId ->
              T_TyVarCateg  ->
              T_Ty 
sem_Ty_Var tv_ categ_  =
    (case (hsnUnknown) of
     { _lhsOappFunNm ->
     (case ((let sem_Ty_Var_1 :: T_Ty_1 
                 sem_Ty_Var_1  =
                     (\ _lhsIappSpinePos
                        _lhsIisAtTop
                        _lhsIisRow
                        _lhsIopts
                        _lhsItyCtxt ->
                          (case (categ_ ) of
                           { ( _categIself) ->
                               (case (if tvCatIsFixed _categIself || not (ttkoptsVarsAsWild _lhsIopts)
                                      then TT1K_One (Key_UID tv_)
                                      else TT1K_Any) of
                                { _bkey ->
                                (case (_bkey) of
                                 { _lhsObkey ->
                                 (case ([]) of
                                  { _childKeyL ->
                                  (case (_childKeyL) of
                                   { _lhsOchildKeyL ->
                                   (case (False) of
                                    { _lhsOisArrow ->
                                    (case (False) of
                                     { _lhsOisFixed ->
                                     (case (False) of
                                      { _lhsOisPred ->
                                      (case (ttkSingleton _bkey) of
                                       { _key ->
                                       (case (_key) of
                                        { _lhsOkey ->
                                        (case (Ty_Var tv_ _categIself) of
                                         { _self ->
                                         (case (_self) of
                                          { _lhsOself ->
                                          ( _lhsObkey,_lhsOchildKeyL,_lhsOisArrow,_lhsOisFixed,_lhsOisPred,_lhsOkey,_lhsOself) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_Ty_Var_1)) of
      { ( sem_Ty_1) ->
      ( _lhsOappFunNm,sem_Ty_1) }) })
-- TyAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : TTKeyableOpts
      synthesized attribute:
         key                  : TreeTrieKey Key
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
type T_TyAGItf  = TTKeyableOpts ->
                  ( (TreeTrieKey Key))
data Inh_TyAGItf  = Inh_TyAGItf {opts_Inh_TyAGItf :: !(TTKeyableOpts)}
data Syn_TyAGItf  = Syn_TyAGItf {key_Syn_TyAGItf :: !((TreeTrieKey Key))}
wrap_TyAGItf :: T_TyAGItf  ->
                Inh_TyAGItf  ->
                Syn_TyAGItf 
wrap_TyAGItf sem (Inh_TyAGItf _lhsIopts )  =
    (let ( _lhsOkey) = sem _lhsIopts 
     in  (Syn_TyAGItf _lhsOkey ))
sem_TyAGItf_AGItf :: T_Ty  ->
                     T_TyAGItf 
sem_TyAGItf_AGItf ty_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _tyOopts ->
          (case (0) of
           { _tyOappSpinePos ->
           (case (ty_ ) of
            { ( _tyIappFunNm,ty_1) ->
                (case (TyQuCtxtOnTop) of
                 { _tyOtyCtxt ->
                 (case (False) of
                  { _tyOisRow ->
                  (case (True) of
                   { _tyOisAtTop ->
                   (case (ty_1 _tyOappSpinePos _tyOisAtTop _tyOisRow _tyOopts _tyOtyCtxt ) of
                    { ( _tyIbkey,_tyIchildKeyL,_tyIisArrow,_tyIisFixed,_tyIisPred,_tyIkey,_tyIself) ->
                        (case (_tyIkey) of
                         { _lhsOkey ->
                         ( _lhsOkey) }) }) }) }) }) }) }) }))
-- TyAnn -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local self        : _
      alternative Mono:
         visit 0:
            local self        : _
      alternative Strictness:
         child s              : {Strictness}
         visit 0:
            local self        : _
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
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyAnn_Mono :: T_TyAnn 
sem_TyAnn_Mono  =
    (case (TyAnn_Mono) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyAnn_Strictness :: Strictness ->
                        T_TyAnn 
sem_TyAnn_Strictness s_  =
    (case (TyAnn_Strictness s_) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
-- TyQu --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Exists:
         child mlev           : {MetaLev}
         visit 0:
            local self        : _
      alternative Forall:
         child mlev           : {MetaLev}
         visit 0:
            local self        : _
      alternative Plain:
         child mlev           : {MetaLev}
         visit 0:
            local self        : _
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
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyQu_Forall :: MetaLev ->
                   T_TyQu 
sem_TyQu_Forall mlev_  =
    (case (TyQu_Forall mlev_) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
sem_TyQu_Plain :: MetaLev ->
                  T_TyQu 
sem_TyQu_Plain mlev_  =
    (case (TyQu_Plain mlev_) of
     { _self ->
     (case (_self) of
      { _lhsOself ->
      ( _lhsOself) }) })
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
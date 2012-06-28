

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/LetFlattenStrict.a)
module EH101.Core.Trf.LetFlattenStrict(cmodTrfLetFlattenStrict) where

import EH101.Base.Common
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import Data.Maybe
import qualified EH.Util.FastSeq as Seq









cmodTrfLetFlattenStrict :: CModule -> CModule
cmodTrfLetFlattenStrict cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod)) Inh_CodeAGItf
     in   cTrf_Syn_CodeAGItf t



type Lift   = (Seq.Seq CBind,CExpr)
type MbLift = Maybe Lift

-- CAlt --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local isStrictChain : _
            local cTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (case (False) of
     { _isStrictChain ->
     (case (_isStrictChain) of
      { _exprOisStrictChain ->
      (case (expr_ _exprOisStrictChain ) of
       { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
           (case (pat_ ) of
            { ( _patIcTrf) ->
                (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                 { _cTrf ->
                 (case (_cTrf) of
                  { _lhsOcTrf ->
                  ( _lhsOcTrf) }) }) }) }) }) })
-- CAltL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIcTrf) ->
         (case (hd_ ) of
          { ( _hdIcTrf) ->
              (case ((:) _hdIcTrf _tlIcTrf) of
               { _cTrf ->
               (case (_cTrf) of
                { _lhsOcTrf ->
                ( _lhsOcTrf) }) }) }) })
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (case ([]) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CBind -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         isStrictChain        : Bool
      synthesized attributes:
         binds                : Seq.Seq CBind
         cTrf                 : SELF 
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = Bool ->
                ( (Seq.Seq CBind),CBind ,Bool)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _bindAspectsOisStrictChain ->
          (case (bindAspects_ _bindAspectsOisStrictChain ) of
           { ( _bindAspectsIaspLiftL,_bindAspectsIcTrf,_bindAspectsIhasNonPlainBinds,_bindAspectsIisStrictChain) ->
               (case (CBind_Bind nm_ _bindAspectsIcTrf) of
                { _cTrf ->
                (case (case _bindAspectsIaspLiftL of
                         [(flatBinds,flatExpr)] | not _bindAspectsIhasNonPlainBinds
                           -> flatBinds `Seq.union` Seq.singleton (acoreBind1Cat CBindCateg_Strict nm_ flatExpr)
                         _ -> Seq.singleton _cTrf) of
                 { _lhsObinds ->
                 (case (_cTrf) of
                  { _lhsOcTrf ->
                  (case (_bindAspectsIisStrictChain) of
                   { _lhsOisStrictChain ->
                   ( _lhsObinds,_lhsOcTrf,_lhsOisStrictChain) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = ( CBindAnn )
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (case (CBindAnn_Coe coe_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = ( CBindAnnL )
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIcTrf) ->
         (case (hd_ ) of
          { ( _hdIcTrf) ->
              (case ((:) _hdIcTrf _tlIcTrf) of
               { _cTrf ->
               (case (_cTrf) of
                { _lhsOcTrf ->
                ( _lhsOcTrf) }) }) }) })
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (case ([]) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CBindL ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         isStrictChain        : Bool
      synthesized attributes:
         binds                : Seq.Seq CBind
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = Bool ->
                 ( (Seq.Seq CBind),CBindL ,Bool)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _hdOisStrictChain ->
          (case (hd_ _hdOisStrictChain ) of
           { ( _hdIbinds,_hdIcTrf,_hdIisStrictChain) ->
               (case (_hdIisStrictChain) of
                { _tlOisStrictChain ->
                (case (tl_ _tlOisStrictChain ) of
                 { ( _tlIbinds,_tlIcTrf,_tlIisStrictChain) ->
                     (case (_hdIbinds `Seq.union` _tlIbinds) of
                      { _lhsObinds ->
                      (case ((:) _hdIcTrf _tlIcTrf) of
                       { _cTrf ->
                       (case (_cTrf) of
                        { _lhsOcTrf ->
                        (case (_tlIisStrictChain) of
                         { _lhsOisStrictChain ->
                         ( _lhsObinds,_lhsOcTrf,_lhsOisStrictChain) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIisStrictChain ->
         (case (Seq.empty) of
          { _lhsObinds ->
          (case ([]) of
           { _cTrf ->
           (case (_cTrf) of
            { _lhsOcTrf ->
            (case (_lhsIisStrictChain) of
             { _lhsOisStrictChain ->
             ( _lhsObinds,_lhsOcTrf,_lhsOisStrictChain) }) }) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         isStrictChain        : Bool
      synthesized attributes:
         aspLiftL             : [Lift]
         cTrf                 : SELF 
         hasNonPlainBinds     : Bool
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local cTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBound :: CBound  ->
              T_CBound 
sem_CBound (CBound_Bind _bindMeta _expr )  =
    (sem_CBound_Bind (sem_CMetas _bindMeta ) (sem_CExpr _expr ) )
sem_CBound (CBound_FFE _callconv _expEnt _expr _ty )  =
    (sem_CBound_FFE _callconv _expEnt (sem_CExpr _expr ) _ty )
sem_CBound (CBound_Meta _aspectKeyS _cmetas )  =
    (sem_CBound_Meta _aspectKeyS (sem_CMetas _cmetas ) )
sem_CBound (CBound_RelevTy _aspectKeyS _relevTy )  =
    (sem_CBound_RelevTy _aspectKeyS _relevTy )
sem_CBound (CBound_Ty _aspectKeyS _ty )  =
    (sem_CBound_Ty _aspectKeyS _ty )
sem_CBound (CBound_Val _aspectKeyS _expr )  =
    (sem_CBound_Val _aspectKeyS (sem_CExpr _expr ) )
-- semantic domain
type T_CBound  = Bool ->
                 ( ([Lift]),CBound ,Bool,Bool)
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (maybe [] (:[]) _exprImbLift) of
                { _lhsOaspLiftL ->
                (case (bindMeta_ ) of
                 { ( _bindMetaIcTrf) ->
                     (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                      { _cTrf ->
                      (case (_cTrf) of
                       { _lhsOcTrf ->
                       (case (False) of
                        { _lhsOhasNonPlainBinds ->
                        (case (_exprIisStrictChain) of
                         { _lhsOisStrictChain ->
                         ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case (_lhsIisStrictChain) of
           { _exprOisStrictChain ->
           (case (expr_ _exprOisStrictChain ) of
            { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
                (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                 { _cTrf ->
                 (case (_cTrf) of
                  { _lhsOcTrf ->
                  (case (True) of
                   { _lhsOhasNonPlainBinds ->
                   (case (_exprIisStrictChain) of
                    { _lhsOisStrictChain ->
                    ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case (cmetas_ ) of
           { ( _cmetasIcTrf) ->
               (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                { _cTrf ->
                (case (_cTrf) of
                 { _lhsOcTrf ->
                 (case (True) of
                  { _lhsOhasNonPlainBinds ->
                  (case (_lhsIisStrictChain) of
                   { _lhsOisStrictChain ->
                   ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case (CBound_RelevTy aspectKeyS_ relevTy_) of
           { _cTrf ->
           (case (_cTrf) of
            { _lhsOcTrf ->
            (case (True) of
             { _lhsOhasNonPlainBinds ->
             (case (_lhsIisStrictChain) of
              { _lhsOisStrictChain ->
              ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case (CBound_Ty aspectKeyS_ ty_) of
           { _cTrf ->
           (case (_cTrf) of
            { _lhsOcTrf ->
            (case (True) of
             { _lhsOhasNonPlainBinds ->
             (case (_lhsIisStrictChain) of
              { _lhsOisStrictChain ->
              ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case (_lhsIisStrictChain) of
           { _exprOisStrictChain ->
           (case (expr_ _exprOisStrictChain ) of
            { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
                (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                 { _cTrf ->
                 (case (_cTrf) of
                  { _lhsOcTrf ->
                  (case (True) of
                   { _lhsOhasNonPlainBinds ->
                   (case (_exprIisStrictChain) of
                    { _lhsOisStrictChain ->
                    ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         isStrictChain        : Bool
      synthesized attributes:
         aspLiftL             : [Lift]
         cTrf                 : SELF 
         hasNonPlainBinds     : Bool
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = Bool ->
                  ( ([Lift]),CBoundL ,Bool,Bool)
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _hdOisStrictChain ->
          (case (hd_ _hdOisStrictChain ) of
           { ( _hdIaspLiftL,_hdIcTrf,_hdIhasNonPlainBinds,_hdIisStrictChain) ->
               (case (_hdIisStrictChain) of
                { _tlOisStrictChain ->
                (case (tl_ _tlOisStrictChain ) of
                 { ( _tlIaspLiftL,_tlIcTrf,_tlIhasNonPlainBinds,_tlIisStrictChain) ->
                     (case (_hdIaspLiftL ++ _tlIaspLiftL) of
                      { _lhsOaspLiftL ->
                      (case ((:) _hdIcTrf _tlIcTrf) of
                       { _cTrf ->
                       (case (_cTrf) of
                        { _lhsOcTrf ->
                        (case (_hdIhasNonPlainBinds || _tlIhasNonPlainBinds) of
                         { _lhsOhasNonPlainBinds ->
                         (case (_tlIisStrictChain) of
                          { _lhsOisStrictChain ->
                          ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIisStrictChain ->
         (case ([]) of
          { _lhsOaspLiftL ->
          (case ([]) of
           { _cTrf ->
           (case (_cTrf) of
            { _lhsOcTrf ->
            (case (False) of
             { _lhsOhasNonPlainBinds ->
             (case (_lhsIisStrictChain) of
              { _lhsOisStrictChain ->
              ( _lhsOaspLiftL,_lhsOcTrf,_lhsOhasNonPlainBinds,_lhsOisStrictChain) }) }) }) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         isStrictChain        : Bool
      synthesized attributes:
         binds                : Seq.Seq CBind
         cTrf                 : SELF 
         flatExpr             : CExpr 
         mbLift               : MbLift
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
            local _tup1       : {(Seq.Seq CBind,CExpr)}
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local cTrf        : _
            local _tup2       : {(Seq.Seq CBind,CExpr)}
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local cTrf        : _
            local _tup3       : {(Seq.Seq CBind,CExpr)}
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local cTrf        : _
            local _tup4       : {(Seq.Seq CBind,CExpr)}
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
            local _tup5       : {(Seq.Seq CBind,CExpr)}
      alternative CoeArg:
         visit 0:
            local cTrf        : _
            local _tup6       : {(Seq.Seq CBind,CExpr)}
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
            local _tup7       : {(Seq.Seq CBind,CExpr)}
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local cTrf        : _
            local _tup8       : {(Seq.Seq CBind,CExpr)}
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
            local _tup9       : {(Seq.Seq CBind,CExpr)}
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local cTrf        : _
            local _tup10      : {(Seq.Seq CBind,CExpr)}
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
            local _tup11      : {(Seq.Seq CBind,CExpr)}
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
            local _tup12      : {(Seq.Seq CBind,CExpr)}
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local cTrf        : _
            local _tup13      : {(Seq.Seq CBind,CExpr)}
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local cTrf        : _
            local _tup14      : {(Seq.Seq CBind,CExpr)}
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local isStrictChain : _
            local cTrf        : _
            local letbinds    : _
            local isLaterInChain : _
            local _tup15      : {(Seq.Seq CBind,CExpr)}
            local isFirstInChain : _
      alternative String:
         child str            : {String}
         visit 0:
            local cTrf        : _
            local _tup16      : {(Seq.Seq CBind,CExpr)}
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local cTrf        : _
            local _tup17      : {(Seq.Seq CBind,CExpr)}
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local cTrf        : _
            local _tup18      : {(Seq.Seq CBind,CExpr)}
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
            local _tup19      : {(Seq.Seq CBind,CExpr)}
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
            local _tup20      : {(Seq.Seq CBind,CExpr)}
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local cTrf        : _
            local _tup21      : {(Seq.Seq CBind,CExpr)}
-}
-- cata
sem_CExpr :: CExpr  ->
             T_CExpr 
sem_CExpr (CExpr_Ann _ann _expr )  =
    (sem_CExpr_Ann (sem_CExprAnn _ann ) (sem_CExpr _expr ) )
sem_CExpr (CExpr_App _func _arg )  =
    (sem_CExpr_App (sem_CExpr _func ) (sem_CBound _arg ) )
sem_CExpr (CExpr_Case _expr _alts _dflt )  =
    (sem_CExpr_Case (sem_CExpr _expr ) (sem_CAltL _alts ) (sem_CExpr _dflt ) )
sem_CExpr (CExpr_CaseAltFail _failReason _errorExpr )  =
    (sem_CExpr_CaseAltFail _failReason (sem_CExpr _errorExpr ) )
sem_CExpr (CExpr_Char _char )  =
    (sem_CExpr_Char _char )
sem_CExpr (CExpr_CoeArg )  =
    (sem_CExpr_CoeArg )
sem_CExpr (CExpr_FFI _callconv _safety _impEnt _ty )  =
    (sem_CExpr_FFI _callconv _safety _impEnt _ty )
sem_CExpr (CExpr_Hole _uid )  =
    (sem_CExpr_Hole _uid )
sem_CExpr (CExpr_HoleLet _bindsUid _body )  =
    (sem_CExpr_HoleLet _bindsUid (sem_CExpr _body ) )
sem_CExpr (CExpr_ImplsApp _func _uid )  =
    (sem_CExpr_ImplsApp (sem_CExpr _func ) _uid )
sem_CExpr (CExpr_ImplsLam _uid _body )  =
    (sem_CExpr_ImplsLam _uid (sem_CExpr _body ) )
sem_CExpr (CExpr_Int _int )  =
    (sem_CExpr_Int _int )
sem_CExpr (CExpr_Integer _integer )  =
    (sem_CExpr_Integer _integer )
sem_CExpr (CExpr_Lam _bind _body )  =
    (sem_CExpr_Lam (sem_CBind _bind ) (sem_CExpr _body ) )
sem_CExpr (CExpr_Let _categ _binds _body )  =
    (sem_CExpr_Let _categ (sem_CBindL _binds ) (sem_CExpr _body ) )
sem_CExpr (CExpr_String _str )  =
    (sem_CExpr_String _str )
sem_CExpr (CExpr_Tup _tag )  =
    (sem_CExpr_Tup _tag )
sem_CExpr (CExpr_TupDel _expr _tag _nm _offset )  =
    (sem_CExpr_TupDel (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) )
sem_CExpr (CExpr_TupIns _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupIns (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_TupUpd _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupUpd (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_Var _ref )  =
    (sem_CExpr_Var _ref )
-- semantic domain
type T_CExpr  = Bool ->
                ( (Seq.Seq CBind),CExpr ,CExpr ,Bool,MbLift)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (ann_ ) of
                { ( _annIcTrf) ->
                    (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                     { _cTrf ->
                     (case ((Seq.empty,_cTrf)) of
                      { __tup1 ->
                      (case (__tup1) of
                       { (_lhsObinds,_) ->
                       (case (_cTrf) of
                        { _lhsOcTrf ->
                        (case (__tup1) of
                         { (_,_lhsOflatExpr) ->
                         (case (_exprIisStrictChain) of
                          { _lhsOisStrictChain ->
                          (case (Nothing) of
                           { _lhsOmbLift ->
                           ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _funcOisStrictChain ->
          (case (func_ _funcOisStrictChain ) of
           { ( _funcIbinds,_funcIcTrf,_funcIflatExpr,_funcIisStrictChain,_funcImbLift) ->
               (case (_funcIisStrictChain) of
                { _argOisStrictChain ->
                (case (arg_ _argOisStrictChain ) of
                 { ( _argIaspLiftL,_argIcTrf,_argIhasNonPlainBinds,_argIisStrictChain) ->
                     (case (CExpr_App _funcIcTrf _argIcTrf) of
                      { _cTrf ->
                      (case ((Seq.empty,_cTrf)) of
                       { __tup2 ->
                       (case (__tup2) of
                        { (_lhsObinds,_) ->
                        (case (_cTrf) of
                         { _lhsOcTrf ->
                         (case (__tup2) of
                          { (_,_lhsOflatExpr) ->
                          (case (_lhsIisStrictChain) of
                           { _lhsOisStrictChain ->
                           (case (Nothing) of
                            { _lhsOmbLift ->
                            ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (_exprIisStrictChain) of
                { _dfltOisStrictChain ->
                (case (dflt_ _dfltOisStrictChain ) of
                 { ( _dfltIbinds,_dfltIcTrf,_dfltIflatExpr,_dfltIisStrictChain,_dfltImbLift) ->
                     (case (alts_ ) of
                      { ( _altsIcTrf) ->
                          (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                           { _cTrf ->
                           (case ((Seq.empty,_cTrf)) of
                            { __tup3 ->
                            (case (__tup3) of
                             { (_lhsObinds,_) ->
                             (case (_cTrf) of
                              { _lhsOcTrf ->
                              (case (__tup3) of
                               { (_,_lhsOflatExpr) ->
                               (case (_lhsIisStrictChain) of
                                { _lhsOisStrictChain ->
                                (case (Nothing) of
                                 { _lhsOmbLift ->
                                 ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _errorExprOisStrictChain ->
          (case (errorExpr_ _errorExprOisStrictChain ) of
           { ( _errorExprIbinds,_errorExprIcTrf,_errorExprIflatExpr,_errorExprIisStrictChain,_errorExprImbLift) ->
               (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                { _cTrf ->
                (case ((Seq.empty,_cTrf)) of
                 { __tup4 ->
                 (case (__tup4) of
                  { (_lhsObinds,_) ->
                  (case (_cTrf) of
                   { _lhsOcTrf ->
                   (case (__tup4) of
                    { (_,_lhsOflatExpr) ->
                    (case (_lhsIisStrictChain) of
                     { _lhsOisStrictChain ->
                     (case (Nothing) of
                      { _lhsOmbLift ->
                      ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Char char_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup5 ->
           (case (__tup5) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup5) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_CoeArg) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup6 ->
           (case (__tup6) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup6) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup7 ->
           (case (__tup7) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup7) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Hole uid_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup8 ->
           (case (__tup8) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup8) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _bodyOisStrictChain ->
          (case (body_ _bodyOisStrictChain ) of
           { ( _bodyIbinds,_bodyIcTrf,_bodyIflatExpr,_bodyIisStrictChain,_bodyImbLift) ->
               (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                { _cTrf ->
                (case ((Seq.empty,_cTrf)) of
                 { __tup9 ->
                 (case (__tup9) of
                  { (_lhsObinds,_) ->
                  (case (_cTrf) of
                   { _lhsOcTrf ->
                   (case (__tup9) of
                    { (_,_lhsOflatExpr) ->
                    (case (_lhsIisStrictChain) of
                     { _lhsOisStrictChain ->
                     (case (Nothing) of
                      { _lhsOmbLift ->
                      ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _funcOisStrictChain ->
          (case (func_ _funcOisStrictChain ) of
           { ( _funcIbinds,_funcIcTrf,_funcIflatExpr,_funcIisStrictChain,_funcImbLift) ->
               (case (CExpr_ImplsApp _funcIcTrf uid_) of
                { _cTrf ->
                (case ((Seq.empty,_cTrf)) of
                 { __tup10 ->
                 (case (__tup10) of
                  { (_lhsObinds,_) ->
                  (case (_cTrf) of
                   { _lhsOcTrf ->
                   (case (__tup10) of
                    { (_,_lhsOflatExpr) ->
                    (case (_lhsIisStrictChain) of
                     { _lhsOisStrictChain ->
                     (case (Nothing) of
                      { _lhsOmbLift ->
                      ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _bodyOisStrictChain ->
          (case (body_ _bodyOisStrictChain ) of
           { ( _bodyIbinds,_bodyIcTrf,_bodyIflatExpr,_bodyIisStrictChain,_bodyImbLift) ->
               (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                { _cTrf ->
                (case ((Seq.empty,_cTrf)) of
                 { __tup11 ->
                 (case (__tup11) of
                  { (_lhsObinds,_) ->
                  (case (_cTrf) of
                   { _lhsOcTrf ->
                   (case (__tup11) of
                    { (_,_lhsOflatExpr) ->
                    (case (_lhsIisStrictChain) of
                     { _lhsOisStrictChain ->
                     (case (Nothing) of
                      { _lhsOmbLift ->
                      ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Int int_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup12 ->
           (case (__tup12) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup12) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Integer integer_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup13 ->
           (case (__tup13) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup13) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _bindOisStrictChain ->
          (case (bind_ _bindOisStrictChain ) of
           { ( _bindIbinds,_bindIcTrf,_bindIisStrictChain) ->
               (case (_bindIisStrictChain) of
                { _bodyOisStrictChain ->
                (case (body_ _bodyOisStrictChain ) of
                 { ( _bodyIbinds,_bodyIcTrf,_bodyIflatExpr,_bodyIisStrictChain,_bodyImbLift) ->
                     (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                      { _cTrf ->
                      (case ((Seq.empty,_cTrf)) of
                       { __tup14 ->
                       (case (__tup14) of
                        { (_lhsObinds,_) ->
                        (case (_cTrf) of
                         { _lhsOcTrf ->
                         (case (__tup14) of
                          { (_,_lhsOflatExpr) ->
                          (case (_lhsIisStrictChain) of
                           { _lhsOisStrictChain ->
                           (case (Nothing) of
                            { _lhsOmbLift ->
                            ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIisStrictChain ->
         (case (categ_ == CBindCateg_Strict) of
          { _isStrictChain ->
          (case (_isStrictChain) of
           { _bindsOisStrictChain ->
           (case (binds_ _bindsOisStrictChain ) of
            { ( _bindsIbinds,_bindsIcTrf,_bindsIisStrictChain) ->
                (case (_isStrictChain && _bindsIisStrictChain) of
                 { _bodyOisStrictChain ->
                 (case (body_ _bodyOisStrictChain ) of
                  { ( _bodyIbinds,_bodyIcTrf,_bodyIflatExpr,_bodyIisStrictChain,_bodyImbLift) ->
                      (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                       { _cTrf ->
                       (case (_bindsIbinds `Seq.union` _bodyIbinds) of
                        { _letbinds ->
                        (case (_isStrictChain &&     _lhsIisStrictChain) of
                         { _isLaterInChain ->
                         (case (if _isLaterInChain
                                then (_letbinds,_bodyIflatExpr)
                                else (Seq.empty,_cTrf)) of
                          { __tup15 ->
                          (case (__tup15) of
                           { (_lhsObinds,_) ->
                           (case (_isStrictChain && not _lhsIisStrictChain) of
                            { _isFirstInChain ->
                            (case (if _isFirstInChain
                                   then foldr (\b e -> CExpr_Let categ_ [b] e) _bodyIflatExpr $ Seq.toList _letbinds
                                   else _cTrf) of
                             { _lhsOcTrf ->
                             (case (__tup15) of
                              { (_,_lhsOflatExpr) ->
                              (case (_isStrictChain && _bodyIisStrictChain) of
                               { _lhsOisStrictChain ->
                               (case (if _isLaterInChain
                                      then Just (_letbinds, _bodyIflatExpr)
                                      else Nothing) of
                                { _lhsOmbLift ->
                                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_String str_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup16 ->
           (case (__tup16) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup16) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Tup tag_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup17 ->
           (case (__tup17) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup17) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (_exprIisStrictChain) of
                { _offsetOisStrictChain ->
                (case (offset_ _offsetOisStrictChain ) of
                 { ( _offsetIbinds,_offsetIcTrf,_offsetIflatExpr,_offsetIisStrictChain,_offsetImbLift) ->
                     (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                      { _cTrf ->
                      (case ((Seq.empty,_cTrf)) of
                       { __tup18 ->
                       (case (__tup18) of
                        { (_lhsObinds,_) ->
                        (case (_cTrf) of
                         { _lhsOcTrf ->
                         (case (__tup18) of
                          { (_,_lhsOflatExpr) ->
                          (case (_lhsIisStrictChain) of
                           { _lhsOisStrictChain ->
                           (case (Nothing) of
                            { _lhsOmbLift ->
                            ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (_exprIisStrictChain) of
                { _offsetOisStrictChain ->
                (case (offset_ _offsetOisStrictChain ) of
                 { ( _offsetIbinds,_offsetIcTrf,_offsetIflatExpr,_offsetIisStrictChain,_offsetImbLift) ->
                     (case (_offsetIisStrictChain) of
                      { _fldExprOisStrictChain ->
                      (case (fldExpr_ _fldExprOisStrictChain ) of
                       { ( _fldExprIbinds,_fldExprIcTrf,_fldExprIflatExpr,_fldExprIisStrictChain,_fldExprImbLift) ->
                           (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                            { _cTrf ->
                            (case ((Seq.empty,_cTrf)) of
                             { __tup19 ->
                             (case (__tup19) of
                              { (_lhsObinds,_) ->
                              (case (_cTrf) of
                               { _lhsOcTrf ->
                               (case (__tup19) of
                                { (_,_lhsOflatExpr) ->
                                (case (_lhsIisStrictChain) of
                                 { _lhsOisStrictChain ->
                                 (case (Nothing) of
                                  { _lhsOmbLift ->
                                  ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIisStrictChain ->
         (case (_lhsIisStrictChain) of
          { _exprOisStrictChain ->
          (case (expr_ _exprOisStrictChain ) of
           { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
               (case (_exprIisStrictChain) of
                { _offsetOisStrictChain ->
                (case (offset_ _offsetOisStrictChain ) of
                 { ( _offsetIbinds,_offsetIcTrf,_offsetIflatExpr,_offsetIisStrictChain,_offsetImbLift) ->
                     (case (_offsetIisStrictChain) of
                      { _fldExprOisStrictChain ->
                      (case (fldExpr_ _fldExprOisStrictChain ) of
                       { ( _fldExprIbinds,_fldExprIcTrf,_fldExprIflatExpr,_fldExprIisStrictChain,_fldExprImbLift) ->
                           (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                            { _cTrf ->
                            (case ((Seq.empty,_cTrf)) of
                             { __tup20 ->
                             (case (__tup20) of
                              { (_lhsObinds,_) ->
                              (case (_cTrf) of
                               { _lhsOcTrf ->
                               (case (__tup20) of
                                { (_,_lhsOflatExpr) ->
                                (case (_lhsIisStrictChain) of
                                 { _lhsOisStrictChain ->
                                 (case (Nothing) of
                                  { _lhsOmbLift ->
                                  ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIisStrictChain ->
         (case (CExpr_Var ref_) of
          { _cTrf ->
          (case ((Seq.empty,_cTrf)) of
           { __tup21 ->
           (case (__tup21) of
            { (_lhsObinds,_) ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             (case (__tup21) of
              { (_,_lhsOflatExpr) ->
              (case (_lhsIisStrictChain) of
               { _lhsOisStrictChain ->
               (case (Nothing) of
                { _lhsOmbLift ->
                ( _lhsObinds,_lhsOcTrf,_lhsOflatExpr,_lhsOisStrictChain,_lhsOmbLift) }) }) }) }) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CExprAnn :: CExprAnn  ->
                T_CExprAnn 
sem_CExprAnn (CExprAnn_Coe _coe )  =
    (sem_CExprAnn_Coe _coe )
sem_CExprAnn (CExprAnn_Debug _info )  =
    (sem_CExprAnn_Debug _info )
sem_CExprAnn (CExprAnn_Ty _ty )  =
    (sem_CExprAnn_Ty _ty )
-- semantic domain
type T_CExprAnn  = ( CExprAnn )
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (case (CExprAnn_Coe coe_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (case (CExprAnn_Debug info_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (case (CExprAnn_Ty ty_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Apply0:
         visit 0:
            local cTrf        : _
      alternative Function0:
         visit 0:
            local cTrf        : _
      alternative Function1:
         visit 0:
            local cTrf        : _
      alternative Plain:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CMetaBind :: CMetaBind  ->
                 T_CMetaBind 
sem_CMetaBind (CMetaBind_Apply0 )  =
    (sem_CMetaBind_Apply0 )
sem_CMetaBind (CMetaBind_Function0 )  =
    (sem_CMetaBind_Function0 )
sem_CMetaBind (CMetaBind_Function1 )  =
    (sem_CMetaBind_Function1 )
sem_CMetaBind (CMetaBind_Plain )  =
    (sem_CMetaBind_Plain )
-- semantic domain
type T_CMetaBind  = ( CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (case (CMetaBind_Apply0) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (case (CMetaBind_Function0) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (case (CMetaBind_Function1) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (case (CMetaBind_Plain) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Dict:
         visit 0:
            local cTrf        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local cTrf        : _
      alternative Val:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CMetaVal :: CMetaVal  ->
                T_CMetaVal 
sem_CMetaVal (CMetaVal_Dict )  =
    (sem_CMetaVal_Dict )
sem_CMetaVal (CMetaVal_DictClass _tracks )  =
    (sem_CMetaVal_DictClass _tracks )
sem_CMetaVal (CMetaVal_DictInstance _tracks )  =
    (sem_CMetaVal_DictInstance _tracks )
sem_CMetaVal (CMetaVal_Track _track )  =
    (sem_CMetaVal_Track _track )
sem_CMetaVal (CMetaVal_Val )  =
    (sem_CMetaVal_Val )
-- semantic domain
type T_CMetaVal  = ( CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (case (CMetaVal_Dict) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (case (CMetaVal_DictClass tracks_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (case (CMetaVal_DictInstance tracks_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (case (CMetaVal_Track track_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (case (CMetaVal_Val) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CMetas ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = ( CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (case (x2_ ) of
     { ( _x2IcTrf) ->
         (case (x1_ ) of
          { ( _x1IcTrf) ->
              (case ((_x1IcTrf,_x2IcTrf)) of
               { _cTrf ->
               (case (_cTrf) of
                { _lhsOcTrf ->
                ( _lhsOcTrf) }) }) }) })
-- CModule -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local isStrictChain : _
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ( CModule )
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (case (False) of
     { _isStrictChain ->
     (case (_isStrictChain) of
      { _exprOisStrictChain ->
      (case (expr_ _exprOisStrictChain ) of
       { ( _exprIbinds,_exprIcTrf,_exprIflatExpr,_exprIisStrictChain,_exprImbLift) ->
           (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
            { _cTrf ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             ( _lhsOcTrf) }) }) }) }) })
-- CPat --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPat :: CPat  ->
            T_CPat 
sem_CPat (CPat_BoolExpr _cexpr )  =
    (sem_CPat_BoolExpr _cexpr )
sem_CPat (CPat_Char _char )  =
    (sem_CPat_Char _char )
sem_CPat (CPat_Con _tag _rest _binds )  =
    (sem_CPat_Con _tag (sem_CPatRest _rest ) (sem_CPatFldL _binds ) )
sem_CPat (CPat_Int _int )  =
    (sem_CPat_Int _int )
sem_CPat (CPat_Var _pnm )  =
    (sem_CPat_Var _pnm )
-- semantic domain
type T_CPat  = ( CPat )
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (case (CPat_BoolExpr cexpr_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (case (CPat_Char char_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (case (binds_ ) of
     { ( _bindsIcTrf) ->
         (case (rest_ ) of
          { ( _restIcTrf) ->
              (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
               { _cTrf ->
               (case (_cTrf) of
                { _lhsOcTrf ->
                ( _lhsOcTrf) }) }) }) })
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (case (CPat_Int int_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (case (CPat_Var pnm_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local isStrictChain : _
            local cTrf        : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = ( CPatFld )
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (case (False) of
     { _isStrictChain ->
     (case (_isStrictChain) of
      { _bindOisStrictChain ->
      (case (_isStrictChain) of
       { _offsetOisStrictChain ->
       (case (fldAnns_ ) of
        { ( _fldAnnsIcTrf) ->
            (case (bind_ _bindOisStrictChain ) of
             { ( _bindIbinds,_bindIcTrf,_bindIisStrictChain) ->
                 (case (offset_ _offsetOisStrictChain ) of
                  { ( _offsetIbinds,_offsetIcTrf,_offsetIflatExpr,_offsetIisStrictChain,_offsetImbLift) ->
                      (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                       { _cTrf ->
                       (case (_cTrf) of
                        { _lhsOcTrf ->
                        ( _lhsOcTrf) }) }) }) }) }) }) }) })
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = ( CPatFldL )
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIcTrf) ->
         (case (hd_ ) of
          { ( _hdIcTrf) ->
              (case ((:) _hdIcTrf _tlIcTrf) of
               { _cTrf ->
               (case (_cTrf) of
                { _lhsOcTrf ->
                ( _lhsOcTrf) }) }) }) })
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (case ([]) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local cTrf        : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = ( CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (case (CPatRest_Empty) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (case (CPatRest_Var nm_) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : CModule 
   alternatives:
      alternative AGItf:
         child module         : CModule 
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf )  =
    (let ( _lhsOcTrf) = sem 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleIcTrf) ->
         (case (_moduleIcTrf) of
          { _lhsOcTrf ->
          ( _lhsOcTrf) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local isStrictChain : _
            local cTrf        : _
      alternative Nothing:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = ( MbCExpr )
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (case (False) of
     { _isStrictChain ->
     (case (_isStrictChain) of
      { _justOisStrictChain ->
      (case (just_ _justOisStrictChain ) of
       { ( _justIbinds,_justIcTrf,_justIflatExpr,_justIisStrictChain,_justImbLift) ->
           (case (Just _justIcTrf) of
            { _cTrf ->
            (case (_cTrf) of
             { _lhsOcTrf ->
             ( _lhsOcTrf) }) }) }) }) })
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (case (Nothing) of
     { _cTrf ->
     (case (_cTrf) of
      { _lhsOcTrf ->
      ( _lhsOcTrf) }) })
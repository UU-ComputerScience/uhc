

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/ModAsMap.ag)
module EH101.Core.ModAsMap(cexprModAsDatabase) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array
import qualified EH.Util.FastSeq as Seq
import EH101.Base.Common
import EH101.Core
import EH101.Ty











cexprModAsDatabase :: CModule -> CModuleDatabase
cexprModAsDatabase m
  = db_Syn_CodeAGItf t
  where t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf m)) Inh_CodeAGItf

-- CAlt --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local copy        : _
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
    (case (expr_ ) of
     { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
         (case (pat_ ) of
          { ( _patIcopy) ->
              (case (CAlt_Alt _patIcopy _exprIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
-- CAltL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
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
     { ( _tlIcopy) ->
         (case (hd_ ) of
          { ( _hdIcopy) ->
              (case ((:) _hdIcopy _tlIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (case ([]) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CBind -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local copy        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = ( CBind )
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (case (bindAspects_ ) of
     { ( _bindAspectsIcopy) ->
         (case (CBind_Bind nm_ _bindAspectsIcopy) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           ( _lhsOcopy) }) }) })
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
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
     { ( _tlIcopy) ->
         (case (hd_ ) of
          { ( _hdIcopy) ->
              (case ((:) _hdIcopy _tlIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (case ([]) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CBindL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ( CBindL )
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIcopy) ->
         (case (hd_ ) of
          { ( _hdIcopy) ->
              (case ((:) _hdIcopy _tlIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (case ([]) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CBound ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local copy        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local copy        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local copy        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local copy        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local copy        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local copy        : _
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
type T_CBound  = ( CBound )
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (case (expr_ ) of
     { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
         (case (bindMeta_ ) of
          { ( _bindMetaIcopy) ->
              (case (CBound_Bind _bindMetaIcopy _exprIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (case (expr_ ) of
     { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
         (case (CBound_FFE callconv_ expEnt_ _exprIcopy ty_) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           ( _lhsOcopy) }) }) })
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (case (cmetas_ ) of
     { ( _cmetasIcopy) ->
         (case (CBound_Meta aspectKeyS_ _cmetasIcopy) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           ( _lhsOcopy) }) }) })
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (case (CBound_RelevTy aspectKeyS_ relevTy_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (case (CBound_Ty aspectKeyS_ ty_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (case (expr_ ) of
     { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
         (case (CBound_Val aspectKeyS_ _exprIcopy) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           ( _lhsOcopy) }) }) })
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = ( CBoundL )
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIcopy) ->
         (case (hd_ ) of
          { ( _hdIcopy) ->
              (case ((:) _hdIcopy _tlIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (case ([]) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         bindSq               : Seq.Seq CDbBindLetInfo
         copy                 : SELF 
         expr                 : CExpr 
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local copy        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local copy        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local copy        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local copy        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local copy        : _
      alternative CoeArg:
         visit 0:
            local copy        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local copy        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local copy        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local copy        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local copy        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local copy        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local copy        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local copy        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local copy        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local copy        : _
      alternative String:
         child str            : {String}
         visit 0:
            local copy        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local copy        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local copy        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local copy        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local copy        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local copy        : _
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
type T_CExpr  = ( (Seq.Seq CDbBindLetInfo),CExpr ,CExpr )
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (expr_ ) of
      { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
          (case (ann_ ) of
           { ( _annIcopy) ->
               (case (CExpr_Ann _annIcopy _exprIcopy) of
                { _copy ->
                (case (_copy) of
                 { _lhsOcopy ->
                 (case (_copy) of
                  { _lhsOexpr ->
                  ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) })
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (arg_ ) of
      { ( _argIcopy) ->
          (case (func_ ) of
           { ( _funcIbindSq,_funcIcopy,_funcIexpr) ->
               (case (CExpr_App _funcIcopy _argIcopy) of
                { _copy ->
                (case (_copy) of
                 { _lhsOcopy ->
                 (case (_copy) of
                  { _lhsOexpr ->
                  ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) })
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (dflt_ ) of
      { ( _dfltIbindSq,_dfltIcopy,_dfltIexpr) ->
          (case (alts_ ) of
           { ( _altsIcopy) ->
               (case (expr_ ) of
                { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
                    (case (CExpr_Case _exprIcopy _altsIcopy _dfltIcopy) of
                     { _copy ->
                     (case (_copy) of
                      { _lhsOcopy ->
                      (case (_copy) of
                       { _lhsOexpr ->
                       ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) }) })
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (errorExpr_ ) of
      { ( _errorExprIbindSq,_errorExprIcopy,_errorExprIexpr) ->
          (case (CExpr_CaseAltFail failReason_ _errorExprIcopy) of
           { _copy ->
           (case (_copy) of
            { _lhsOcopy ->
            (case (_copy) of
             { _lhsOexpr ->
             ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) })
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Char char_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_CoeArg) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Hole uid_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (body_ ) of
      { ( _bodyIbindSq,_bodyIcopy,_bodyIexpr) ->
          (case (CExpr_HoleLet bindsUid_ _bodyIcopy) of
           { _copy ->
           (case (_copy) of
            { _lhsOcopy ->
            (case (_copy) of
             { _lhsOexpr ->
             ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) })
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (func_ ) of
      { ( _funcIbindSq,_funcIcopy,_funcIexpr) ->
          (case (CExpr_ImplsApp _funcIcopy uid_) of
           { _copy ->
           (case (_copy) of
            { _lhsOcopy ->
            (case (_copy) of
             { _lhsOexpr ->
             ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) })
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (body_ ) of
      { ( _bodyIbindSq,_bodyIcopy,_bodyIexpr) ->
          (case (CExpr_ImplsLam uid_ _bodyIcopy) of
           { _copy ->
           (case (_copy) of
            { _lhsOcopy ->
            (case (_copy) of
             { _lhsOexpr ->
             ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) })
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Int int_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Integer integer_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (body_ ) of
      { ( _bodyIbindSq,_bodyIcopy,_bodyIexpr) ->
          (case (bind_ ) of
           { ( _bindIcopy) ->
               (case (CExpr_Lam _bindIcopy _bodyIcopy) of
                { _copy ->
                (case (_copy) of
                 { _lhsOcopy ->
                 (case (_copy) of
                  { _lhsOexpr ->
                  ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) })
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (case (body_ ) of
     { ( _bodyIbindSq,_bodyIcopy,_bodyIexpr) ->
         (case (binds_ ) of
          { ( _bindsIcopy) ->
              (case (Seq.singleton (categ_,_bindsIcopy) `Seq.union` _bodyIbindSq) of
               { _lhsObindSq ->
               (case (CExpr_Let categ_ _bindsIcopy _bodyIcopy) of
                { _copy ->
                (case (_copy) of
                 { _lhsOcopy ->
                 (case (_bodyIexpr) of
                  { _lhsOexpr ->
                  ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) })
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_String str_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Tup tag_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (offset_ ) of
      { ( _offsetIbindSq,_offsetIcopy,_offsetIexpr) ->
          (case (expr_ ) of
           { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
               (case (CExpr_TupDel _exprIcopy tag_ nm_ _offsetIcopy) of
                { _copy ->
                (case (_copy) of
                 { _lhsOcopy ->
                 (case (_copy) of
                  { _lhsOexpr ->
                  ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) })
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (fldExpr_ ) of
      { ( _fldExprIbindSq,_fldExprIcopy,_fldExprIexpr) ->
          (case (offset_ ) of
           { ( _offsetIbindSq,_offsetIcopy,_offsetIexpr) ->
               (case (expr_ ) of
                { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
                    (case (CExpr_TupIns _exprIcopy tag_ nm_ _offsetIcopy _fldExprIcopy) of
                     { _copy ->
                     (case (_copy) of
                      { _lhsOcopy ->
                      (case (_copy) of
                       { _lhsOexpr ->
                       ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) }) })
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (fldExpr_ ) of
      { ( _fldExprIbindSq,_fldExprIcopy,_fldExprIexpr) ->
          (case (offset_ ) of
           { ( _offsetIbindSq,_offsetIcopy,_offsetIexpr) ->
               (case (expr_ ) of
                { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
                    (case (CExpr_TupUpd _exprIcopy tag_ nm_ _offsetIcopy _fldExprIcopy) of
                     { _copy ->
                     (case (_copy) of
                      { _lhsOcopy ->
                      (case (_copy) of
                       { _lhsOexpr ->
                       ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) }) }) }) })
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (case (Seq.empty) of
     { _lhsObindSq ->
     (case (CExpr_Var ref_) of
      { _copy ->
      (case (_copy) of
       { _lhsOcopy ->
       (case (_copy) of
        { _lhsOexpr ->
        ( _lhsObindSq,_lhsOcopy,_lhsOexpr) }) }) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local copy        : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local copy        : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (case (CExprAnn_Debug info_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (case (CExprAnn_Ty ty_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Apply0:
         visit 0:
            local copy        : _
      alternative Function0:
         visit 0:
            local copy        : _
      alternative Function1:
         visit 0:
            local copy        : _
      alternative Plain:
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (case (CMetaBind_Function0) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (case (CMetaBind_Function1) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (case (CMetaBind_Plain) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Dict:
         visit 0:
            local copy        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local copy        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local copy        : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local copy        : _
      alternative Val:
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (case (CMetaVal_DictClass tracks_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (case (CMetaVal_DictInstance tracks_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (case (CMetaVal_Track track_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (case (CMetaVal_Val) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CMetas ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local copy        : _
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
     { ( _x2Icopy) ->
         (case (x1_ ) of
          { ( _x1Icopy) ->
              (case ((_x1Icopy,_x2Icopy)) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
-- CModule -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         db                   : CModuleDatabase
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local copy        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ( CModule ,CModuleDatabase)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (case (expr_ ) of
     { ( _exprIbindSq,_exprIcopy,_exprIexpr) ->
         (case (CModule_Mod moduleNm_ _exprIcopy ctagsMp_) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           (case (let binds = Seq.toList _exprIbindSq
                      nrBinds = length binds
                      (refs,arrs)
                            = unzip
                                [ (Map.unions refs, (cat, listArray (0, nrSubBinds-1) bs))
                                | (bi,(cat,bs)) <- zip [0 .. nrBinds-1] binds
                                , let nrSubBinds = length bs
                                , let refs = zipWith (\b i -> Map.singleton (cbindNm b) (bi,i)) bs [0 .. nrSubBinds-1]
                                ]
                  in  emptyCModuleDatabase
                          { cmoddbModNm       = moduleNm_
                          , cmoddbBindMp      = Map.unions refs
                          , cmoddbBindArr     = listArray (0, nrBinds-1) arrs
                          , cmoddbMainExpr    = _exprIexpr
                          , cmoddbTagsMp      = ctagsMp_
                          }) of
            { _lhsOdb ->
            ( _lhsOcopy,_lhsOdb) }) }) }) })
-- CPat --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local copy        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local copy        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local copy        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local copy        : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (case (CPat_Char char_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (case (binds_ ) of
     { ( _bindsIcopy) ->
         (case (rest_ ) of
          { ( _restIcopy) ->
              (case (CPat_Con tag_ _restIcopy _bindsIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (case (CPat_Int int_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (case (CPat_Var pnm_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local copy        : _
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
    (case (fldAnns_ ) of
     { ( _fldAnnsIcopy) ->
         (case (bind_ ) of
          { ( _bindIcopy) ->
              (case (offset_ ) of
               { ( _offsetIbindSq,_offsetIcopy,_offsetIexpr) ->
                   (case (CPatFld_Fld lbl_ _offsetIcopy _bindIcopy _fldAnnsIcopy) of
                    { _copy ->
                    (case (_copy) of
                     { _lhsOcopy ->
                     ( _lhsOcopy) }) }) }) }) })
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
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
     { ( _tlIcopy) ->
         (case (hd_ ) of
          { ( _hdIcopy) ->
              (case ((:) _hdIcopy _tlIcopy) of
               { _copy ->
               (case (_copy) of
                { _lhsOcopy ->
                ( _lhsOcopy) }) }) }) })
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (case ([]) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local copy        : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local copy        : _
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
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (case (CPatRest_Var nm_) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         db                   : CModuleDatabase
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
type T_CodeAGItf  = ( CModuleDatabase)
data Inh_CodeAGItf  = Inh_CodeAGItf {}
data Syn_CodeAGItf  = Syn_CodeAGItf {db_Syn_CodeAGItf :: !(CModuleDatabase)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf )  =
    (let ( _lhsOdb) = sem 
     in  (Syn_CodeAGItf _lhsOdb ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleIcopy,_moduleIdb) ->
         (case (_moduleIdb) of
          { _lhsOdb ->
          ( _lhsOdb) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         copy                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local copy        : _
      alternative Nothing:
         visit 0:
            local copy        : _
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
    (case (just_ ) of
     { ( _justIbindSq,_justIcopy,_justIexpr) ->
         (case (Just _justIcopy) of
          { _copy ->
          (case (_copy) of
           { _lhsOcopy ->
           ( _lhsOcopy) }) }) })
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (case (Nothing) of
     { _copy ->
     (case (_copy) of
      { _lhsOcopy ->
      ( _lhsOcopy) }) })


-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/UsedModNms.ag)
module EH101.Core.UsedModNms(cmodUsedModNms) where

import qualified Data.Set as Set
import Data.Maybe
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Core
import EH101.Ty







cmodUsedModNms :: CModule -> HsNameS
cmodUsedModNms m
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf m)) Inh_CodeAGItf
     in   usedModNmS_Syn_CodeAGItf t

-- CAlt --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = ( HsNameS)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (case (expr_ ) of
     { ( _exprIusedModNmS) ->
         (case (pat_ ) of
          { ( _patIfldNmL,_patIusedModNmS) ->
              (case (_patIusedModNmS `Set.union` _exprIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
-- CAltL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
      alternative Nil:
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = ( HsNameS)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIusedModNmS) ->
         (case (hd_ ) of
          { ( _hdIusedModNmS) ->
              (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CBind -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         nm                   : HsName
         usedModNmS           : HsNameS
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = ( HsName,HsNameS)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (case (nm_) of
     { _lhsOnm ->
     (case (nm_) of
      { _bindAspectsOnm ->
      (case (bindAspects_ _bindAspectsOnm ) of
       { ( _bindAspectsIusedModNmS) ->
           (case (_bindAspectsIusedModNmS) of
            { _lhsOusedModNmS ->
            ( _lhsOnm,_lhsOusedModNmS) }) }) }) })
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = ( HsNameS)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
      alternative Nil:
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = ( HsNameS)
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIusedModNmS) ->
         (case (hd_ ) of
          { ( _hdIusedModNmS) ->
              (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CBindL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
      alternative Nil:
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ( HsNameS)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIusedModNmS) ->
         (case (hd_ ) of
          { ( _hdInm,_hdIusedModNmS) ->
              (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nm                   : HsName
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
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
type T_CBound  = HsName ->
                 ( HsNameS)
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsInm ->
         (case (expr_ ) of
          { ( _exprIusedModNmS) ->
              (case (bindMeta_ ) of
               { ( _bindMetaIusedModNmS) ->
                   (case (_bindMetaIusedModNmS `Set.union` _exprIusedModNmS) of
                    { _lhsOusedModNmS ->
                    ( _lhsOusedModNmS) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsInm ->
         (case (expr_ ) of
          { ( _exprIusedModNmS) ->
              (case (_exprIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsInm ->
         (case (cmetas_ ) of
          { ( _cmetasIusedModNmS) ->
              (case (_cmetasIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsInm ->
         (case (Set.empty) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsInm ->
         (case (Set.empty) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsInm ->
         (case (expr_ ) of
          { ( _exprIusedModNmS) ->
              (case (_exprIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nm                   : HsName
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
      alternative Nil:
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = HsName ->
                  ( HsNameS)
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsInm ->
         (case (_lhsInm) of
          { _tlOnm ->
          (case (tl_ _tlOnm ) of
           { ( _tlIusedModNmS) ->
               (case (_lhsInm) of
                { _hdOnm ->
                (case (hd_ _hdOnm ) of
                 { ( _hdIusedModNmS) ->
                     (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
                      { _lhsOusedModNmS ->
                      ( _lhsOusedModNmS) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsInm ->
         (case (Set.empty) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
      alternative Char:
         child char           : {Char}
      alternative CoeArg:
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
      alternative Hole:
         child uid            : {UID}
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
      alternative Int:
         child int            : {Int}
      alternative Integer:
         child integer        : {Integer}
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
      alternative String:
         child str            : {String}
      alternative Tup:
         child tag            : {CTag}
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local nm          : {HsName}
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
type T_CExpr  = ( HsNameS)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (case (expr_ ) of
     { ( _exprIusedModNmS) ->
         (case (ann_ ) of
          { ( _annIusedModNmS) ->
              (case (_annIusedModNmS `Set.union` _exprIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (case (hsnUnknown) of
     { _argOnm ->
     (case (arg_ _argOnm ) of
      { ( _argIusedModNmS) ->
          (case (func_ ) of
           { ( _funcIusedModNmS) ->
               (case (_funcIusedModNmS `Set.union` _argIusedModNmS) of
                { _lhsOusedModNmS ->
                ( _lhsOusedModNmS) }) }) }) })
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (case (dflt_ ) of
     { ( _dfltIusedModNmS) ->
         (case (alts_ ) of
          { ( _altsIusedModNmS) ->
              (case (expr_ ) of
               { ( _exprIusedModNmS) ->
                   (case (_exprIusedModNmS `Set.union` _altsIusedModNmS `Set.union` _dfltIusedModNmS) of
                    { _lhsOusedModNmS ->
                    ( _lhsOusedModNmS) }) }) }) })
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (case (errorExpr_ ) of
     { ( _errorExprIusedModNmS) ->
         (case (_errorExprIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (case (body_ ) of
     { ( _bodyIusedModNmS) ->
         (case (_bodyIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (case (func_ ) of
     { ( _funcIusedModNmS) ->
         (case (_funcIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (case (body_ ) of
     { ( _bodyIusedModNmS) ->
         (case (_bodyIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (case (body_ ) of
     { ( _bodyIusedModNmS) ->
         (case (bind_ ) of
          { ( _bindInm,_bindIusedModNmS) ->
              (case (_bindIusedModNmS `Set.union` _bodyIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (case (body_ ) of
     { ( _bodyIusedModNmS) ->
         (case (binds_ ) of
          { ( _bindsIusedModNmS) ->
              (case (_bindsIusedModNmS `Set.union` _bodyIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (case (offset_ ) of
     { ( _offsetIusedModNmS) ->
         (case (expr_ ) of
          { ( _exprIusedModNmS) ->
              (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (case (fldExpr_ ) of
     { ( _fldExprIusedModNmS) ->
         (case (offset_ ) of
          { ( _offsetIusedModNmS) ->
              (case (expr_ ) of
               { ( _exprIusedModNmS) ->
                   (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS `Set.union` _fldExprIusedModNmS) of
                    { _lhsOusedModNmS ->
                    ( _lhsOusedModNmS) }) }) }) })
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (case (fldExpr_ ) of
     { ( _fldExprIusedModNmS) ->
         (case (offset_ ) of
          { ( _offsetIusedModNmS) ->
              (case (expr_ ) of
               { ( _exprIusedModNmS) ->
                   (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS `Set.union` _fldExprIusedModNmS) of
                    { _lhsOusedModNmS ->
                    ( _lhsOusedModNmS) }) }) }) })
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (case (acbrefNm ref_) of
     { _nm ->
     (case (Set.fromList $ maybeToList $ hsnQualifier _nm) of
      { _lhsOusedModNmS ->
      ( _lhsOusedModNmS) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
      alternative Debug:
         child info           : {String}
      alternative Ty:
         child ty             : {Ty}
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
type T_CExprAnn  = ( HsNameS)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Apply0:
      alternative Function0:
      alternative Function1:
      alternative Plain:
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
type T_CMetaBind  = ( HsNameS)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Dict:
      alternative DictClass:
         child tracks         : {[Track]}
      alternative DictInstance:
         child tracks         : {[Track]}
      alternative Track:
         child track          : {Track}
      alternative Val:
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
type T_CMetaVal  = ( HsNameS)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CMetas ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = ( HsNameS)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (case (x2_ ) of
     { ( _x2IusedModNmS) ->
         (case (x1_ ) of
          { ( _x1IusedModNmS) ->
              (case (_x1IusedModNmS `Set.union` _x2IusedModNmS) of
               { _lhsOusedModNmS ->
               ( _lhsOusedModNmS) }) }) })
-- CModule -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = ( HsNameS)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (case (expr_ ) of
     { ( _exprIusedModNmS) ->
         (case (_exprIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
-- CPat --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         fldNmL               : [HsName]
         usedModNmS           : HsNameS
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
      alternative Char:
         child char           : {Char}
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
      alternative Int:
         child int            : {Int}
      alternative Var:
         child pnm            : {HsName}
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
type T_CPat  = ( ([HsName]),HsNameS)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (case ([]) of
     { _lhsOfldNmL ->
     (case (Set.empty) of
      { _lhsOusedModNmS ->
      ( _lhsOfldNmL,_lhsOusedModNmS) }) })
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (case ([]) of
     { _lhsOfldNmL ->
     (case (Set.empty) of
      { _lhsOusedModNmS ->
      ( _lhsOfldNmL,_lhsOusedModNmS) }) })
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (case (binds_ ) of
     { ( _bindsIfldNmL,_bindsIusedModNmS) ->
         (case (_bindsIfldNmL) of
          { _lhsOfldNmL ->
          (case (rest_ ) of
           { ( _restIusedModNmS) ->
               (case (_restIusedModNmS `Set.union` _bindsIusedModNmS) of
                { _lhsOusedModNmS ->
                ( _lhsOfldNmL,_lhsOusedModNmS) }) }) }) })
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (case ([]) of
     { _lhsOfldNmL ->
     (case (Set.empty) of
      { _lhsOusedModNmS ->
      ( _lhsOfldNmL,_lhsOusedModNmS) }) })
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (case ([]) of
     { _lhsOfldNmL ->
     (case (Set.empty) of
      { _lhsOusedModNmS ->
      ( _lhsOfldNmL,_lhsOusedModNmS) }) })
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         fldNmL               : [HsName]
         usedModNmS           : HsNameS
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = ( ([HsName]),HsNameS)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (case (bind_ ) of
     { ( _bindInm,_bindIusedModNmS) ->
         (case (_bindInm) of
          { _fldNm ->
          (case ([_fldNm]) of
           { _lhsOfldNmL ->
           (case (fldAnns_ ) of
            { ( _fldAnnsIusedModNmS) ->
                (case (offset_ ) of
                 { ( _offsetIusedModNmS) ->
                     (case (_offsetIusedModNmS `Set.union` _bindIusedModNmS `Set.union` _fldAnnsIusedModNmS) of
                      { _lhsOusedModNmS ->
                      ( _lhsOfldNmL,_lhsOusedModNmS) }) }) }) }) }) })
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         fldNmL               : [HsName]
         usedModNmS           : HsNameS
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
      alternative Nil:
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = ( ([HsName]),HsNameS)
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIfldNmL,_tlIusedModNmS) ->
         (case (hd_ ) of
          { ( _hdIfldNmL,_hdIusedModNmS) ->
              (case (_hdIfldNmL ++ _tlIfldNmL) of
               { _lhsOfldNmL ->
               (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
                { _lhsOusedModNmS ->
                ( _lhsOfldNmL,_lhsOusedModNmS) }) }) }) })
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (case ([]) of
     { _lhsOfldNmL ->
     (case (Set.empty) of
      { _lhsOusedModNmS ->
      ( _lhsOfldNmL,_lhsOusedModNmS) }) })
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Empty:
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = ( HsNameS)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
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
type T_CodeAGItf  = ( HsNameS)
data Inh_CodeAGItf  = Inh_CodeAGItf {}
data Syn_CodeAGItf  = Syn_CodeAGItf {usedModNmS_Syn_CodeAGItf :: !(HsNameS)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf )  =
    (let ( _lhsOusedModNmS) = sem 
     in  (Syn_CodeAGItf _lhsOusedModNmS ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleIusedModNmS) ->
         (case (_moduleIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         usedModNmS           : HsNameS
   alternatives:
      alternative Just:
         child just           : CExpr 
      alternative Nothing:
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = ( HsNameS)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (case (just_ ) of
     { ( _justIusedModNmS) ->
         (case (_justIusedModNmS) of
          { _lhsOusedModNmS ->
          ( _lhsOusedModNmS) }) })
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (case (Set.empty) of
     { _lhsOusedModNmS ->
     ( _lhsOusedModNmS) })
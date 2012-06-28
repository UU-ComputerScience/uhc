

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/ConstProp.ag)
module EH101.Core.Trf.ConstProp(cmodTrfConstProp) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import Data.Maybe









cmodTrfConstProp :: EHCOpts -> CModule -> CModule
cmodTrfConstProp opts cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf
                               { opts_Inh_CodeAGItf = opts
                               })
     in   cTrf_Syn_CodeAGItf t

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
            local lev         : _
            local cTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = EvalCtx ->
               Bool ->
               Bool ->
               Int ->
               EHCOpts ->
               ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _patOopts | _patOopts `seq` (True) ->
           (case (True) of
            { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
            (case (expr_ ) of
             { ( _exprIwhatBelow,expr_1) | True ->
                 (case (ExprIsOther) of
                  { _whatAbove | _whatAbove `seq` (True) ->
                  (case (_whatAbove) of
                   { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                   (case (_lhsIlev + 1) of
                    { _lev | _lev `seq` (True) ->
                    (case (_lev) of
                     { _exprOlev | _exprOlev `seq` (True) ->
                     (case (_lhsIisStrict) of
                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                      (case (_lhsIisLamBody) of
                       { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                       (case (_lhsIevalCtx) of
                        { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                        (case (True) of
                         { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                         (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                          { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                              (case (_lev) of
                               { _patOlev | _patOlev `seq` (True) ->
                               (case (pat_ _patOlev _patOopts ) of
                                { ( _patIcTrf,_patIfldNmL) | True ->
                                    (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                                     { _cTrf | _cTrf `seq` (True) ->
                                     (case (_cTrf) of
                                      { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                      ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         opts                 : EHCOpts
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
type T_CAltL  = EvalCtx ->
                Bool ->
                Bool ->
                Int ->
                EHCOpts ->
                ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (_lhsIisStrict) of
             { _tlOisStrict | _tlOisStrict `seq` (True) ->
             (case (_lhsIisLamBody) of
              { _tlOisLamBody | _tlOisLamBody `seq` (True) ->
              (case (_lhsIevalCtx) of
               { _tlOevalCtx | _tlOevalCtx `seq` (True) ->
               (case (tl_ _tlOevalCtx _tlOisLamBody _tlOisStrict _tlOlev _tlOopts ) of
                { ( _tlIcTrf) | True ->
                    (case (_lhsIlev) of
                     { _hdOlev | _hdOlev `seq` (True) ->
                     (case (_lhsIisStrict) of
                      { _hdOisStrict | _hdOisStrict `seq` (True) ->
                      (case (_lhsIisLamBody) of
                       { _hdOisLamBody | _hdOisLamBody `seq` (True) ->
                       (case (_lhsIevalCtx) of
                        { _hdOevalCtx | _hdOevalCtx `seq` (True) ->
                        (case (hd_ _hdOevalCtx _hdOisLamBody _hdOisStrict _hdOlev _hdOopts ) of
                         { ( _hdIcTrf) | True ->
                             (case ((:) _hdIcTrf _tlIcTrf) of
                              { _cTrf | _cTrf `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsIopts ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         nm                   : HsName
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
type T_CBind  = EvalCtx ->
                Bool ->
                Bool ->
                Bool ->
                CBindCateg ->
                Int ->
                EHCOpts ->
                ( CBind ,HsName)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bindAspectsOopts | _bindAspectsOopts `seq` (True) ->
          (case (_lhsIlev) of
           { _bindAspectsOlev | _bindAspectsOlev `seq` (True) ->
           (case (_lhsIletBindingsCateg) of
            { _bindAspectsOletBindingsCateg | _bindAspectsOletBindingsCateg `seq` (True) ->
            (case (_lhsIisStrict) of
             { _bindAspectsOisStrict | _bindAspectsOisStrict `seq` (True) ->
             (case (_lhsIisLamBody) of
              { _bindAspectsOisLamBody | _bindAspectsOisLamBody `seq` (True) ->
              (case (_lhsIisGlobal) of
               { _bindAspectsOisGlobal | _bindAspectsOisGlobal `seq` (True) ->
               (case (_lhsIevalCtx) of
                { _bindAspectsOevalCtx | _bindAspectsOevalCtx `seq` (True) ->
                (case (nm_) of
                 { _bindAspectsOnm | _bindAspectsOnm `seq` (True) ->
                 (case (bindAspects_ _bindAspectsOevalCtx _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOnm _bindAspectsOopts ) of
                  { ( _bindAspectsIcTrf) | True ->
                      (case (CBind_Bind nm_ _bindAspectsIcTrf) of
                       { _cTrf | _cTrf `seq` (True) ->
                       (case (_cTrf) of
                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                        (case (nm_) of
                         { _lhsOnm | _lhsOnm `seq` (True) ->
                         ( _lhsOcTrf,_lhsOnm) }) }) }) }) }) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CBindAnn  = Int ->
                   EHCOpts ->
                   ( CBindAnn )
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CBindAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CBindAnnL  = Int ->
                    EHCOpts ->
                    ( CBindAnnL )
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIlev) of
           { _tlOlev | _tlOlev `seq` (True) ->
           (case (tl_ _tlOlev _tlOopts ) of
            { ( _tlIcTrf) | True ->
                (case (_lhsIopts) of
                 { _hdOopts | _hdOopts `seq` (True) ->
                 (case (_lhsIlev) of
                  { _hdOlev | _hdOlev `seq` (True) ->
                  (case (hd_ _hdOlev _hdOopts ) of
                   { ( _hdIcTrf) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         ( _lhsOcTrf) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlev
       _lhsIopts ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         opts                 : EHCOpts
      synthesized attribute:
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
type T_CBindL  = EvalCtx ->
                 Bool ->
                 Bool ->
                 Bool ->
                 CBindCateg ->
                 Int ->
                 EHCOpts ->
                 ( CBindL )
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (_lhsIletBindingsCateg) of
             { _tlOletBindingsCateg | _tlOletBindingsCateg `seq` (True) ->
             (case (_lhsIisStrict) of
              { _tlOisStrict | _tlOisStrict `seq` (True) ->
              (case (_lhsIisLamBody) of
               { _tlOisLamBody | _tlOisLamBody `seq` (True) ->
               (case (_lhsIisGlobal) of
                { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
                (case (_lhsIevalCtx) of
                 { _tlOevalCtx | _tlOevalCtx `seq` (True) ->
                 (case (tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOopts ) of
                  { ( _tlIcTrf) | True ->
                      (case (_lhsIlev) of
                       { _hdOlev | _hdOlev `seq` (True) ->
                       (case (_lhsIletBindingsCateg) of
                        { _hdOletBindingsCateg | _hdOletBindingsCateg `seq` (True) ->
                        (case (_lhsIisStrict) of
                         { _hdOisStrict | _hdOisStrict `seq` (True) ->
                         (case (_lhsIisLamBody) of
                          { _hdOisLamBody | _hdOisLamBody `seq` (True) ->
                          (case (_lhsIisGlobal) of
                           { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                           (case (_lhsIevalCtx) of
                            { _hdOevalCtx | _hdOevalCtx `seq` (True) ->
                            (case (hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOletBindingsCateg _hdOlev _hdOopts ) of
                             { ( _hdIcTrf,_hdInm) | True ->
                                 (case ((:) _hdIcTrf _tlIcTrf) of
                                  { _cTrf | _cTrf `seq` (True) ->
                                  (case (_cTrf) of
                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                   ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIopts ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local whatAbove   : {WhatExpr}
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
            local whatAbove   : {WhatExpr}
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
type T_CBound  = EvalCtx ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 CBindCateg ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 ( CBound )
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (True) of
           { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
           (case (expr_ ) of
            { ( _exprIwhatBelow,expr_1) | True ->
                (case (ExprIsBind) of
                 { _whatAbove | _whatAbove `seq` (True) ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                  (case (_lhsIlev) of
                   { _exprOlev | _exprOlev `seq` (True) ->
                   (case (_lhsIisLamBody) of
                    { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                    (case (_lhsIevalCtx) of
                     { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                     (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                      (case (True) of
                       { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                        { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                            (case (_lhsIopts) of
                             { _bindMetaOopts | _bindMetaOopts `seq` (True) ->
                             (case (_lhsIlev) of
                              { _bindMetaOlev | _bindMetaOlev `seq` (True) ->
                              (case (bindMeta_ _bindMetaOlev _bindMetaOopts ) of
                               { ( _bindMetaIcTrf) | True ->
                                   (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                                    { _cTrf | _cTrf `seq` (True) ->
                                    (case (_cTrf) of
                                     { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                     ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (True) of
           { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
           (case (expr_ ) of
            { ( _exprIwhatBelow,expr_1) | True ->
                (case (ExprIsLam) of
                 { _whatAbove | _whatAbove `seq` (True) ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                  (case (_lhsIlev) of
                   { _exprOlev | _exprOlev `seq` (True) ->
                   (case (_lhsIisLamBody) of
                    { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                    (case (_lhsIevalCtx) of
                     { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                     (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                      (case (True) of
                       { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                        { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                            (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _cmetasOopts | _cmetasOopts `seq` (True) ->
          (case (_lhsIlev) of
           { _cmetasOlev | _cmetasOlev `seq` (True) ->
           (case (cmetas_ _cmetasOlev _cmetasOopts ) of
            { ( _cmetasIcTrf) | True ->
                (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  ( _lhsOcTrf) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (CBound_RelevTy aspectKeyS_ relevTy_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (CBound_Ty aspectKeyS_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (_lhsIisTopApp) of
           { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
           (case (expr_ ) of
            { ( _exprIwhatBelow,expr_1) | True ->
                (case (ExprIsBind) of
                 { _whatAbove | _whatAbove `seq` (True) ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                  (case (_lhsIlev) of
                   { _exprOlev | _exprOlev `seq` (True) ->
                   (case (_lhsIisTopTup) of
                    { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                    (case (_lhsIisLamBody) of
                     { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                     (case (_lhsIevalCtx) of
                      { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                      (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                       { _exprOisStrict | _exprOisStrict `seq` (True) ->
                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                        { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                            (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
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
type T_CBoundL  = EvalCtx ->
                  Bool ->
                  Bool ->
                  Bool ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  ( CBoundL )
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (True) of
            { _hdOisTopApp | _hdOisTopApp `seq` (True) ->
            (case (_lhsInm) of
             { _tlOnm | _tlOnm `seq` (True) ->
             (case (_lhsIlev) of
              { _tlOlev | _tlOlev `seq` (True) ->
              (case (_lhsIletBindingsCateg) of
               { _tlOletBindingsCateg | _tlOletBindingsCateg `seq` (True) ->
               (case (_lhsIisStrict) of
                { _tlOisStrict | _tlOisStrict `seq` (True) ->
                (case (_lhsIisLamBody) of
                 { _tlOisLamBody | _tlOisLamBody `seq` (True) ->
                 (case (_lhsIisGlobal) of
                  { _tlOisGlobal | _tlOisGlobal `seq` (True) ->
                  (case (_lhsIevalCtx) of
                   { _tlOevalCtx | _tlOevalCtx `seq` (True) ->
                   (case (tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOnm _tlOopts ) of
                    { ( _tlIcTrf) | True ->
                        (case (_lhsInm) of
                         { _hdOnm | _hdOnm `seq` (True) ->
                         (case (_lhsIlev) of
                          { _hdOlev | _hdOlev `seq` (True) ->
                          (case (_lhsIletBindingsCateg) of
                           { _hdOletBindingsCateg | _hdOletBindingsCateg `seq` (True) ->
                           (case (_lhsIisStrict) of
                            { _hdOisStrict | _hdOisStrict `seq` (True) ->
                            (case (_lhsIisLamBody) of
                             { _hdOisLamBody | _hdOisLamBody `seq` (True) ->
                             (case (_lhsIisGlobal) of
                              { _hdOisGlobal | _hdOisGlobal `seq` (True) ->
                              (case (_lhsIevalCtx) of
                               { _hdOevalCtx | _hdOevalCtx `seq` (True) ->
                               (case (True) of
                                { _hdOisTopTup | _hdOisTopTup `seq` (True) ->
                                (case (hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOletBindingsCateg _hdOlev _hdOnm _hdOopts ) of
                                 { ( _hdIcTrf) | True ->
                                     (case ((:) _hdIcTrf _tlIcTrf) of
                                      { _cTrf | _cTrf `seq` (True) ->
                                      (case (_cTrf) of
                                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                       ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIopts ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         whatBelow            : WhatExpr
   visit 1:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lev                  : Int
         opts                 : EHCOpts
         whatAbove            : WhatExpr
      synthesized attributes:
         appArgL              : [CBound]
         appLam               : CExpr 
         cTrf                 : SELF 
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 1:
            local cTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local whatBelow   : _
         visit 1:
            local letBindingsCateg : _
            local isTopTup    : _
            local isGlobal    : _
            local whatAbove   : {WhatExpr}
            local appArgL     : _
            local appLam      : _
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local lev         : _
            local isTopTup    : _
            local letBindingsCateg : _
            local isGlobal    : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local evalCtx     : _
            local letBindingsCateg : _
            local isGlobal    : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local whatBelow   : _
         visit 1:
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local isTopApp    : {Bool}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local nm          : {HsName}
            local whatBelow   : _
         visit 1:
            local cTrf        : _
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
type T_CExpr  = ( WhatExpr,T_CExpr_1 )
type T_CExpr_1  = EvalCtx ->
                  Bool ->
                  Bool ->
                  Bool ->
                  Bool ->
                  Int ->
                  EHCOpts ->
                  WhatExpr ->
                  ( ([CBound]),CExpr ,CExpr )
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (case (expr_ ) of
     { ( _exprIwhatBelow,expr_1) | True ->
         (case (_exprIwhatBelow) of
          { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
          (case ((let sem_CExpr_Ann_1 :: T_CExpr_1 
                      sem_CExpr_Ann_1  =
                          (\ _lhsIevalCtx
                             _lhsIisLamBody
                             _lhsIisStrict
                             _lhsIisTopApp
                             _lhsIisTopTup
                             _lhsIlev
                             _lhsIopts
                             _lhsIwhatAbove ->
                               (case (_lhsIopts) of
                                { _exprOopts | _exprOopts `seq` (True) ->
                                (case (_lhsIwhatAbove) of
                                 { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                 (case (_lhsIlev) of
                                  { _exprOlev | _exprOlev `seq` (True) ->
                                  (case (_lhsIisTopTup) of
                                   { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                   (case (_lhsIisTopApp) of
                                    { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                    (case (_lhsIisStrict) of
                                     { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                     (case (_lhsIisLamBody) of
                                      { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                      (case (_lhsIevalCtx) of
                                       { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                                        { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                                            (case (_exprIappArgL) of
                                             { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                                             (case (_exprIappLam) of
                                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                              (case (_lhsIopts) of
                                               { _annOopts | _annOopts `seq` (True) ->
                                               (case (_lhsIlev) of
                                                { _annOlev | _annOlev `seq` (True) ->
                                                (case (ann_ _annOlev _annOopts ) of
                                                 { ( _annIcTrf) | True ->
                                                     (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                                                      { _cTrf | _cTrf `seq` (True) ->
                                                      (case (_cTrf) of
                                                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                       ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_CExpr_Ann_1)) of
           { ( sem_CExpr_1) | True ->
           ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (case (func_ ) of
     { ( _funcIwhatBelow,func_1) | True ->
         (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow) of
          { _whatBelow | _whatBelow `seq` (True) ->
          (case (_whatBelow) of
           { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
           (case ((let sem_CExpr_App_1 :: T_CExpr_1 
                       sem_CExpr_App_1  =
                           (\ _lhsIevalCtx
                              _lhsIisLamBody
                              _lhsIisStrict
                              _lhsIisTopApp
                              _lhsIisTopTup
                              _lhsIlev
                              _lhsIopts
                              _lhsIwhatAbove ->
                                (case (_lhsIopts) of
                                 { _argOopts | _argOopts `seq` (True) ->
                                 (case (_lhsIopts) of
                                  { _funcOopts | _funcOopts `seq` (True) ->
                                  (case (True) of
                                   { _argOisTopApp | _argOisTopApp `seq` (True) ->
                                   (case (_lhsIlev) of
                                    { _argOlev | _argOlev `seq` (True) ->
                                    (case (acoreBindcategPlain) of
                                     { _letBindingsCateg | _letBindingsCateg `seq` (True) ->
                                     (case (_letBindingsCateg) of
                                      { _argOletBindingsCateg | _argOletBindingsCateg `seq` (True) ->
                                      (case (True) of
                                       { _isTopTup | _isTopTup `seq` (True) ->
                                       (case (_isTopTup) of
                                        { _argOisTopTup | _argOisTopTup `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _argOisStrict | _argOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _argOisLamBody | _argOisLamBody `seq` (True) ->
                                          (case (False) of
                                           { _isGlobal | _isGlobal `seq` (True) ->
                                           (case (_isGlobal) of
                                            { _argOisGlobal | _argOisGlobal `seq` (True) ->
                                            (case (_lhsIevalCtx) of
                                             { _argOevalCtx | _argOevalCtx `seq` (True) ->
                                             (case (hsnUnknown) of
                                              { _argOnm | _argOnm `seq` (True) ->
                                              (case (arg_ _argOevalCtx _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOletBindingsCateg _argOlev _argOnm _argOopts ) of
                                               { ( _argIcTrf) | True ->
                                                   (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove) of
                                                    { _whatAbove | _whatAbove `seq` (True) ->
                                                    (case (_whatAbove) of
                                                     { _funcOwhatAbove | _funcOwhatAbove `seq` (True) ->
                                                     (case (_lhsIlev) of
                                                      { _funcOlev | _funcOlev `seq` (True) ->
                                                      (case (_isTopTup) of
                                                       { _funcOisTopTup | _funcOisTopTup `seq` (True) ->
                                                       (case (_lhsIisStrict) of
                                                        { _funcOisStrict | _funcOisStrict `seq` (True) ->
                                                        (case (_lhsIisLamBody) of
                                                         { _funcOisLamBody | _funcOisLamBody `seq` (True) ->
                                                         (case (_lhsIevalCtx) of
                                                          { _funcOevalCtx | _funcOevalCtx `seq` (True) ->
                                                          (case (False) of
                                                           { _funcOisTopApp | _funcOisTopApp `seq` (True) ->
                                                           (case (func_1 _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOopts _funcOwhatAbove ) of
                                                            { ( _funcIappArgL,_funcIappLam,_funcIcTrf) | True ->
                                                                (case (_argIcTrf : _funcIappArgL) of
                                                                 { _appArgL | _appArgL `seq` (True) ->
                                                                 (case (_appArgL) of
                                                                  { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                                                                  (case (_funcIappLam) of
                                                                   { _appLam | _appLam `seq` (True) ->
                                                                   (case (_appLam) of
                                                                    { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                    (case (CExpr_App _funcIcTrf _argIcTrf) of
                                                                     { _cTrf | _cTrf `seq` (True) ->
                                                                     (case (if _lhsIisTopApp
                                                                            then  case (acoreExprMbVar _appLam,reverse _appArgL) of
                                                                                    (Just n,[e1,e2]) | n == (ehcOptBuiltin _lhsIopts ehbnPrimAddInt) && isJust i1 && isJust i2
                                                                                      -> acoreInt (fromInteger $ snd (fromJust i1) + snd (fromJust i2))
                                                                                      where i1 = acoreExprMbInt $ acoreUnBoundVal e1
                                                                                            i2 = acoreExprMbInt $ acoreUnBoundVal e2
                                                                                    _ -> _cTrf
                                                                            else  _cTrf) of
                                                                      { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                      ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_CExpr_App_1)) of
            { ( sem_CExpr_1) | True ->
            ( _lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Case_1 :: T_CExpr_1 
                  sem_CExpr_Case_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _dfltOopts | _dfltOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _dfltOisTopApp | _dfltOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _altsOopts | _altsOopts `seq` (True) ->
                                (case (_lhsIopts) of
                                 { _exprOopts | _exprOopts `seq` (True) ->
                                 (case (_isTopApp) of
                                  { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                  (case (dflt_ ) of
                                   { ( _dfltIwhatBelow,dflt_1) | True ->
                                       (case (ExprIsOther) of
                                        { _whatAbove | _whatAbove `seq` (True) ->
                                        (case (_whatAbove) of
                                         { _dfltOwhatAbove | _dfltOwhatAbove `seq` (True) ->
                                         (case (_lhsIlev) of
                                          { _dfltOlev | _dfltOlev `seq` (True) ->
                                          (case (True) of
                                           { _isTopTup | _isTopTup `seq` (True) ->
                                           (case (_isTopTup) of
                                            { _dfltOisTopTup | _dfltOisTopTup `seq` (True) ->
                                            (case (_lhsIisStrict) of
                                             { _dfltOisStrict | _dfltOisStrict `seq` (True) ->
                                             (case (_lhsIisLamBody) of
                                              { _dfltOisLamBody | _dfltOisLamBody `seq` (True) ->
                                              (case (_lhsIevalCtx) of
                                               { _dfltOevalCtx | _dfltOevalCtx `seq` (True) ->
                                               (case (dflt_1 _dfltOevalCtx _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlev _dfltOopts _dfltOwhatAbove ) of
                                                { ( _dfltIappArgL,_dfltIappLam,_dfltIcTrf) | True ->
                                                    (case (_lhsIlev) of
                                                     { _altsOlev | _altsOlev `seq` (True) ->
                                                     (case (_lhsIisStrict) of
                                                      { _altsOisStrict | _altsOisStrict `seq` (True) ->
                                                      (case (_lhsIisLamBody) of
                                                       { _altsOisLamBody | _altsOisLamBody `seq` (True) ->
                                                       (case (_lhsIevalCtx) of
                                                        { _altsOevalCtx | _altsOevalCtx `seq` (True) ->
                                                        (case (alts_ _altsOevalCtx _altsOisLamBody _altsOisStrict _altsOlev _altsOopts ) of
                                                         { ( _altsIcTrf) | True ->
                                                             (case (expr_ ) of
                                                              { ( _exprIwhatBelow,expr_1) | True ->
                                                                  (case (_whatAbove) of
                                                                   { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                                   (case (_lhsIlev) of
                                                                    { _exprOlev | _exprOlev `seq` (True) ->
                                                                    (case (_isTopTup) of
                                                                     { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                     (case (_lhsIisStrict) of
                                                                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                      (case (_lhsIisLamBody) of
                                                                       { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                       (case (_lhsIevalCtx) of
                                                                        { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                        (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                                                                         { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                                                                             (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                                                                              { _cTrf | _cTrf `seq` (True) ->
                                                                              (case (_cTrf) of
                                                                               { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                               (case (_cTrf) of
                                                                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_Case_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (case (errorExpr_ ) of
     { ( _errorExprIwhatBelow,errorExpr_1) | True ->
         (case (_errorExprIwhatBelow) of
          { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
          (case ((let sem_CExpr_CaseAltFail_1 :: T_CExpr_1 
                      sem_CExpr_CaseAltFail_1  =
                          (\ _lhsIevalCtx
                             _lhsIisLamBody
                             _lhsIisStrict
                             _lhsIisTopApp
                             _lhsIisTopTup
                             _lhsIlev
                             _lhsIopts
                             _lhsIwhatAbove ->
                               (case ([]) of
                                { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                                (case (_lhsIopts) of
                                 { _errorExprOopts | _errorExprOopts `seq` (True) ->
                                 (case (True) of
                                  { _isTopApp | _isTopApp `seq` (True) ->
                                  (case (_isTopApp) of
                                   { _errorExprOisTopApp | _errorExprOisTopApp `seq` (True) ->
                                   (case (ExprIsOther) of
                                    { _whatAbove | _whatAbove `seq` (True) ->
                                    (case (_whatAbove) of
                                     { _errorExprOwhatAbove | _errorExprOwhatAbove `seq` (True) ->
                                     (case (_lhsIlev) of
                                      { _errorExprOlev | _errorExprOlev `seq` (True) ->
                                      (case (True) of
                                       { _isTopTup | _isTopTup `seq` (True) ->
                                       (case (_isTopTup) of
                                        { _errorExprOisTopTup | _errorExprOisTopTup `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _errorExprOisStrict | _errorExprOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _errorExprOisLamBody | _errorExprOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _errorExprOevalCtx | _errorExprOevalCtx `seq` (True) ->
                                           (case (errorExpr_1 _errorExprOevalCtx _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlev _errorExprOopts _errorExprOwhatAbove ) of
                                            { ( _errorExprIappArgL,_errorExprIappLam,_errorExprIcTrf) | True ->
                                                (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                  (case (_cTrf) of
                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                   ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_CExpr_CaseAltFail_1)) of
           { ( sem_CExpr_1) | True ->
           ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Char_1 :: T_CExpr_1 
                  sem_CExpr_Char_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_Char char_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_Char_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_CoeArg_1 :: T_CExpr_1 
                  sem_CExpr_CoeArg_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_CoeArg) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_CoeArg_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_FFI_1 :: T_CExpr_1 
                  sem_CExpr_FFI_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_FFI_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Hole_1 :: T_CExpr_1 
                  sem_CExpr_Hole_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_Hole uid_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_Hole_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_HoleLet_1 :: T_CExpr_1 
                  sem_CExpr_HoleLet_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _bodyOopts | _bodyOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                               (case (body_ ) of
                                { ( _bodyIwhatBelow,body_1) | True ->
                                    (case (ExprIsOther) of
                                     { _whatAbove | _whatAbove `seq` (True) ->
                                     (case (_whatAbove) of
                                      { _bodyOwhatAbove | _bodyOwhatAbove `seq` (True) ->
                                      (case (_lhsIlev) of
                                       { _bodyOlev | _bodyOlev `seq` (True) ->
                                       (case (True) of
                                        { _isTopTup | _isTopTup `seq` (True) ->
                                        (case (_isTopTup) of
                                         { _bodyOisTopTup | _bodyOisTopTup `seq` (True) ->
                                         (case (_lhsIisStrict) of
                                          { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                          (case (_lhsIisLamBody) of
                                           { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                           (case (_lhsIevalCtx) of
                                            { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                            (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOopts _bodyOwhatAbove ) of
                                             { ( _bodyIappArgL,_bodyIappLam,_bodyIcTrf) | True ->
                                                 (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                                                  { _cTrf | _cTrf `seq` (True) ->
                                                  (case (_cTrf) of
                                                   { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                   (case (_cTrf) of
                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                    ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_HoleLet_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_ImplsApp_1 :: T_CExpr_1 
                  sem_CExpr_ImplsApp_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _funcOopts | _funcOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _funcOisTopApp | _funcOisTopApp `seq` (True) ->
                               (case (func_ ) of
                                { ( _funcIwhatBelow,func_1) | True ->
                                    (case (ExprIsOther) of
                                     { _whatAbove | _whatAbove `seq` (True) ->
                                     (case (_whatAbove) of
                                      { _funcOwhatAbove | _funcOwhatAbove `seq` (True) ->
                                      (case (_lhsIlev) of
                                       { _funcOlev | _funcOlev `seq` (True) ->
                                       (case (True) of
                                        { _isTopTup | _isTopTup `seq` (True) ->
                                        (case (_isTopTup) of
                                         { _funcOisTopTup | _funcOisTopTup `seq` (True) ->
                                         (case (_lhsIisStrict) of
                                          { _funcOisStrict | _funcOisStrict `seq` (True) ->
                                          (case (_lhsIisLamBody) of
                                           { _funcOisLamBody | _funcOisLamBody `seq` (True) ->
                                           (case (_lhsIevalCtx) of
                                            { _funcOevalCtx | _funcOevalCtx `seq` (True) ->
                                            (case (func_1 _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOopts _funcOwhatAbove ) of
                                             { ( _funcIappArgL,_funcIappLam,_funcIcTrf) | True ->
                                                 (case (CExpr_ImplsApp _funcIcTrf uid_) of
                                                  { _cTrf | _cTrf `seq` (True) ->
                                                  (case (_cTrf) of
                                                   { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                   (case (_cTrf) of
                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                    ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_ImplsApp_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_ImplsLam_1 :: T_CExpr_1 
                  sem_CExpr_ImplsLam_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _bodyOopts | _bodyOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                               (case (body_ ) of
                                { ( _bodyIwhatBelow,body_1) | True ->
                                    (case (ExprIsOther) of
                                     { _whatAbove | _whatAbove `seq` (True) ->
                                     (case (_whatAbove) of
                                      { _bodyOwhatAbove | _bodyOwhatAbove `seq` (True) ->
                                      (case (_lhsIlev) of
                                       { _bodyOlev | _bodyOlev `seq` (True) ->
                                       (case (True) of
                                        { _isTopTup | _isTopTup `seq` (True) ->
                                        (case (_isTopTup) of
                                         { _bodyOisTopTup | _bodyOisTopTup `seq` (True) ->
                                         (case (_lhsIisStrict) of
                                          { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                          (case (_lhsIisLamBody) of
                                           { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                           (case (_lhsIevalCtx) of
                                            { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                            (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOopts _bodyOwhatAbove ) of
                                             { ( _bodyIappArgL,_bodyIappLam,_bodyIcTrf) | True ->
                                                 (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                                                  { _cTrf | _cTrf `seq` (True) ->
                                                  (case (_cTrf) of
                                                   { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                   (case (_cTrf) of
                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                    ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_ImplsLam_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (case (ExprIsInt int_) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Int_1 :: T_CExpr_1 
                  sem_CExpr_Int_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_Int int_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_Int_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Integer_1 :: T_CExpr_1 
                  sem_CExpr_Integer_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_Integer integer_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_Integer_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (case (ExprIsLam) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Lam_1 :: T_CExpr_1 
                  sem_CExpr_Lam_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _bodyOopts | _bodyOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _bindOopts | _bindOopts `seq` (True) ->
                                (case (body_ ) of
                                 { ( _bodyIwhatBelow,body_1) | True ->
                                     (case (ExprIsLam) of
                                      { _whatAbove | _whatAbove `seq` (True) ->
                                      (case (_whatAbove) of
                                       { _bodyOwhatAbove | _bodyOwhatAbove `seq` (True) ->
                                       (case (_lhsIlev + 1) of
                                        { _lev | _lev `seq` (True) ->
                                        (case (_lev) of
                                         { _bodyOlev | _bodyOlev `seq` (True) ->
                                         (case (True) of
                                          { _isTopTup | _isTopTup `seq` (True) ->
                                          (case (_isTopTup) of
                                           { _bodyOisTopTup | _bodyOisTopTup `seq` (True) ->
                                           (case (_lhsIisStrict) of
                                            { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                            (case (_lhsIisLamBody) of
                                             { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                             (case (_lhsIevalCtx) of
                                              { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                              (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOopts _bodyOwhatAbove ) of
                                               { ( _bodyIappArgL,_bodyIappLam,_bodyIcTrf) | True ->
                                                   (case (_lev) of
                                                    { _bindOlev | _bindOlev `seq` (True) ->
                                                    (case (acoreBindcategPlain) of
                                                     { _letBindingsCateg | _letBindingsCateg `seq` (True) ->
                                                     (case (_letBindingsCateg) of
                                                      { _bindOletBindingsCateg | _bindOletBindingsCateg `seq` (True) ->
                                                      (case (_lhsIisStrict) of
                                                       { _bindOisStrict | _bindOisStrict `seq` (True) ->
                                                       (case (_lhsIisLamBody) of
                                                        { _bindOisLamBody | _bindOisLamBody `seq` (True) ->
                                                        (case (False) of
                                                         { _isGlobal | _isGlobal `seq` (True) ->
                                                         (case (_isGlobal) of
                                                          { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                                                          (case (_lhsIevalCtx) of
                                                           { _bindOevalCtx | _bindOevalCtx `seq` (True) ->
                                                           (case (bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOopts ) of
                                                            { ( _bindIcTrf,_bindInm) | True ->
                                                                (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                                                                 { _cTrf | _cTrf `seq` (True) ->
                                                                 (case (_cTrf) of
                                                                  { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                  (case (_cTrf) of
                                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                   ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_Lam_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Let_1 :: T_CExpr_1 
                  sem_CExpr_Let_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _bodyOopts | _bodyOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _bindsOopts | _bindsOopts `seq` (True) ->
                                (case (body_ ) of
                                 { ( _bodyIwhatBelow,body_1) | True ->
                                     (case (ExprIsOther) of
                                      { _whatAbove | _whatAbove `seq` (True) ->
                                      (case (_whatAbove) of
                                       { _bodyOwhatAbove | _bodyOwhatAbove `seq` (True) ->
                                       (case (_lhsIlev) of
                                        { _bodyOlev | _bodyOlev `seq` (True) ->
                                        (case (True) of
                                         { _isTopTup | _isTopTup `seq` (True) ->
                                         (case (_isTopTup) of
                                          { _bodyOisTopTup | _bodyOisTopTup `seq` (True) ->
                                          (case (_lhsIisStrict) of
                                           { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                           (case (_lhsIisLamBody) of
                                            { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                            (case (if categ_ == CBindCateg_Strict
                                                   then EvalCtx_Eval
                                                   else EvalCtx_None) of
                                             { _evalCtx | _evalCtx `seq` (True) ->
                                             (case (_evalCtx) of
                                              { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                              (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOopts _bodyOwhatAbove ) of
                                               { ( _bodyIappArgL,_bodyIappLam,_bodyIcTrf) | True ->
                                                   (case (_lhsIlev) of
                                                    { _bindsOlev | _bindsOlev `seq` (True) ->
                                                    (case (categ_) of
                                                     { _letBindingsCateg | _letBindingsCateg `seq` (True) ->
                                                     (case (_letBindingsCateg) of
                                                      { _bindsOletBindingsCateg | _bindsOletBindingsCateg `seq` (True) ->
                                                      (case (_lhsIisLamBody) of
                                                       { _bindsOisLamBody | _bindsOisLamBody `seq` (True) ->
                                                       (case (_lhsIlev == cLevModule) of
                                                        { _isGlobal | _isGlobal `seq` (True) ->
                                                        (case (_isGlobal) of
                                                         { _bindsOisGlobal | _bindsOisGlobal `seq` (True) ->
                                                         (case (_evalCtx) of
                                                          { _bindsOevalCtx | _bindsOevalCtx `seq` (True) ->
                                                          (case (_isGlobal || categ_ == CBindCateg_Strict) of
                                                           { _bindsOisStrict | _bindsOisStrict `seq` (True) ->
                                                           (case (binds_ _bindsOevalCtx _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOletBindingsCateg _bindsOlev _bindsOopts ) of
                                                            { ( _bindsIcTrf) | True ->
                                                                (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                                                                 { _cTrf | _cTrf `seq` (True) ->
                                                                 (case (_cTrf) of
                                                                  { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                  (case (_cTrf) of
                                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                   ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_Let_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_String_1 :: T_CExpr_1 
                  sem_CExpr_String_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_String str_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_String_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_Tup_1 :: T_CExpr_1 
                  sem_CExpr_Tup_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (CExpr_Tup tag_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOappLam | _lhsOappLam `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
              in  sem_CExpr_Tup_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_TupDel_1 :: T_CExpr_1 
                  sem_CExpr_TupDel_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _offsetOopts | _offsetOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _exprOopts | _exprOopts `seq` (True) ->
                                (case (_isTopApp) of
                                 { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                 (case (offset_ ) of
                                  { ( _offsetIwhatBelow,offset_1) | True ->
                                      (case (ExprIsOther) of
                                       { _whatAbove | _whatAbove `seq` (True) ->
                                       (case (_whatAbove) of
                                        { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                        (case (_lhsIlev) of
                                         { _offsetOlev | _offsetOlev `seq` (True) ->
                                         (case (True) of
                                          { _isTopTup | _isTopTup `seq` (True) ->
                                          (case (_isTopTup) of
                                           { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                           (case (_lhsIisStrict) of
                                            { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                            (case (_lhsIisLamBody) of
                                             { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                             (case (_lhsIevalCtx) of
                                              { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                              (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOopts _offsetOwhatAbove ) of
                                               { ( _offsetIappArgL,_offsetIappLam,_offsetIcTrf) | True ->
                                                   (case (expr_ ) of
                                                    { ( _exprIwhatBelow,expr_1) | True ->
                                                        (case (_whatAbove) of
                                                         { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                         (case (_lhsIlev) of
                                                          { _exprOlev | _exprOlev `seq` (True) ->
                                                          (case (_lhsIisStrict) of
                                                           { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                           (case (_lhsIisLamBody) of
                                                            { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                            (case (_lhsIevalCtx) of
                                                             { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                             (case (False) of
                                                              { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                              (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                                                               { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                                                                   (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                                                                    { _cTrf | _cTrf `seq` (True) ->
                                                                    (case (_cTrf) of
                                                                     { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                     (case (_cTrf) of
                                                                      { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                      ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupDel_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_TupIns_1 :: T_CExpr_1 
                  sem_CExpr_TupIns_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _fldExprOopts | _fldExprOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _fldExprOisTopApp | _fldExprOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _offsetOopts | _offsetOopts `seq` (True) ->
                                (case (_isTopApp) of
                                 { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                 (case (_lhsIopts) of
                                  { _exprOopts | _exprOopts `seq` (True) ->
                                  (case (_isTopApp) of
                                   { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                   (case (fldExpr_ ) of
                                    { ( _fldExprIwhatBelow,fldExpr_1) | True ->
                                        (case (ExprIsOther) of
                                         { _whatAbove | _whatAbove `seq` (True) ->
                                         (case (_whatAbove) of
                                          { _fldExprOwhatAbove | _fldExprOwhatAbove `seq` (True) ->
                                          (case (_lhsIlev) of
                                           { _fldExprOlev | _fldExprOlev `seq` (True) ->
                                           (case (True) of
                                            { _isTopTup | _isTopTup `seq` (True) ->
                                            (case (_isTopTup) of
                                             { _fldExprOisTopTup | _fldExprOisTopTup `seq` (True) ->
                                             (case (_lhsIisStrict) of
                                              { _fldExprOisStrict | _fldExprOisStrict `seq` (True) ->
                                              (case (_lhsIisLamBody) of
                                               { _fldExprOisLamBody | _fldExprOisLamBody `seq` (True) ->
                                               (case (_lhsIevalCtx) of
                                                { _fldExprOevalCtx | _fldExprOevalCtx `seq` (True) ->
                                                (case (fldExpr_1 _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOopts _fldExprOwhatAbove ) of
                                                 { ( _fldExprIappArgL,_fldExprIappLam,_fldExprIcTrf) | True ->
                                                     (case (offset_ ) of
                                                      { ( _offsetIwhatBelow,offset_1) | True ->
                                                          (case (_whatAbove) of
                                                           { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                                           (case (_lhsIlev) of
                                                            { _offsetOlev | _offsetOlev `seq` (True) ->
                                                            (case (_isTopTup) of
                                                             { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                                             (case (_lhsIisStrict) of
                                                              { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                                              (case (_lhsIisLamBody) of
                                                               { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                                               (case (_lhsIevalCtx) of
                                                                { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                                                (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOopts _offsetOwhatAbove ) of
                                                                 { ( _offsetIappArgL,_offsetIappLam,_offsetIcTrf) | True ->
                                                                     (case (expr_ ) of
                                                                      { ( _exprIwhatBelow,expr_1) | True ->
                                                                          (case (_whatAbove) of
                                                                           { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                                           (case (_lhsIlev) of
                                                                            { _exprOlev | _exprOlev `seq` (True) ->
                                                                            (case (_lhsIisStrict) of
                                                                             { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                             (case (_lhsIisLamBody) of
                                                                              { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                              (case (_lhsIevalCtx) of
                                                                               { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                               (case (False) of
                                                                                { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                                (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                                                                                 { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                                                                                     (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                      { _cTrf | _cTrf `seq` (True) ->
                                                                                      (case (_cTrf) of
                                                                                       { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                                       (case (_cTrf) of
                                                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                        ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupIns_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (case (ExprIsOther) of
     { _whatBelow | _whatBelow `seq` (True) ->
     (case (_whatBelow) of
      { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
      (case ((let sem_CExpr_TupUpd_1 :: T_CExpr_1 
                  sem_CExpr_TupUpd_1  =
                      (\ _lhsIevalCtx
                         _lhsIisLamBody
                         _lhsIisStrict
                         _lhsIisTopApp
                         _lhsIisTopTup
                         _lhsIlev
                         _lhsIopts
                         _lhsIwhatAbove ->
                           (case ([]) of
                            { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                            (case (_lhsIopts) of
                             { _fldExprOopts | _fldExprOopts `seq` (True) ->
                             (case (True) of
                              { _isTopApp | _isTopApp `seq` (True) ->
                              (case (_isTopApp) of
                               { _fldExprOisTopApp | _fldExprOisTopApp `seq` (True) ->
                               (case (_lhsIopts) of
                                { _offsetOopts | _offsetOopts `seq` (True) ->
                                (case (_isTopApp) of
                                 { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                 (case (_lhsIopts) of
                                  { _exprOopts | _exprOopts `seq` (True) ->
                                  (case (_isTopApp) of
                                   { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                   (case (fldExpr_ ) of
                                    { ( _fldExprIwhatBelow,fldExpr_1) | True ->
                                        (case (ExprIsOther) of
                                         { _whatAbove | _whatAbove `seq` (True) ->
                                         (case (_whatAbove) of
                                          { _fldExprOwhatAbove | _fldExprOwhatAbove `seq` (True) ->
                                          (case (_lhsIlev) of
                                           { _fldExprOlev | _fldExprOlev `seq` (True) ->
                                           (case (True) of
                                            { _isTopTup | _isTopTup `seq` (True) ->
                                            (case (_isTopTup) of
                                             { _fldExprOisTopTup | _fldExprOisTopTup `seq` (True) ->
                                             (case (_lhsIisStrict) of
                                              { _fldExprOisStrict | _fldExprOisStrict `seq` (True) ->
                                              (case (_lhsIisLamBody) of
                                               { _fldExprOisLamBody | _fldExprOisLamBody `seq` (True) ->
                                               (case (_lhsIevalCtx) of
                                                { _fldExprOevalCtx | _fldExprOevalCtx `seq` (True) ->
                                                (case (fldExpr_1 _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOopts _fldExprOwhatAbove ) of
                                                 { ( _fldExprIappArgL,_fldExprIappLam,_fldExprIcTrf) | True ->
                                                     (case (offset_ ) of
                                                      { ( _offsetIwhatBelow,offset_1) | True ->
                                                          (case (_whatAbove) of
                                                           { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                                           (case (_lhsIlev) of
                                                            { _offsetOlev | _offsetOlev `seq` (True) ->
                                                            (case (_isTopTup) of
                                                             { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                                             (case (_lhsIisStrict) of
                                                              { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                                              (case (_lhsIisLamBody) of
                                                               { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                                               (case (_lhsIevalCtx) of
                                                                { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                                                (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOopts _offsetOwhatAbove ) of
                                                                 { ( _offsetIappArgL,_offsetIappLam,_offsetIcTrf) | True ->
                                                                     (case (expr_ ) of
                                                                      { ( _exprIwhatBelow,expr_1) | True ->
                                                                          (case (_whatAbove) of
                                                                           { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                                           (case (_lhsIlev) of
                                                                            { _exprOlev | _exprOlev `seq` (True) ->
                                                                            (case (_lhsIisStrict) of
                                                                             { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                             (case (_lhsIisLamBody) of
                                                                              { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                              (case (_lhsIevalCtx) of
                                                                               { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                               (case (False) of
                                                                                { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                                (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                                                                                 { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                                                                                     (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                      { _cTrf | _cTrf `seq` (True) ->
                                                                                      (case (_cTrf) of
                                                                                       { _lhsOappLam | _lhsOappLam `seq` (True) ->
                                                                                       (case (_cTrf) of
                                                                                        { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                        ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
              in  sem_CExpr_TupUpd_1)) of
       { ( sem_CExpr_1) | True ->
       ( _lhsOwhatBelow,sem_CExpr_1) }) }) })
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (case (acbrefNm ref_) of
     { _nm | _nm `seq` (True) ->
     (case (ExprIsVar _nm) of
      { _whatBelow | _whatBelow `seq` (True) ->
      (case (_whatBelow) of
       { _lhsOwhatBelow | _lhsOwhatBelow `seq` (True) ->
       (case ((let sem_CExpr_Var_1 :: T_CExpr_1 
                   sem_CExpr_Var_1  =
                       (\ _lhsIevalCtx
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsIopts
                          _lhsIwhatAbove ->
                            (case ([]) of
                             { _lhsOappArgL | _lhsOappArgL `seq` (True) ->
                             (case (CExpr_Var ref_) of
                              { _cTrf | _cTrf `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOappLam | _lhsOappLam `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                ( _lhsOappArgL,_lhsOappLam,_lhsOcTrf) }) }) }) }))
               in  sem_CExpr_Var_1)) of
        { ( sem_CExpr_1) | True ->
        ( _lhsOwhatBelow,sem_CExpr_1) }) }) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CExprAnn  = Int ->
                   EHCOpts ->
                   ( CExprAnn )
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CExprAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CExprAnn_Debug info_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CExprAnn_Ty ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CMetaBind  = Int ->
                    EHCOpts ->
                    ( CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaBind_Apply0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaBind_Function0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaBind_Function1) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaBind_Plain) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CMetaVal  = Int ->
                   EHCOpts ->
                   ( CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaVal_Dict) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaVal_DictClass tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaVal_Track track_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CMetaVal_Val) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CMetas  = Int ->
                 EHCOpts ->
                 ( CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _x2Oopts | _x2Oopts `seq` (True) ->
          (case (_lhsIlev) of
           { _x2Olev | _x2Olev `seq` (True) ->
           (case (x2_ _x2Olev _x2Oopts ) of
            { ( _x2IcTrf) | True ->
                (case (_lhsIopts) of
                 { _x1Oopts | _x1Oopts `seq` (True) ->
                 (case (_lhsIlev) of
                  { _x1Olev | _x1Olev `seq` (True) ->
                  (case (x1_ _x1Olev _x1Oopts ) of
                   { ( _x1IcTrf) | True ->
                       (case ((_x1IcTrf,_x2IcTrf)) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         ( _lhsOcTrf) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local whatAbove   : {WhatExpr}
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = Int ->
                  EHCOpts ->
                  ( CModule )
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (True) of
           { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
           (case (expr_ ) of
            { ( _exprIwhatBelow,expr_1) | True ->
                (case (ExprIsOther) of
                 { _whatAbove | _whatAbove `seq` (True) ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                  (case (_lhsIlev) of
                   { _exprOlev | _exprOlev `seq` (True) ->
                   (case (EvalCtx_Eval) of
                    { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                    (case (False) of
                     { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                     (case (True) of
                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                      (case (True) of
                       { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOopts _exprOwhatAbove ) of
                        { ( _exprIappArgL,_exprIappLam,_exprIcTrf) | True ->
                            (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
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
type T_CPat  = Int ->
               EHCOpts ->
               ( CPat ,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPat_BoolExpr cexpr_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPat_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bindsOopts | _bindsOopts `seq` (True) ->
          (case (_lhsIlev) of
           { _bindsOlev | _bindsOlev `seq` (True) ->
           (case (binds_ _bindsOlev _bindsOopts ) of
            { ( _bindsIcTrf,_bindsIfldNmL) | True ->
                (case (_lhsIopts) of
                 { _restOopts | _restOopts `seq` (True) ->
                 (case (_lhsIlev) of
                  { _restOlev | _restOlev `seq` (True) ->
                  (case (rest_ _restOlev _restOopts ) of
                   { ( _restIcTrf) | True ->
                       (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_bindsIfldNmL) of
                          { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                          ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPat_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPat_Var pnm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local whatAbove   : {WhatExpr}
            local cTrf        : _
            local fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = Int ->
                  EHCOpts ->
                  ( CPatFld ,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bindOopts | _bindOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _offsetOopts | _offsetOopts `seq` (True) ->
           (case (True) of
            { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
            (case (_lhsIopts) of
             { _fldAnnsOopts | _fldAnnsOopts `seq` (True) ->
             (case (_lhsIlev) of
              { _fldAnnsOlev | _fldAnnsOlev `seq` (True) ->
              (case (fldAnns_ _fldAnnsOlev _fldAnnsOopts ) of
               { ( _fldAnnsIcTrf) | True ->
                   (case (_lhsIlev) of
                    { _bindOlev | _bindOlev `seq` (True) ->
                    (case (EvalCtx_None) of
                     { _bindOevalCtx | _bindOevalCtx `seq` (True) ->
                     (case (False) of
                      { _bindOisLamBody | _bindOisLamBody `seq` (True) ->
                      (case (False) of
                       { _bindOisStrict | _bindOisStrict `seq` (True) ->
                       (case (acoreBindcategPlain) of
                        { _bindOletBindingsCateg | _bindOletBindingsCateg `seq` (True) ->
                        (case (False) of
                         { _bindOisGlobal | _bindOisGlobal `seq` (True) ->
                         (case (bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOopts ) of
                          { ( _bindIcTrf,_bindInm) | True ->
                              (case (offset_ ) of
                               { ( _offsetIwhatBelow,offset_1) | True ->
                                   (case (ExprIsOther) of
                                    { _whatAbove | _whatAbove `seq` (True) ->
                                    (case (_whatAbove) of
                                     { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                     (case (_lhsIlev) of
                                      { _offsetOlev | _offsetOlev `seq` (True) ->
                                      (case (EvalCtx_Eval) of
                                       { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                       (case (False) of
                                        { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                        (case (True) of
                                         { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                         (case (True) of
                                          { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                          (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOopts _offsetOwhatAbove ) of
                                           { ( _offsetIappArgL,_offsetIappLam,_offsetIcTrf) | True ->
                                               (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                                                { _cTrf | _cTrf `seq` (True) ->
                                                (case (_cTrf) of
                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                 (case (_bindInm) of
                                                  { _fldNm | _fldNm `seq` (True) ->
                                                  (case ([_fldNm]) of
                                                   { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                   ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
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
type T_CPatFldL  = Int ->
                   EHCOpts ->
                   ( CPatFldL ,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (_lhsIlev) of
            { _tlOlev | _tlOlev `seq` (True) ->
            (case (tl_ _tlOlev _tlOopts ) of
             { ( _tlIcTrf,_tlIfldNmL) | True ->
                 (case (_lhsIlev) of
                  { _hdOlev | _hdOlev `seq` (True) ->
                  (case (hd_ _hdOlev _hdOopts ) of
                   { ( _hdIcTrf,_hdIfldNmL) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_hdIfldNmL ++ _tlIfldNmL) of
                          { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                          ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlev
       _lhsIopts ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            ( _lhsOcTrf,_lhsOfldNmL) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lev                  : Int
         opts                 : EHCOpts
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
type T_CPatRest  = Int ->
                   EHCOpts ->
                   ( CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPatRest_Empty) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlev
       _lhsIopts ->
         (case (CPatRest_Var nm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
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
type T_CodeAGItf  = EHCOpts ->
                    ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIopts )  =
    (let ( _lhsOcTrf) | True = sem _lhsIopts 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _moduleOopts | _moduleOopts `seq` (True) ->
          (case (cLevModule) of
           { _moduleOlev | _moduleOlev `seq` (True) ->
           (case (module_ _moduleOlev _moduleOopts ) of
            { ( _moduleIcTrf) | True ->
                (case (_moduleIcTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 ( _lhsOcTrf) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
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
type T_MbCExpr  = EvalCtx ->
                  Bool ->
                  Bool ->
                  Int ->
                  EHCOpts ->
                  ( MbCExpr )
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsIopts ->
         (case (_lhsIopts) of
          { _justOopts | _justOopts `seq` (True) ->
          (case (True) of
           { _justOisTopApp | _justOisTopApp `seq` (True) ->
           (case (just_ ) of
            { ( _justIwhatBelow,just_1) | True ->
                (case (ExprIsOther) of
                 { _whatAbove | _whatAbove `seq` (True) ->
                 (case (_whatAbove) of
                  { _justOwhatAbove | _justOwhatAbove `seq` (True) ->
                  (case (_lhsIlev) of
                   { _justOlev | _justOlev `seq` (True) ->
                   (case (_lhsIisStrict) of
                    { _justOisStrict | _justOisStrict `seq` (True) ->
                    (case (_lhsIisLamBody) of
                     { _justOisLamBody | _justOisLamBody `seq` (True) ->
                     (case (_lhsIevalCtx) of
                      { _justOevalCtx | _justOevalCtx `seq` (True) ->
                      (case (True) of
                       { _justOisTopTup | _justOisTopTup `seq` (True) ->
                       (case (just_1 _justOevalCtx _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlev _justOopts _justOwhatAbove ) of
                        { ( _justIappArgL,_justIappLam,_justIcTrf) | True ->
                            (case (Just _justIcTrf) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              ( _lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsIopts ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
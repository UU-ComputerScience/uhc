

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/EtaRed.ag)
module EH101.Core.Trf.EtaRed(cmodTrfEtaRed) where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Core
import EH101.Ty
import EH101.AbstractCore









cmodTrfEtaRed :: CModule -> CModule
cmodTrfEtaRed cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod)) Inh_CodeAGItf
     in   cTrf_Syn_CodeAGItf t



type MbApp = Maybe ((CExpr,FvS),CBound)

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
            local lev         : _
            local cTrf        : _
            local fvS         : _
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
               ( CAlt ,FvS)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev ->
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
                      (case (True) of
                       { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                       (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                        { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                            (case (_lev) of
                             { _patOlev | _patOlev `seq` (True) ->
                             (case (pat_ _patOlev ) of
                              { ( _patIcTrf,_patIfldNmL,_patIfvS,_patInmL) | True ->
                                  (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                                   { _cTrf | _cTrf `seq` (True) ->
                                   (case (_cTrf) of
                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                    (case (_exprIfvS `Set.difference` Set.fromList _patInmL) of
                                     { _fvS | _fvS `seq` (True) ->
                                     (case (_fvS) of
                                      { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                      ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                ( CAltL ,FvS)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (_lhsIisStrict) of
           { _tlOisStrict | _tlOisStrict `seq` (True) ->
           (case (_lhsIisLamBody) of
            { _tlOisLamBody | _tlOisLamBody `seq` (True) ->
            (case (_lhsIevalCtx) of
             { _tlOevalCtx | _tlOevalCtx `seq` (True) ->
             (case (tl_ _tlOevalCtx _tlOisLamBody _tlOisStrict _tlOlev ) of
              { ( _tlIcTrf,_tlIfvS) | True ->
                  (case (_lhsIlev) of
                   { _hdOlev | _hdOlev `seq` (True) ->
                   (case (_lhsIisStrict) of
                    { _hdOisStrict | _hdOisStrict `seq` (True) ->
                    (case (_lhsIisLamBody) of
                     { _hdOisLamBody | _hdOisLamBody `seq` (True) ->
                     (case (_lhsIevalCtx) of
                      { _hdOevalCtx | _hdOevalCtx `seq` (True) ->
                      (case (hd_ _hdOevalCtx _hdOisLamBody _hdOisStrict _hdOlev ) of
                       { ( _hdIcTrf,_hdIfvS) | True ->
                           (case ((:) _hdIcTrf _tlIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (_hdIfvS `Set.union` _tlIfvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
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
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nm                   : HsName
         nmL                  : [HsName]
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
                ( CBind ,FvS,FvSMp,HsName,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev ->
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
                (case (bindAspects_ _bindAspectsOevalCtx _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOnm ) of
                 { ( _bindAspectsIcTrf,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsInmL) | True ->
                     (case (CBind_Bind nm_ _bindAspectsIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_bindAspectsIfvS) of
                        { _lhsOfvS | _lhsOfvS `seq` (True) ->
                        (case (Map.singleton nm_ _bindAspectsIfvS) of
                         { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                         (case (nm_) of
                          { _lhsOnm | _lhsOnm `seq` (True) ->
                          (case ([nm_]) of
                           { _lhsOnmL | _lhsOnmL `seq` (True) ->
                           ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnm,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
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
                   ( CBindAnn ,FvS,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlev ->
         (case (CBindAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOnmL) }) }) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
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
                    ( CBindAnnL ,FvS,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (tl_ _tlOlev ) of
           { ( _tlIcTrf,_tlIfvS,_tlInmL) | True ->
               (case (_lhsIlev) of
                { _hdOlev | _hdOlev `seq` (True) ->
                (case (hd_ _hdOlev ) of
                 { ( _hdIcTrf,_hdIfvS,_hdInmL) | True ->
                     (case ((:) _hdIcTrf _tlIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_hdIfvS `Set.union` _tlIfvS) of
                        { _lhsOfvS | _lhsOfvS `seq` (True) ->
                        (case (_hdInmL ++ _tlInmL) of
                         { _lhsOnmL | _lhsOnmL `seq` (True) ->
                         ( _lhsOcTrf,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOnmL) }) }) }) }))
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
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
                 ( CBindL ,FvS,FvSMp,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev ->
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
               (case (tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev ) of
                { ( _tlIcTrf,_tlIfvS,_tlIfvSMp,_tlInmL) | True ->
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
                          (case (hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOletBindingsCateg _hdOlev ) of
                           { ( _hdIcTrf,_hdIfvS,_hdIfvSMp,_hdInm,_hdInmL) | True ->
                               (case ((:) _hdIcTrf _tlIcTrf) of
                                { _cTrf | _cTrf `seq` (True) ->
                                (case (_cTrf) of
                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                 (case (_hdIfvS `Set.union` _tlIfvS) of
                                  { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                  (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                                   { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                                   (case (_hdInmL ++ _tlInmL) of
                                    { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                    ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }))
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
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
                 ( CBound ,FvS,FvSMp,([HsName]))
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
       _lhsInm ->
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
                     (case (True) of
                      { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                      (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                       { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                           (case (_lhsIlev) of
                            { _bindMetaOlev | _bindMetaOlev `seq` (True) ->
                            (case (bindMeta_ _bindMetaOlev ) of
                             { ( _bindMetaIcTrf,_bindMetaIfvS) | True ->
                                 (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                                  { _cTrf | _cTrf `seq` (True) ->
                                  (case (_cTrf) of
                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                   (case (_bindMetaIfvS `Set.union` _exprIfvS) of
                                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                    (case (Map.empty) of
                                     { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                                     (case ([]) of
                                      { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                      ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
       _lhsInm ->
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
                     (case (True) of
                      { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                      (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                       { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                           (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (_exprIfvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              (case (Map.empty) of
                               { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                               (case ([]) of
                                { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
       _lhsInm ->
         (case (_lhsIlev) of
          { _cmetasOlev | _cmetasOlev `seq` (True) ->
          (case (cmetas_ _cmetasOlev ) of
           { ( _cmetasIcTrf,_cmetasIfvS) | True ->
               (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                { _cTrf | _cTrf `seq` (True) ->
                (case (_cTrf) of
                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                 (case (_cmetasIfvS) of
                  { _lhsOfvS | _lhsOfvS `seq` (True) ->
                  (case (Map.empty) of
                   { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                   (case ([]) of
                    { _lhsOnmL | _lhsOnmL `seq` (True) ->
                    ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }))
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
       _lhsInm ->
         (case (CBound_RelevTy aspectKeyS_ relevTy_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }))
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
       _lhsInm ->
         (case (CBound_Ty aspectKeyS_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }))
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
       _lhsInm ->
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
                  (case (_lhsIisTopApp) of
                   { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                   (case (_lhsIisLamBody) of
                    { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                    (case (_lhsIevalCtx) of
                     { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                     (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                      { _exprOisStrict | _exprOisStrict `seq` (True) ->
                      (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                       { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                           (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (_exprIfvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              (case (Map.empty) of
                               { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                               (case ([]) of
                                { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         fvSMp                : FvSMp
         nmL                  : [HsName]
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
                  ( CBoundL ,FvS,FvSMp,([HsName]))
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
       _lhsInm ->
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
                (case (tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOnm ) of
                 { ( _tlIcTrf,_tlIfvS,_tlIfvSMp,_tlInmL) | True ->
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
                             (case (True) of
                              { _hdOisTopApp | _hdOisTopApp `seq` (True) ->
                              (case (hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOletBindingsCateg _hdOlev _hdOnm ) of
                               { ( _hdIcTrf,_hdIfvS,_hdIfvSMp,_hdInmL) | True ->
                                   (case ((:) _hdIcTrf _tlIcTrf) of
                                    { _cTrf | _cTrf `seq` (True) ->
                                    (case (_cTrf) of
                                     { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                     (case (_hdIfvS `Set.union` _tlIfvS) of
                                      { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                      (case (_hdIfvSMp `Map.union` _tlIfvSMp) of
                                       { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
                                       (case (_hdInmL ++ _tlInmL) of
                                        { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                        ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case (Map.empty) of
             { _lhsOfvSMp | _lhsOfvSMp `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfvS,_lhsOfvSMp,_lhsOnmL) }) }) }) }) }))
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
         whatAbove            : WhatExpr
      synthesized attributes:
         appFunKind           : AppFunKind
         cTrf                 : SELF 
         fvS                  : FvS
         mbApp                : MbApp
         mbFunAppL            : [MbApp]
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
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
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local letBindingsCateg : _
            local isGlobal    : _
            local cTrf        : _
            local fvS         : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
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
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
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
            local whatAbove   : {WhatExpr}
            local lev         : _
            local isTopTup    : _
            local isTopApp    : {Bool}
            local letBindingsCateg : _
            local isGlobal    : _
            local cTrf        : _
            local argNm       : _
            local _tup2       : _
            local cNew        : _
            local fvS         : _
            local isMatch     : _
            local _tup1       : {(MbApp,[MbApp])}
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local evalCtx     : _
            local letBindingsCateg : _
            local isGlobal    : _
            local cTrf        : _
            local fvS         : _
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
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
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
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
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
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local nm          : {HsName}
            local whatBelow   : _
         visit 1:
            local cTrf        : _
            local mbVar       : {Maybe HsName}
            intra nm          : {HsName}
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
                  WhatExpr ->
                  ( AppFunKind,CExpr ,FvS,MbApp,([MbApp]),(Maybe [HsName]),(Maybe HsName))
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
                             _lhsIwhatAbove ->
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
                                      (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                                       { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                                           (case (_exprIappFunKind) of
                                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                                            (case (_lhsIlev) of
                                             { _annOlev | _annOlev `seq` (True) ->
                                             (case (ann_ _annOlev ) of
                                              { ( _annIcTrf,_annIfvS) | True ->
                                                  (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                                                   { _cTrf | _cTrf `seq` (True) ->
                                                   (case (_cTrf) of
                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                    (case (_annIfvS `Set.union` _exprIfvS) of
                                                     { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                     (case (_exprImbApp) of
                                                      { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                      (case (_exprImbFunAppL) of
                                                       { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                       (case (_exprImbLam) of
                                                        { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                        (case (_exprImbVar) of
                                                         { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                         ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                              _lhsIwhatAbove ->
                                (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove) of
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
                                        (case (False) of
                                         { _funcOisTopApp | _funcOisTopApp `seq` (True) ->
                                         (case (func_1 _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOwhatAbove ) of
                                          { ( _funcIappFunKind,_funcIcTrf,_funcIfvS,_funcImbApp,_funcImbFunAppL,_funcImbLam,_funcImbVar) | True ->
                                              (case (_funcIappFunKind) of
                                               { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                                               (case (_lhsIlev) of
                                                { _argOlev | _argOlev `seq` (True) ->
                                                (case (acoreBindcategPlain) of
                                                 { _letBindingsCateg | _letBindingsCateg `seq` (True) ->
                                                 (case (_letBindingsCateg) of
                                                  { _argOletBindingsCateg | _argOletBindingsCateg `seq` (True) ->
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
                                                         (case (True) of
                                                          { _argOisTopApp | _argOisTopApp `seq` (True) ->
                                                          (case (arg_ _argOevalCtx _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOletBindingsCateg _argOlev _argOnm ) of
                                                           { ( _argIcTrf,_argIfvS,_argIfvSMp,_argInmL) | True ->
                                                               (case (CExpr_App _funcIcTrf _argIcTrf) of
                                                                { _cTrf | _cTrf `seq` (True) ->
                                                                (case (_cTrf) of
                                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                 (case (_funcIfvS `Set.union` _argIfvS) of
                                                                  { _fvS | _fvS `seq` (True) ->
                                                                  (case (_fvS) of
                                                                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                   (case (case _funcIappFunKind of
                                                                            AppFunKind_Tag _  -> Nothing
                                                                            AppFunKind_FFI    -> Nothing
                                                                            _                 -> Just ((_funcIcTrf,_funcIfvS),_argIcTrf)) of
                                                                    { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                    (case (_funcImbApp : _funcImbFunAppL) of
                                                                     { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                      (case (Nothing) of
                                                                       { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                       ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _dfltOisTopApp | _dfltOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _dfltOisStrict | _dfltOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _dfltOisLamBody | _dfltOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _dfltOevalCtx | _dfltOevalCtx `seq` (True) ->
                                           (case (dflt_1 _dfltOevalCtx _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlev _dfltOwhatAbove ) of
                                            { ( _dfltIappFunKind,_dfltIcTrf,_dfltIfvS,_dfltImbApp,_dfltImbFunAppL,_dfltImbLam,_dfltImbVar) | True ->
                                                (case (_lhsIlev) of
                                                 { _altsOlev | _altsOlev `seq` (True) ->
                                                 (case (_lhsIisStrict) of
                                                  { _altsOisStrict | _altsOisStrict `seq` (True) ->
                                                  (case (_lhsIisLamBody) of
                                                   { _altsOisLamBody | _altsOisLamBody `seq` (True) ->
                                                   (case (_lhsIevalCtx) of
                                                    { _altsOevalCtx | _altsOevalCtx `seq` (True) ->
                                                    (case (alts_ _altsOevalCtx _altsOisLamBody _altsOisStrict _altsOlev ) of
                                                     { ( _altsIcTrf,_altsIfvS) | True ->
                                                         (case (expr_ ) of
                                                          { ( _exprIwhatBelow,expr_1) | True ->
                                                              (case (_whatAbove) of
                                                               { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                               (case (_lhsIlev) of
                                                                { _exprOlev | _exprOlev `seq` (True) ->
                                                                (case (_isTopTup) of
                                                                 { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                 (case (_isTopApp) of
                                                                  { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                                                  (case (_lhsIisStrict) of
                                                                   { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                   (case (_lhsIisLamBody) of
                                                                    { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                    (case (_lhsIevalCtx) of
                                                                     { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                     (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                                                                      { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                                                                          (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                                                                           { _cTrf | _cTrf `seq` (True) ->
                                                                           (case (_cTrf) of
                                                                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                            (case (_exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS) of
                                                                             { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                             (case (Nothing) of
                                                                              { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                              (case ([]) of
                                                                               { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                               (case (Nothing) of
                                                                                { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                                (case (Nothing) of
                                                                                 { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                                 ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                             _lhsIwhatAbove ->
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
                                    (case (True) of
                                     { _isTopApp | _isTopApp `seq` (True) ->
                                     (case (_isTopApp) of
                                      { _errorExprOisTopApp | _errorExprOisTopApp `seq` (True) ->
                                      (case (_lhsIisStrict) of
                                       { _errorExprOisStrict | _errorExprOisStrict `seq` (True) ->
                                       (case (_lhsIisLamBody) of
                                        { _errorExprOisLamBody | _errorExprOisLamBody `seq` (True) ->
                                        (case (_lhsIevalCtx) of
                                         { _errorExprOevalCtx | _errorExprOevalCtx `seq` (True) ->
                                         (case (errorExpr_1 _errorExprOevalCtx _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlev _errorExprOwhatAbove ) of
                                          { ( _errorExprIappFunKind,_errorExprIcTrf,_errorExprIfvS,_errorExprImbApp,_errorExprImbFunAppL,_errorExprImbLam,_errorExprImbVar) | True ->
                                              (case (_errorExprIappFunKind) of
                                               { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                                               (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                                                { _cTrf | _cTrf `seq` (True) ->
                                                (case (_cTrf) of
                                                 { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                 (case (_errorExprIfvS) of
                                                  { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                  (case (Nothing) of
                                                   { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                   (case ([]) of
                                                    { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                    (case (_errorExprImbLam) of
                                                     { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                     (case (_errorExprImbVar) of
                                                      { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                      ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_Char char_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_CoeArg) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_FFI) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_Hole uid_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                           (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOwhatAbove ) of
                                            { ( _bodyIappFunKind,_bodyIcTrf,_bodyIfvS,_bodyImbApp,_bodyImbFunAppL,_bodyImbLam,_bodyImbVar) | True ->
                                                (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case (_bodyIfvS) of
                                                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                   (case (Nothing) of
                                                    { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                    (case ([]) of
                                                     { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                     (case (Nothing) of
                                                      { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                      (case (Nothing) of
                                                       { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                       ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _funcOisTopApp | _funcOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _funcOisStrict | _funcOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _funcOisLamBody | _funcOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _funcOevalCtx | _funcOevalCtx `seq` (True) ->
                                           (case (func_1 _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOwhatAbove ) of
                                            { ( _funcIappFunKind,_funcIcTrf,_funcIfvS,_funcImbApp,_funcImbFunAppL,_funcImbLam,_funcImbVar) | True ->
                                                (case (CExpr_ImplsApp _funcIcTrf uid_) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case (_funcIfvS) of
                                                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                   (case (Nothing) of
                                                    { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                    (case ([]) of
                                                     { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                     (case (Nothing) of
                                                      { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                      (case (Nothing) of
                                                       { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                       ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                           (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOwhatAbove ) of
                                            { ( _bodyIappFunKind,_bodyIcTrf,_bodyIfvS,_bodyImbApp,_bodyImbFunAppL,_bodyImbLam,_bodyImbVar) | True ->
                                                (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                                                 { _cTrf | _cTrf `seq` (True) ->
                                                 (case (_cTrf) of
                                                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                  (case (_bodyIfvS) of
                                                   { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                   (case (Nothing) of
                                                    { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                    (case ([]) of
                                                     { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                     (case (Nothing) of
                                                      { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                      (case (Nothing) of
                                                       { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                       ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_Int int_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_Integer integer_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                       (case (True) of
                                        { _isTopApp | _isTopApp `seq` (True) ->
                                        (case (_isTopApp) of
                                         { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
                                         (case (_lhsIisStrict) of
                                          { _bodyOisStrict | _bodyOisStrict `seq` (True) ->
                                          (case (_lhsIisLamBody) of
                                           { _bodyOisLamBody | _bodyOisLamBody `seq` (True) ->
                                           (case (_lhsIevalCtx) of
                                            { _bodyOevalCtx | _bodyOevalCtx `seq` (True) ->
                                            (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOwhatAbove ) of
                                             { ( _bodyIappFunKind,_bodyIcTrf,_bodyIfvS,_bodyImbApp,_bodyImbFunAppL,_bodyImbLam,_bodyImbVar) | True ->
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
                                                         (case (bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev ) of
                                                          { ( _bindIcTrf,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL) | True ->
                                                              (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                                                               { _cTrf | _cTrf `seq` (True) ->
                                                               (case (_bindInm) of
                                                                { _argNm | _argNm `seq` (True) ->
                                                                (case (case _bodyImbApp of
                                                                         Just ((f,fFvS),v) | isJust mbV && a == _argNm && not (a `Set.member` fFvS)
                                                                           -> (f,True)
                                                                           where mbV@(~(Just a)) = acoreExprMbVar $ acoreUnBoundVal v
                                                                         _ -> (_cTrf,False)) of
                                                                 { __tup2 | __tup2 `seq` (True) ->
                                                                 (case (__tup2) of
                                                                  { (_cNew,_) | _cNew `seq` (True) ->
                                                                  (case (_cNew) of
                                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                   (case (_argNm `Set.delete` _bodyIfvS) of
                                                                    { _fvS | _fvS `seq` (True) ->
                                                                    (case (_fvS) of
                                                                     { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                     (case (__tup2) of
                                                                      { (_,_isMatch) | _isMatch `seq` (True) ->
                                                                      (case (case _bodyImbFunAppL of
                                                                               (f:fs) | _isMatch
                                                                                 -> (f,fs)
                                                                               _ -> (Nothing,_bodyImbFunAppL)) of
                                                                       { __tup1 | __tup1 `seq` (True) ->
                                                                       (case (__tup1) of
                                                                        { (_lhsOmbApp,_) | _lhsOmbApp `seq` (True) ->
                                                                        (case (__tup1) of
                                                                         { (_,_lhsOmbFunAppL) | _lhsOmbFunAppL `seq` (True) ->
                                                                         (case (Just $ maybe [_argNm] (_argNm:) _bodyImbLam) of
                                                                          { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                          (case (Nothing) of
                                                                           { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                           ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _bodyOisTopApp | _bodyOisTopApp `seq` (True) ->
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
                                            (case (body_1 _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOwhatAbove ) of
                                             { ( _bodyIappFunKind,_bodyIcTrf,_bodyIfvS,_bodyImbApp,_bodyImbFunAppL,_bodyImbLam,_bodyImbVar) | True ->
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
                                                         (case (binds_ _bindsOevalCtx _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOletBindingsCateg _bindsOlev ) of
                                                          { ( _bindsIcTrf,_bindsIfvS,_bindsIfvSMp,_bindsInmL) | True ->
                                                              (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                                                               { _cTrf | _cTrf `seq` (True) ->
                                                               (case (_cTrf) of
                                                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                (case ((_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL) of
                                                                 { _fvS | _fvS `seq` (True) ->
                                                                 (case (_fvS) of
                                                                  { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                  (case (Nothing) of
                                                                   { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                   (case ([]) of
                                                                    { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                     (case (Nothing) of
                                                                      { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                      ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_String str_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_Tag tag_) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                            (case (CExpr_Tup tag_) of
                             { _cTrf | _cTrf `seq` (True) ->
                             (case (_cTrf) of
                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                              (case (Set.empty) of
                               { _lhsOfvS | _lhsOfvS `seq` (True) ->
                               (case (Nothing) of
                                { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                (case ([]) of
                                 { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                 (case (Nothing) of
                                  { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                   ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                           (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOwhatAbove ) of
                                            { ( _offsetIappFunKind,_offsetIcTrf,_offsetIfvS,_offsetImbApp,_offsetImbFunAppL,_offsetImbLam,_offsetImbVar) | True ->
                                                (case (expr_ ) of
                                                 { ( _exprIwhatBelow,expr_1) | True ->
                                                     (case (_whatAbove) of
                                                      { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                      (case (_lhsIlev) of
                                                       { _exprOlev | _exprOlev `seq` (True) ->
                                                       (case (_isTopApp) of
                                                        { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                                        (case (_lhsIisStrict) of
                                                         { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                         (case (_lhsIisLamBody) of
                                                          { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                          (case (_lhsIevalCtx) of
                                                           { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                           (case (False) of
                                                            { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                            (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                                                             { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                                                                 (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                                                                  { _cTrf | _cTrf `seq` (True) ->
                                                                  (case (_cTrf) of
                                                                   { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                   (case (_exprIfvS `Set.union` _offsetIfvS) of
                                                                    { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                    (case (Nothing) of
                                                                     { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                     (case ([]) of
                                                                      { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                      (case (Nothing) of
                                                                       { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                       (case (Nothing) of
                                                                        { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                        ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _fldExprOisTopApp | _fldExprOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _fldExprOisStrict | _fldExprOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _fldExprOisLamBody | _fldExprOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _fldExprOevalCtx | _fldExprOevalCtx `seq` (True) ->
                                           (case (fldExpr_1 _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOwhatAbove ) of
                                            { ( _fldExprIappFunKind,_fldExprIcTrf,_fldExprIfvS,_fldExprImbApp,_fldExprImbFunAppL,_fldExprImbLam,_fldExprImbVar) | True ->
                                                (case (offset_ ) of
                                                 { ( _offsetIwhatBelow,offset_1) | True ->
                                                     (case (_whatAbove) of
                                                      { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                                      (case (_lhsIlev) of
                                                       { _offsetOlev | _offsetOlev `seq` (True) ->
                                                       (case (_isTopTup) of
                                                        { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                                        (case (_isTopApp) of
                                                         { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                                         (case (_lhsIisStrict) of
                                                          { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                                          (case (_lhsIisLamBody) of
                                                           { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                                           (case (_lhsIevalCtx) of
                                                            { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                                            (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOwhatAbove ) of
                                                             { ( _offsetIappFunKind,_offsetIcTrf,_offsetIfvS,_offsetImbApp,_offsetImbFunAppL,_offsetImbLam,_offsetImbVar) | True ->
                                                                 (case (expr_ ) of
                                                                  { ( _exprIwhatBelow,expr_1) | True ->
                                                                      (case (_whatAbove) of
                                                                       { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                                       (case (_lhsIlev) of
                                                                        { _exprOlev | _exprOlev `seq` (True) ->
                                                                        (case (_isTopApp) of
                                                                         { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                            (case (False) of
                                                                             { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                             (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                                                                              { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                                                                                  (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                   { _cTrf | _cTrf `seq` (True) ->
                                                                                   (case (_cTrf) of
                                                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                    (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                                                                     { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                                     (case (Nothing) of
                                                                                      { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                                      (case ([]) of
                                                                                       { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                                       (case (Nothing) of
                                                                                        { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                                        (case (Nothing) of
                                                                                         { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                                         ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                         _lhsIwhatAbove ->
                           (case (AppFunKind_NoApp) of
                            { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
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
                                      (case (True) of
                                       { _isTopApp | _isTopApp `seq` (True) ->
                                       (case (_isTopApp) of
                                        { _fldExprOisTopApp | _fldExprOisTopApp `seq` (True) ->
                                        (case (_lhsIisStrict) of
                                         { _fldExprOisStrict | _fldExprOisStrict `seq` (True) ->
                                         (case (_lhsIisLamBody) of
                                          { _fldExprOisLamBody | _fldExprOisLamBody `seq` (True) ->
                                          (case (_lhsIevalCtx) of
                                           { _fldExprOevalCtx | _fldExprOevalCtx `seq` (True) ->
                                           (case (fldExpr_1 _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOwhatAbove ) of
                                            { ( _fldExprIappFunKind,_fldExprIcTrf,_fldExprIfvS,_fldExprImbApp,_fldExprImbFunAppL,_fldExprImbLam,_fldExprImbVar) | True ->
                                                (case (offset_ ) of
                                                 { ( _offsetIwhatBelow,offset_1) | True ->
                                                     (case (_whatAbove) of
                                                      { _offsetOwhatAbove | _offsetOwhatAbove `seq` (True) ->
                                                      (case (_lhsIlev) of
                                                       { _offsetOlev | _offsetOlev `seq` (True) ->
                                                       (case (_isTopTup) of
                                                        { _offsetOisTopTup | _offsetOisTopTup `seq` (True) ->
                                                        (case (_isTopApp) of
                                                         { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                                         (case (_lhsIisStrict) of
                                                          { _offsetOisStrict | _offsetOisStrict `seq` (True) ->
                                                          (case (_lhsIisLamBody) of
                                                           { _offsetOisLamBody | _offsetOisLamBody `seq` (True) ->
                                                           (case (_lhsIevalCtx) of
                                                            { _offsetOevalCtx | _offsetOevalCtx `seq` (True) ->
                                                            (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOwhatAbove ) of
                                                             { ( _offsetIappFunKind,_offsetIcTrf,_offsetIfvS,_offsetImbApp,_offsetImbFunAppL,_offsetImbLam,_offsetImbVar) | True ->
                                                                 (case (expr_ ) of
                                                                  { ( _exprIwhatBelow,expr_1) | True ->
                                                                      (case (_whatAbove) of
                                                                       { _exprOwhatAbove | _exprOwhatAbove `seq` (True) ->
                                                                       (case (_lhsIlev) of
                                                                        { _exprOlev | _exprOlev `seq` (True) ->
                                                                        (case (_isTopApp) of
                                                                         { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _exprOisStrict | _exprOisStrict `seq` (True) ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _exprOisLamBody | _exprOisLamBody `seq` (True) ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _exprOevalCtx | _exprOevalCtx `seq` (True) ->
                                                                            (case (False) of
                                                                             { _exprOisTopTup | _exprOisTopTup `seq` (True) ->
                                                                             (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                                                                              { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                                                                                  (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                                                                                   { _cTrf | _cTrf `seq` (True) ->
                                                                                   (case (_cTrf) of
                                                                                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                                                                    (case (_exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS) of
                                                                                     { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                                                     (case (Nothing) of
                                                                                      { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                                                                      (case ([]) of
                                                                                       { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                                                                       (case (Nothing) of
                                                                                        { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                                                                        (case (Nothing) of
                                                                                         { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                                                                         ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
                          _lhsIwhatAbove ->
                            (case (AppFunKind_Fun ref_) of
                             { _lhsOappFunKind | _lhsOappFunKind `seq` (True) ->
                             (case (CExpr_Var ref_) of
                              { _cTrf | _cTrf `seq` (True) ->
                              (case (_cTrf) of
                               { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                               (case (Set.singleton _nm) of
                                { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                (case (Nothing) of
                                 { _lhsOmbApp | _lhsOmbApp `seq` (True) ->
                                 (case ([]) of
                                  { _lhsOmbFunAppL | _lhsOmbFunAppL `seq` (True) ->
                                  (case (Nothing) of
                                   { _lhsOmbLam | _lhsOmbLam `seq` (True) ->
                                   (case (Just _nm) of
                                    { _mbVar | _mbVar `seq` (True) ->
                                    (case (_mbVar) of
                                     { _lhsOmbVar | _lhsOmbVar `seq` (True) ->
                                     ( _lhsOappFunKind,_lhsOcTrf,_lhsOfvS,_lhsOmbApp,_lhsOmbFunAppL,_lhsOmbLam,_lhsOmbVar) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Var_1)) of
        { ( sem_CExpr_1) | True ->
        ( _lhsOwhatBelow,sem_CExpr_1) }) }) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                   ( CExprAnn ,FvS)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Debug info_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlev ->
         (case (CExprAnn_Ty ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                    ( CMetaBind ,FvS)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlev ->
         (case (CMetaBind_Apply0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlev ->
         (case (CMetaBind_Function0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlev ->
         (case (CMetaBind_Function1) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlev ->
         (case (CMetaBind_Plain) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                   ( CMetaVal ,FvS)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlev ->
         (case (CMetaVal_Dict) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlev ->
         (case (CMetaVal_DictClass tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlev ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlev ->
         (case (CMetaVal_Track track_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlev ->
         (case (CMetaVal_Val) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                 ( CMetas ,FvS)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _x2Olev | _x2Olev `seq` (True) ->
          (case (x2_ _x2Olev ) of
           { ( _x2IcTrf,_x2IfvS) | True ->
               (case (_lhsIlev) of
                { _x1Olev | _x1Olev `seq` (True) ->
                (case (x1_ _x1Olev ) of
                 { ( _x1IcTrf,_x1IfvS) | True ->
                     (case ((_x1IcTrf,_x2IcTrf)) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_x1IfvS `Set.union` _x2IfvS) of
                        { _lhsOfvS | _lhsOfvS `seq` (True) ->
                        ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                  ( CModule ,FvS)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlev ->
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
                     (case (True) of
                      { _exprOisTopApp | _exprOisTopApp `seq` (True) ->
                      (case (expr_1 _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOwhatAbove ) of
                       { ( _exprIappFunKind,_exprIcTrf,_exprIfvS,_exprImbApp,_exprImbFunAppL,_exprImbLam,_exprImbVar) | True ->
                           (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (_exprIfvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
               ( CPat ,([HsName]),FvS,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlev ->
         (case (CPat_BoolExpr cexpr_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlev ->
         (case (CPat_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _bindsOlev | _bindsOlev `seq` (True) ->
          (case (binds_ _bindsOlev ) of
           { ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsInmL) | True ->
               (case (_lhsIlev) of
                { _restOlev | _restOlev `seq` (True) ->
                (case (rest_ _restOlev ) of
                 { ( _restIcTrf,_restIfvS,_restInmL) | True ->
                     (case (CPat_Con tag_ _restIcTrf _bindsIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_bindsIfldNmL) of
                        { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                        (case (_restIfvS `Set.union` _bindsIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (_restInmL ++ _bindsInmL) of
                          { _lhsOnmL | _lhsOnmL `seq` (True) ->
                          ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlev ->
         (case (CPat_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlev ->
         (case (CPat_Var pnm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([pnm_]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
                  ( CPatFld ,([HsName]),FvS,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _fldAnnsOlev | _fldAnnsOlev `seq` (True) ->
          (case (fldAnns_ _fldAnnsOlev ) of
           { ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsInmL) | True ->
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
                     (case (bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev ) of
                      { ( _bindIcTrf,_bindIfvS,_bindIfvSMp,_bindInm,_bindInmL) | True ->
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
                                      (case (True) of
                                       { _offsetOisTopApp | _offsetOisTopApp `seq` (True) ->
                                       (case (offset_1 _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOwhatAbove ) of
                                        { ( _offsetIappFunKind,_offsetIcTrf,_offsetIfvS,_offsetImbApp,_offsetImbFunAppL,_offsetImbLam,_offsetImbVar) | True ->
                                            (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                                             { _cTrf | _cTrf `seq` (True) ->
                                             (case (_cTrf) of
                                              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                              (case (_bindInm) of
                                               { _fldNm | _fldNm `seq` (True) ->
                                               (case ([_fldNm]) of
                                                { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                                (case (_offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS) of
                                                 { _lhsOfvS | _lhsOfvS `seq` (True) ->
                                                 (case ([_fldNm]) of
                                                  { _lhsOnmL | _lhsOnmL `seq` (True) ->
                                                  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         nmL                  : [HsName]
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
                   ( CPatFldL ,([HsName]),FvS,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlev ->
         (case (_lhsIlev) of
          { _tlOlev | _tlOlev `seq` (True) ->
          (case (tl_ _tlOlev ) of
           { ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlInmL) | True ->
               (case (_lhsIlev) of
                { _hdOlev | _hdOlev `seq` (True) ->
                (case (hd_ _hdOlev ) of
                 { ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdInmL) | True ->
                     (case ((:) _hdIcTrf _tlIcTrf) of
                      { _cTrf | _cTrf `seq` (True) ->
                      (case (_cTrf) of
                       { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                       (case (_hdIfldNmL ++ _tlIfldNmL) of
                        { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                        (case (_hdIfvS `Set.union` _tlIfvS) of
                         { _lhsOfvS | _lhsOfvS `seq` (True) ->
                         (case (_hdInmL ++ _tlInmL) of
                          { _lhsOnmL | _lhsOnmL `seq` (True) ->
                          ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlev ->
         (case ([]) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case ([]) of
            { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
            (case (Set.empty) of
             { _lhsOfvS | _lhsOfvS `seq` (True) ->
             (case ([]) of
              { _lhsOnmL | _lhsOnmL `seq` (True) ->
              ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOnmL) }) }) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         nmL                  : [HsName]
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
                   ( CPatRest ,FvS,([HsName]))
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlev ->
         (case (CPatRest_Empty) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOnmL) }) }) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlev ->
         (case (CPatRest_Var nm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            (case ([nm_]) of
             { _lhsOnmL | _lhsOnmL `seq` (True) ->
             ( _lhsOcTrf,_lhsOfvS,_lhsOnmL) }) }) }) }))
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
    (let ( _lhsOcTrf) | True = sem 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (case (cLevModule) of
     { _moduleOlev | _moduleOlev `seq` (True) ->
     (case (module_ _moduleOlev ) of
      { ( _moduleIcTrf,_moduleIfvS) | True ->
          (case (_moduleIcTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }) })
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
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
                  ( MbCExpr ,FvS)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev ->
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
                     (case (True) of
                      { _justOisTopApp | _justOisTopApp `seq` (True) ->
                      (case (just_1 _justOevalCtx _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlev _justOwhatAbove ) of
                       { ( _justIappFunKind,_justIcTrf,_justIfvS,_justImbApp,_justImbFunAppL,_justImbLam,_justImbVar) | True ->
                           (case (Just _justIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             (case (_justIfvS) of
                              { _lhsOfvS | _lhsOfvS `seq` (True) ->
                              ( _lhsOcTrf,_lhsOfvS) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Set.empty) of
            { _lhsOfvS | _lhsOfvS `seq` (True) ->
            ( _lhsOcTrf,_lhsOfvS) }) }) }))
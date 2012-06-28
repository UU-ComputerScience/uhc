

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/EraseExtractTysigC)
module EH101.Core.Trf.EraseExtractTysigCore(cmodTrfEraseExtractTysigCore) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import EH101.LamInfo
import Data.Maybe
import qualified Data.Map as Map











cmodTrfEraseExtractTysigCore
  :: EHCOpts
     -> CModule
     -> ( CModule
        , LamMp
        )
cmodTrfEraseExtractTysigCore opts cmod
  = ( cTrf_Syn_CodeAGItf t
    , gathLamMp_Syn_CodeAGItf t
    )
  where t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                           (Inh_CodeAGItf
                             { opts_Inh_CodeAGItf = opts
                             , lamMp_Inh_CodeAGItf = Map.empty
                             })

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = LamMp ->
               EHCOpts ->
               ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _exprOlamMp | _exprOlamMp `seq` (True) ->
           (case (expr_ _exprOlamMp _exprOopts ) of
            { ( _exprIcTrf,_exprIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _patOopts | _patOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _patOlamMp | _patOlamMp `seq` (True) ->
                  (case (pat_ _patOlamMp _patOopts ) of
                   { ( _patIcTrf,_patIfldNmL) | True ->
                       (case (CAlt_Alt _patIcTrf _exprIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         ( _lhsOcTrf) }) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
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
type T_CAltL  = LamMp ->
                EHCOpts ->
                ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _tlOlamMp | _tlOlamMp `seq` (True) ->
           (case (tl_ _tlOlamMp _tlOopts ) of
            { ( _tlIcTrf) | True ->
                (case (_lhsIopts) of
                 { _hdOopts | _hdOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp | _hdOlamMp `seq` (True) ->
                  (case (hd_ _hdOlamMp _hdOopts ) of
                   { ( _hdIcTrf) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         ( _lhsOcTrf) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
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
type T_CBind  = LamMp ->
                EHCOpts ->
                ( LamMp,CBind ,HsName)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (nm_) of
          { _bindAspectsOnm | _bindAspectsOnm `seq` (True) ->
          (case (_lhsIopts) of
           { _bindAspectsOopts | _bindAspectsOopts `seq` (True) ->
           (case (_lhsIlamMp) of
            { _bindAspectsOlamMp | _bindAspectsOlamMp `seq` (True) ->
            (case (bindAspects_ _bindAspectsOlamMp _bindAspectsOnm _bindAspectsOopts ) of
             { ( _bindAspectsIbindLamMp,_bindAspectsIcBindTrf,_bindAspectsIcTrf) | True ->
                 (case (_bindAspectsIbindLamMp) of
                  { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
                  (case (CBind_Bind nm_ _bindAspectsIcBindTrf) of
                   { _cTrf | _cTrf `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                    (case (nm_) of
                     { _lhsOnm | _lhsOnm `seq` (True) ->
                     ( _lhsObindLamMp,_lhsOcTrf,_lhsOnm) }) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
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
type T_CBindAnn  = LamMp ->
                   EHCOpts ->
                   ( CBindAnn )
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
type T_CBindAnnL  = LamMp ->
                    EHCOpts ->
                    ( CBindAnnL )
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _tlOlamMp | _tlOlamMp `seq` (True) ->
           (case (tl_ _tlOlamMp _tlOopts ) of
            { ( _tlIcTrf) | True ->
                (case (_lhsIopts) of
                 { _hdOopts | _hdOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp | _hdOlamMp `seq` (True) ->
                  (case (hd_ _hdOlamMp _hdOopts ) of
                   { ( _hdIcTrf) | True ->
                       (case ((:) _hdIcTrf _tlIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         ( _lhsOcTrf) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
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
type T_CBindL  = LamMp ->
                 EHCOpts ->
                 ( LamMp,CBindL )
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _tlOlamMp | _tlOlamMp `seq` (True) ->
           (case (tl_ _tlOlamMp _tlOopts ) of
            { ( _tlIbindLamMp,_tlIcTrf) | True ->
                (case (_lhsIopts) of
                 { _hdOopts | _hdOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp | _hdOlamMp `seq` (True) ->
                  (case (hd_ _hdOlamMp _hdOopts ) of
                   { ( _hdIbindLamMp,_hdIcTrf,_hdInm) | True ->
                       (case (_hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp) of
                        { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
                        (case ((:) _hdIcTrf _tlIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          ( _lhsObindLamMp,_lhsOcTrf) }) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case ([]) of
           { _cTrf | _cTrf `seq` (True) ->
           (case (_cTrf) of
            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
            ( _lhsObindLamMp,_lhsOcTrf) }) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
         cBindTrf             : [CBound]
         cTrf                 : SELF 
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local bindLamL    : _
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
            local bindLamL    : _
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
type T_CBound  = LamMp ->
                 HsName ->
                 EHCOpts ->
                 ( LamMp,([CBound]),CBound )
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case ([]) of
          { _bindLamL | _bindLamL `seq` (True) ->
          (case (Map.singleton _lhsInm (emptyLamInfo {laminfoBindAspMp = Map.fromList _bindLamL})) of
           { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
           (case (_lhsIopts) of
            { _exprOopts | _exprOopts `seq` (True) ->
            (case (_lhsIlamMp) of
             { _exprOlamMp | _exprOlamMp `seq` (True) ->
             (case (expr_ _exprOlamMp _exprOopts ) of
              { ( _exprIcTrf,_exprIgathLamMp) | True ->
                  (case (_lhsIopts) of
                   { _bindMetaOopts | _bindMetaOopts `seq` (True) ->
                   (case (_lhsIlamMp) of
                    { _bindMetaOlamMp | _bindMetaOlamMp `seq` (True) ->
                    (case (bindMeta_ _bindMetaOlamMp _bindMetaOopts ) of
                     { ( _bindMetaIcTrf) | True ->
                         (case (CBound_Bind _bindMetaIcTrf _exprIcTrf) of
                          { _cTrf | _cTrf `seq` (True) ->
                          (case ([_cTrf]) of
                           { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
                           (case (_cTrf) of
                            { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                            ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case (_lhsIopts) of
           { _exprOopts | _exprOopts `seq` (True) ->
           (case (_lhsIlamMp) of
            { _exprOlamMp | _exprOlamMp `seq` (True) ->
            (case (expr_ _exprOlamMp _exprOopts ) of
             { ( _exprIcTrf,_exprIgathLamMp) | True ->
                 (case (CBound_FFE callconv_ expEnt_ _exprIcTrf ty_) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case ([_cTrf]) of
                   { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                    ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case (_lhsIopts) of
           { _cmetasOopts | _cmetasOopts `seq` (True) ->
           (case (_lhsIlamMp) of
            { _cmetasOlamMp | _cmetasOlamMp `seq` (True) ->
            (case (cmetas_ _cmetasOlamMp _cmetasOopts ) of
             { ( _cmetasIcTrf) | True ->
                 (case (CBound_Meta aspectKeyS_ _cmetasIcTrf) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case ([_cTrf]) of
                   { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                    ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case (CBound_RelevTy aspectKeyS_ relevTy_) of
           { _cTrf | _cTrf `seq` (True) ->
           (case ([_cTrf]) of
            { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
            (case (_cTrf) of
             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
             ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case ([ (aspectKeyS_, LamInfoBindAsp_Ty ty_) ]) of
          { _bindLamL | _bindLamL `seq` (True) ->
          (case (Map.singleton _lhsInm (emptyLamInfo {laminfoBindAspMp = Map.fromList _bindLamL})) of
           { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
           (case ([]) of
            { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
            (case (CBound_Ty aspectKeyS_ ty_) of
             { _cTrf | _cTrf `seq` (True) ->
             (case (_cTrf) of
              { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
              ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case (_lhsIopts) of
           { _exprOopts | _exprOopts `seq` (True) ->
           (case (_lhsIlamMp) of
            { _exprOlamMp | _exprOlamMp `seq` (True) ->
            (case (expr_ _exprOlamMp _exprOopts ) of
             { ( _exprIcTrf,_exprIgathLamMp) | True ->
                 (case (CBound_Val aspectKeyS_ _exprIcTrf) of
                  { _cTrf | _cTrf `seq` (True) ->
                  (case ([_cTrf]) of
                   { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
                   (case (_cTrf) of
                    { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                    ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
         cBindTrf             : [CBound]
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
type T_CBoundL  = LamMp ->
                  HsName ->
                  EHCOpts ->
                  ( LamMp,([CBound]),CBoundL )
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (_lhsInm) of
          { _tlOnm | _tlOnm `seq` (True) ->
          (case (_lhsInm) of
           { _hdOnm | _hdOnm `seq` (True) ->
           (case (_lhsIopts) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (_lhsIlamMp) of
             { _tlOlamMp | _tlOlamMp `seq` (True) ->
             (case (tl_ _tlOlamMp _tlOnm _tlOopts ) of
              { ( _tlIbindLamMp,_tlIcBindTrf,_tlIcTrf) | True ->
                  (case (_lhsIopts) of
                   { _hdOopts | _hdOopts `seq` (True) ->
                   (case (_lhsIlamMp) of
                    { _hdOlamMp | _hdOlamMp `seq` (True) ->
                    (case (hd_ _hdOlamMp _hdOnm _hdOopts ) of
                     { ( _hdIbindLamMp,_hdIcBindTrf,_hdIcTrf) | True ->
                         (case (_hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp) of
                          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
                          (case (_hdIcBindTrf ++ _tlIcBindTrf) of
                           { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
                           (case ((:) _hdIcTrf _tlIcTrf) of
                            { _cTrf | _cTrf `seq` (True) ->
                            (case (_cTrf) of
                             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                             ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIlamMp
       _lhsInm
       _lhsIopts ->
         (case (Map.empty) of
          { _lhsObindLamMp | _lhsObindLamMp `seq` (True) ->
          (case ([]) of
           { _lhsOcBindTrf | _lhsOcBindTrf `seq` (True) ->
           (case ([]) of
            { _cTrf | _cTrf `seq` (True) ->
            (case (_cTrf) of
             { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
             ( _lhsObindLamMp,_lhsOcBindTrf,_lhsOcTrf) }) }) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         gathLamMp            : LamMp
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local argNm       : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
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
type T_CExpr  = LamMp ->
                EHCOpts ->
                ( CExpr ,LamMp)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _exprOlamMp | _exprOlamMp `seq` (True) ->
           (case (expr_ _exprOlamMp _exprOopts ) of
            { ( _exprIcTrf,_exprIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _annOopts | _annOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _annOlamMp | _annOlamMp `seq` (True) ->
                  (case (ann_ _annOlamMp _annOopts ) of
                   { ( _annIcTrf) | True ->
                       (case (CExpr_Ann _annIcTrf _exprIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_exprIgathLamMp) of
                          { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                          ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _argOopts | _argOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _argOlamMp | _argOlamMp `seq` (True) ->
           (case (hsnUnknown) of
            { _argOnm | _argOnm `seq` (True) ->
            (case (arg_ _argOlamMp _argOnm _argOopts ) of
             { ( _argIbindLamMp,_argIcBindTrf,_argIcTrf) | True ->
                 (case (_lhsIopts) of
                  { _funcOopts | _funcOopts `seq` (True) ->
                  (case (_lhsIlamMp) of
                   { _funcOlamMp | _funcOlamMp `seq` (True) ->
                   (case (func_ _funcOlamMp _funcOopts ) of
                    { ( _funcIcTrf,_funcIgathLamMp) | True ->
                        (case (CExpr_App _funcIcTrf _argIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (Map.empty) of
                           { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _dfltOopts | _dfltOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _dfltOlamMp | _dfltOlamMp `seq` (True) ->
           (case (dflt_ _dfltOlamMp _dfltOopts ) of
            { ( _dfltIcTrf,_dfltIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _altsOopts | _altsOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _altsOlamMp | _altsOlamMp `seq` (True) ->
                  (case (alts_ _altsOlamMp _altsOopts ) of
                   { ( _altsIcTrf) | True ->
                       (case (_lhsIopts) of
                        { _exprOopts | _exprOopts `seq` (True) ->
                        (case (_lhsIlamMp) of
                         { _exprOlamMp | _exprOlamMp `seq` (True) ->
                         (case (expr_ _exprOlamMp _exprOopts ) of
                          { ( _exprIcTrf,_exprIgathLamMp) | True ->
                              (case (CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (Map.empty) of
                                 { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                                 ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _errorExprOopts | _errorExprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _errorExprOlamMp | _errorExprOlamMp `seq` (True) ->
           (case (errorExpr_ _errorExprOlamMp _errorExprOopts ) of
            { ( _errorExprIcTrf,_errorExprIgathLamMp) | True ->
                (case (CExpr_CaseAltFail failReason_ _errorExprIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_errorExprIgathLamMp) of
                   { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                   ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Char char_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_CoeArg) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_FFI callconv_ safety_ impEnt_ ty_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Hole uid_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bodyOopts | _bodyOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _bodyOlamMp | _bodyOlamMp `seq` (True) ->
           (case (body_ _bodyOlamMp _bodyOopts ) of
            { ( _bodyIcTrf,_bodyIgathLamMp) | True ->
                (case (CExpr_HoleLet bindsUid_ _bodyIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (Map.empty) of
                   { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                   ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _funcOopts | _funcOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _funcOlamMp | _funcOlamMp `seq` (True) ->
           (case (func_ _funcOlamMp _funcOopts ) of
            { ( _funcIcTrf,_funcIgathLamMp) | True ->
                (case (CExpr_ImplsApp _funcIcTrf uid_) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (Map.empty) of
                   { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                   ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bodyOopts | _bodyOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _bodyOlamMp | _bodyOlamMp `seq` (True) ->
           (case (body_ _bodyOlamMp _bodyOopts ) of
            { ( _bodyIcTrf,_bodyIgathLamMp) | True ->
                (case (CExpr_ImplsLam uid_ _bodyIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (Map.empty) of
                   { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                   ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Int int_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Integer integer_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bodyOopts | _bodyOopts `seq` (True) ->
          (case (_lhsIopts) of
           { _bindOopts | _bindOopts `seq` (True) ->
           (case (_lhsIlamMp) of
            { _bindOlamMp | _bindOlamMp `seq` (True) ->
            (case (bind_ _bindOlamMp _bindOopts ) of
             { ( _bindIbindLamMp,_bindIcTrf,_bindInm) | True ->
                 (case (_bindInm) of
                  { _argNm | _argNm `seq` (True) ->
                  (case (Map.delete _argNm _lhsIlamMp) of
                   { _bodyOlamMp | _bodyOlamMp `seq` (True) ->
                   (case (body_ _bodyOlamMp _bodyOopts ) of
                    { ( _bodyIcTrf,_bodyIgathLamMp) | True ->
                        (case (CExpr_Lam _bindIcTrf _bodyIcTrf) of
                         { _cTrf | _cTrf `seq` (True) ->
                         (case (_cTrf) of
                          { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                          (case (Map.empty) of
                           { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                           ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bodyOopts | _bodyOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _bodyOlamMp | _bodyOlamMp `seq` (True) ->
           (case (body_ _bodyOlamMp _bodyOopts ) of
            { ( _bodyIcTrf,_bodyIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _bindsOopts | _bindsOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _bindsOlamMp | _bindsOlamMp `seq` (True) ->
                  (case (binds_ _bindsOlamMp _bindsOopts ) of
                   { ( _bindsIbindLamMp,_bindsIcTrf) | True ->
                       (case (CExpr_Let categ_ _bindsIcTrf _bodyIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (_bindsIbindLamMp `Map.union` _bodyIgathLamMp) of
                          { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                          ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_String str_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Tup tag_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _offsetOopts | _offsetOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _offsetOlamMp | _offsetOlamMp `seq` (True) ->
           (case (offset_ _offsetOlamMp _offsetOopts ) of
            { ( _offsetIcTrf,_offsetIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _exprOopts | _exprOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _exprOlamMp | _exprOlamMp `seq` (True) ->
                  (case (expr_ _exprOlamMp _exprOopts ) of
                   { ( _exprIcTrf,_exprIgathLamMp) | True ->
                       (case (CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf) of
                        { _cTrf | _cTrf `seq` (True) ->
                        (case (_cTrf) of
                         { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                         (case (Map.empty) of
                          { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                          ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _fldExprOopts | _fldExprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _fldExprOlamMp | _fldExprOlamMp `seq` (True) ->
           (case (fldExpr_ _fldExprOlamMp _fldExprOopts ) of
            { ( _fldExprIcTrf,_fldExprIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _offsetOopts | _offsetOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _offsetOlamMp | _offsetOlamMp `seq` (True) ->
                  (case (offset_ _offsetOlamMp _offsetOopts ) of
                   { ( _offsetIcTrf,_offsetIgathLamMp) | True ->
                       (case (_lhsIopts) of
                        { _exprOopts | _exprOopts `seq` (True) ->
                        (case (_lhsIlamMp) of
                         { _exprOlamMp | _exprOlamMp `seq` (True) ->
                         (case (expr_ _exprOlamMp _exprOopts ) of
                          { ( _exprIcTrf,_exprIgathLamMp) | True ->
                              (case (CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (Map.empty) of
                                 { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                                 ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _fldExprOopts | _fldExprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _fldExprOlamMp | _fldExprOlamMp `seq` (True) ->
           (case (fldExpr_ _fldExprOlamMp _fldExprOopts ) of
            { ( _fldExprIcTrf,_fldExprIgathLamMp) | True ->
                (case (_lhsIopts) of
                 { _offsetOopts | _offsetOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _offsetOlamMp | _offsetOlamMp `seq` (True) ->
                  (case (offset_ _offsetOlamMp _offsetOopts ) of
                   { ( _offsetIcTrf,_offsetIgathLamMp) | True ->
                       (case (_lhsIopts) of
                        { _exprOopts | _exprOopts `seq` (True) ->
                        (case (_lhsIlamMp) of
                         { _exprOlamMp | _exprOlamMp `seq` (True) ->
                         (case (expr_ _exprOlamMp _exprOopts ) of
                          { ( _exprIcTrf,_exprIgathLamMp) | True ->
                              (case (CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (Map.empty) of
                                 { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                                 ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExpr_Var ref_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           (case (Map.empty) of
            { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
            ( _lhsOcTrf,_lhsOgathLamMp) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
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
type T_CExprAnn  = LamMp ->
                   EHCOpts ->
                   ( CExprAnn )
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExprAnn_Coe coe_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CExprAnn_Debug info_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
type T_CMetaBind  = LamMp ->
                    EHCOpts ->
                    ( CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaBind_Apply0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaBind_Function0) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaBind_Function1) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
type T_CMetaVal  = LamMp ->
                   EHCOpts ->
                   ( CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaVal_Dict) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaVal_DictClass tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaVal_DictInstance tracks_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CMetaVal_Track track_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
type T_CMetas  = LamMp ->
                 EHCOpts ->
                 ( CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _x2Oopts | _x2Oopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _x2OlamMp | _x2OlamMp `seq` (True) ->
           (case (x2_ _x2OlamMp _x2Oopts ) of
            { ( _x2IcTrf) | True ->
                (case (_lhsIopts) of
                 { _x1Oopts | _x1Oopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _x1OlamMp | _x1OlamMp `seq` (True) ->
                  (case (x1_ _x1OlamMp _x1Oopts ) of
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
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : SELF 
         gathLamMp            : LamMp
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = LamMp ->
                  EHCOpts ->
                  ( CModule ,LamMp)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _exprOopts | _exprOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _exprOlamMp | _exprOlamMp `seq` (True) ->
           (case (expr_ _exprOlamMp _exprOopts ) of
            { ( _exprIcTrf,_exprIgathLamMp) | True ->
                (case (CModule_Mod moduleNm_ _exprIcTrf ctagsMp_) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case (_exprIgathLamMp) of
                   { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                   ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
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
type T_CPat  = LamMp ->
               EHCOpts ->
               ( CPat ,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlamMp
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
    (\ _lhsIlamMp
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
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _bindsOopts | _bindsOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _bindsOlamMp | _bindsOlamMp `seq` (True) ->
           (case (binds_ _bindsOlamMp _bindsOopts ) of
            { ( _bindsIcTrf,_bindsIfldNmL) | True ->
                (case (_lhsIopts) of
                 { _restOopts | _restOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _restOlamMp | _restOlamMp `seq` (True) ->
                  (case (rest_ _restOlamMp _restOopts ) of
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
    (\ _lhsIlamMp
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
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
            local cTrf        : _
            local fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = LamMp ->
                  EHCOpts ->
                  ( CPatFld ,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _fldAnnsOopts | _fldAnnsOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _fldAnnsOlamMp | _fldAnnsOlamMp `seq` (True) ->
           (case (fldAnns_ _fldAnnsOlamMp _fldAnnsOopts ) of
            { ( _fldAnnsIcTrf) | True ->
                (case (_lhsIopts) of
                 { _bindOopts | _bindOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _bindOlamMp | _bindOlamMp `seq` (True) ->
                  (case (bind_ _bindOlamMp _bindOopts ) of
                   { ( _bindIbindLamMp,_bindIcTrf,_bindInm) | True ->
                       (case (_lhsIopts) of
                        { _offsetOopts | _offsetOopts `seq` (True) ->
                        (case (_lhsIlamMp) of
                         { _offsetOlamMp | _offsetOlamMp `seq` (True) ->
                         (case (offset_ _offsetOlamMp _offsetOopts ) of
                          { ( _offsetIcTrf,_offsetIgathLamMp) | True ->
                              (case (CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf) of
                               { _cTrf | _cTrf `seq` (True) ->
                               (case (_cTrf) of
                                { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                                (case (_bindInm) of
                                 { _fldNm | _fldNm `seq` (True) ->
                                 (case ([_fldNm]) of
                                  { _lhsOfldNmL | _lhsOfldNmL `seq` (True) ->
                                  ( _lhsOcTrf,_lhsOfldNmL) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
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
type T_CPatFldL  = LamMp ->
                   EHCOpts ->
                   ( CPatFldL ,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts | _tlOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _tlOlamMp | _tlOlamMp `seq` (True) ->
           (case (tl_ _tlOlamMp _tlOopts ) of
            { ( _tlIcTrf,_tlIfldNmL) | True ->
                (case (_lhsIopts) of
                 { _hdOopts | _hdOopts `seq` (True) ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp | _hdOlamMp `seq` (True) ->
                  (case (hd_ _hdOlamMp _hdOopts ) of
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
    (\ _lhsIlamMp
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
         lamMp                : LamMp
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
type T_CPatRest  = LamMp ->
                   EHCOpts ->
                   ( CPatRest )
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CPatRest_Empty) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (CPatRest_Var nm_) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : CModule 
         gathLamMp            : LamMp
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local howUnionGathLamInfo : _
            local howMergeLamInfo : _
            local gathLamMp   : _
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = LamMp ->
                    EHCOpts ->
                    ( CModule ,LamMp)
data Inh_CodeAGItf  = Inh_CodeAGItf {lamMp_Inh_CodeAGItf :: !(LamMp),opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule ),gathLamMp_Syn_CodeAGItf :: !(LamMp)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIlamMp _lhsIopts )  =
    (let ( _lhsOcTrf,_lhsOgathLamMp) | True = sem _lhsIlamMp _lhsIopts 
     in  (Syn_CodeAGItf _lhsOcTrf _lhsOgathLamMp ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _moduleOopts | _moduleOopts `seq` (True) ->
          (case (id) of
           { _howUnionGathLamInfo | _howUnionGathLamInfo `seq` (True) ->
           (case (_howUnionGathLamInfo _lhsIlamMp) of
            { _moduleOlamMp | _moduleOlamMp `seq` (True) ->
            (case (module_ _moduleOlamMp _moduleOopts ) of
             { ( _moduleIcTrf,_moduleIgathLamMp) | True ->
                 (case (_moduleIcTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  (case ((\(LamInfo {laminfoBindAspMp=m}) i -> i {laminfoBindAspMp = m `Map.union` laminfoBindAspMp i})) of
                   { _howMergeLamInfo | _howMergeLamInfo `seq` (True) ->
                   (case (lamMpMergeInto _howMergeLamInfo const _moduleIgathLamMp _lhsIlamMp) of
                    { _gathLamMp | _gathLamMp `seq` (True) ->
                    (case (_gathLamMp) of
                     { _lhsOgathLamMp | _lhsOgathLamMp `seq` (True) ->
                     ( _lhsOcTrf,_lhsOgathLamMp) }) }) }) }) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
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
type T_MbCExpr  = LamMp ->
                  EHCOpts ->
                  ( MbCExpr )
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (_lhsIopts) of
          { _justOopts | _justOopts `seq` (True) ->
          (case (_lhsIlamMp) of
           { _justOlamMp | _justOlamMp `seq` (True) ->
           (case (just_ _justOlamMp _justOopts ) of
            { ( _justIcTrf,_justIgathLamMp) | True ->
                (case (Just _justIcTrf) of
                 { _cTrf | _cTrf `seq` (True) ->
                 (case (_cTrf) of
                  { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
                  ( _lhsOcTrf) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (case (Nothing) of
          { _cTrf | _cTrf `seq` (True) ->
          (case (_cTrf) of
           { _lhsOcTrf | _lhsOcTrf `seq` (True) ->
           ( _lhsOcTrf) }) }))
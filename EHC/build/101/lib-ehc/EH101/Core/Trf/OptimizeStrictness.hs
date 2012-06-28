

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness)
module EH101.Core.Trf.OptimizeStrictness(cmodTrfOptimizeStrictness) where

import Data.Maybe
import EH101.Base.Common
import EH101.Opts
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import EH.Util.Utils
import EH101.LamInfo
import EH101.Base.Debug
import EH101.Base.Builtin















cmodTrfOptimizeStrictness
  :: EHCOpts
     -> LamMp
     -> CModule
     -> ( CModule
        , LamMp
        )
cmodTrfOptimizeStrictness
     opts
     lamMp
     cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf
                               { opts_Inh_CodeAGItf = opts
                               , lamMp_Inh_CodeAGItf = lamMp
                               })
     in   ( cTrf_Syn_CodeAGItf t
          , gathLamMp_Syn_CodeAGItf t
          )

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         anaEvalCtxt          : AnaEval
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = AnaEval ->
               LamMp ->
               EHCOpts ->
               ( CAlt )
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CAlt 
              _patOlamMp :: LamMp
              _patOopts :: EHCOpts
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _patIcTrf :: CPat 
              _patIfldNmL :: ([HsName])
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 126, column 17)
              _lamArgAnaEvalL =
                  []
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _patOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _patOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _patIcTrf,_patIfldNmL) =
                  pat_ _patOlamMp _patOopts 
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsOcTrf)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         anaEvalCtxt          : AnaEval
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
type T_CAltL  = AnaEval ->
                LamMp ->
                EHCOpts ->
                ( CAltL )
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CAltL 
              _hdOanaEvalCtxt :: AnaEval
              _hdOlamMp :: LamMp
              _hdOopts :: EHCOpts
              _tlOanaEvalCtxt :: AnaEval
              _tlOlamMp :: LamMp
              _tlOopts :: EHCOpts
              _hdIcTrf :: CAlt 
              _tlIcTrf :: CAltL 
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIcTrf) =
                  hd_ _hdOanaEvalCtxt _hdOlamMp _hdOopts 
              ( _tlIcTrf) =
                  tl_ _tlOanaEvalCtxt _tlOlamMp _tlOopts 
          in  ( _lhsOcTrf)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CAltL 
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
            local mbStrictArgsRes : _
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
         (let _lhsObindLamMp :: LamMp
              _lhsOcTrf :: CBind 
              _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _bindAspectsOlamMp :: LamMp
              _bindAspectsOmbStrictArgsRes :: (Maybe (RelevTy,[AnaEval],AnaEval))
              _bindAspectsOopts :: EHCOpts
              _bindAspectsIbindLamMp :: LamMp
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIcTrfBoundL :: ([CBound])
              _bindAspectsIgathStrictTyL :: ([RelevTy])
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 75, column 17)
              _lhsObindLamMp =
                  case _mbStrictArgsRes of
                    Just (t,_,_) -> Map.singleton nm_ (emptyLamInfo {laminfoBindAspMp = Map.fromList l})
                                 where l = [(acbaspkeyStrict, LamInfoBindAsp_RelevTy t)]
                    _            -> Map.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 107, column 17)
              _mbStrictArgsRes =
                  case _bindAspectsIgathStrictTyL of
                    [t@(RelevTy_Fun _ _ _ a r)]
                      -> Just (t, map relevtyAnaEval a, relevtyAnaEval r)
                    _ -> Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 158, column 17)
              _lhsOcTrf =
                  CBind_Bind nm_ _bindAspectsIcTrfBoundL
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 4, column 17)
              _bindAspectsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 12, column 17)
              _lhsOnm =
                  nm_
              -- self rule
              _cTrf =
                  CBind_Bind nm_ _bindAspectsIcTrf
              -- copy rule (down)
              _bindAspectsOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _bindAspectsOmbStrictArgsRes =
                  _mbStrictArgsRes
              -- copy rule (down)
              _bindAspectsOopts =
                  _lhsIopts
              ( _bindAspectsIbindLamMp,_bindAspectsIcTrf,_bindAspectsIcTrfBoundL,_bindAspectsIgathStrictTyL) =
                  bindAspects_ _bindAspectsOlamMp _bindAspectsOmbStrictArgsRes _bindAspectsOnm _bindAspectsOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOnm)))
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
         (let _lhsOcTrf :: CBindAnn 
              -- self rule
              _cTrf =
                  CBindAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
         (let _lhsOcTrf :: CBindAnnL 
              _hdOlamMp :: LamMp
              _hdOopts :: EHCOpts
              _tlOlamMp :: LamMp
              _tlOopts :: EHCOpts
              _hdIcTrf :: CBindAnn 
              _tlIcTrf :: CBindAnnL 
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIcTrf) =
                  hd_ _hdOlamMp _hdOopts 
              ( _tlIcTrf) =
                  tl_ _tlOlamMp _tlOopts 
          in  ( _lhsOcTrf)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CBindAnnL 
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
         (let _lhsObindLamMp :: LamMp
              _lhsOcTrf :: CBindL 
              _hdOlamMp :: LamMp
              _hdOopts :: EHCOpts
              _tlOlamMp :: LamMp
              _tlOopts :: EHCOpts
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBind 
              _hdInm :: HsName
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBindL 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIbindLamMp,_hdIcTrf,_hdInm) =
                  hd_ _hdOlamMp _hdOopts 
              ( _tlIbindLamMp,_tlIcTrf) =
                  tl_ _tlOlamMp _tlOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsObindLamMp :: LamMp
              _lhsOcTrf :: CBindL 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindLamMp,_lhsOcTrf)))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         anaEvalCtxt          : AnaEval
         lamMp                : LamMp
         mbStrictArgsRes      : Maybe (RelevTy,[AnaEval],AnaEval)
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
         cTrf                 : SELF 
         cTrfBoundL           : [CBound]
         gathStrictTyL        : [RelevTy]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local doWorkerWrapper : _
            local _tup1       : {([AnaEval],AnaEval)}
            local wrapper     : _
            local worker      : _
            local oldbinding  : _
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local anaEvalCtxt : {AnaEval}
            local gathStrictTyL : _
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
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
type T_CBound  = AnaEval ->
                 LamMp ->
                 (Maybe (RelevTy,[AnaEval],AnaEval)) ->
                 HsName ->
                 EHCOpts ->
                 ( LamMp,CBound ,([CBound]),([RelevTy]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let __tup1 :: (([AnaEval],AnaEval))
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOanaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              _bindMetaOlamMp :: LamMp
              _bindMetaOopts :: EHCOpts
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _bindMetaIcTrf :: CMetas 
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 116, column 17)
              _doWorkerWrapper =
                  isJust _lhsImbStrictArgsRes
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 121, column 17)
              __tup1 =
                  maybe ([],top) tup123to23 _lhsImbStrictArgsRes
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 121, column 17)
              (_exprOlamArgAnaEvalL,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 121, column 17)
              (_,_exprOanaEvalCtxt) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 136, column 17)
              _wrapper =
                  case (_exprImbLam,_lhsImbStrictArgsRes) of
                    (Just as, Just (_,evs,_))
                      -> [ CBound_Val acbaspkeyNone
                         $ acoreLam wrapperArgs
                         $ (\b -> foldr ($) b bodyWrappers)
                         $ acoreApp (CExpr_Var (ACoreBindRef _lhsInm (Just acbaspkeyStrict)))
                         $ map acoreVar workerArgs
                         ]
                      where (wrapperArgs,workerArgs,bodyWrappers) = unzip3 $ zipWith mk as evs
                            mk a AnaEval_WHNF = (a, a', acoreLet1Strict a' (acoreVar a))
                               where a' = hsnUniqify HsNameUniqifier_Evaluated a
                            mk a _            = (a, a , id                             )
                    _ -> []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 136, column 17)
              _worker =
                  if _doWorkerWrapper then [CBound_Val acbaspkeyStrict _exprIcTrf] else []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 136, column 17)
              _oldbinding =
                  if _doWorkerWrapper then [] else [_cTrf]
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 151, column 17)
              _lhsOcTrfBoundL =
                  _worker ++ _wrapper ++ _oldbinding
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  CBound_Bind _bindMetaIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindMetaOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindMetaOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _bindMetaIcTrf) =
                  bindMeta_ _bindMetaOlamMp _bindMetaOopts 
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 86, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 123, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 152, column 17)
              _lhsOcTrfBoundL =
                  [_cTrf]
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  CBound_FFE callconv_ expEnt_ _exprIcTrf ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (from local)
              _exprOanaEvalCtxt =
                  _anaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              _cmetasOlamMp :: LamMp
              _cmetasOopts :: EHCOpts
              _cmetasIcTrf :: CMetas 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 86, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 123, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 155, column 17)
              _lhsOcTrfBoundL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  CBound_Meta aspectKeyS_ _cmetasIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _cmetasOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _cmetasOopts =
                  _lhsIopts
              ( _cmetasIcTrf) =
                  cmetas_ _cmetasOlamMp _cmetasOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 86, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 100, column 17)
              _gathStrictTyL =
                  if aspectKeyS_ == acbaspkeyStrict
                  then [relevTy_]
                  else []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 123, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 155, column 17)
              _lhsOcTrfBoundL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  _gathStrictTyL
              -- self rule
              _cTrf =
                  CBound_RelevTy aspectKeyS_ relevTy_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 86, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 123, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 155, column 17)
              _lhsOcTrfBoundL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  CBound_Ty aspectKeyS_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrfBoundL :: ([CBound])
              _lhsObindLamMp :: LamMp
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBound 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 86, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 123, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 155, column 17)
              _lhsOcTrfBoundL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  CBound_Val aspectKeyS_ _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (from local)
              _exprOanaEvalCtxt =
                  _anaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         mbStrictArgsRes      : Maybe (RelevTy,[AnaEval],AnaEval)
         nm                   : HsName
         opts                 : EHCOpts
      synthesized attributes:
         bindLamMp            : LamMp
         cTrf                 : SELF 
         cTrfBoundL           : [CBound]
         gathStrictTyL        : [RelevTy]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local anaEvalCtxt : {AnaEval}
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
                  (Maybe (RelevTy,[AnaEval],AnaEval)) ->
                  HsName ->
                  EHCOpts ->
                  ( LamMp,CBoundL ,([CBound]),([RelevTy]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _anaEvalCtxt :: AnaEval
              _lhsObindLamMp :: LamMp
              _lhsOcTrfBoundL :: ([CBound])
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBoundL 
              _hdOanaEvalCtxt :: AnaEval
              _hdOlamMp :: LamMp
              _hdOmbStrictArgsRes :: (Maybe (RelevTy,[AnaEval],AnaEval))
              _hdOnm :: HsName
              _hdOopts :: EHCOpts
              _tlOlamMp :: LamMp
              _tlOmbStrictArgsRes :: (Maybe (RelevTy,[AnaEval],AnaEval))
              _tlOnm :: HsName
              _tlOopts :: EHCOpts
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBound 
              _hdIcTrfBoundL :: ([CBound])
              _hdIgathStrictTyL :: ([RelevTy])
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBoundL 
              _tlIcTrfBoundL :: ([CBound])
              _tlIgathStrictTyL :: ([RelevTy])
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 83, column 17)
              _anaEvalCtxt =
                  top
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 133, column 32)
              _lhsOcTrfBoundL =
                  _hdIcTrfBoundL ++ _tlIcTrfBoundL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  _hdIgathStrictTyL ++ _tlIgathStrictTyL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (from local)
              _hdOanaEvalCtxt =
                  _anaEvalCtxt
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOmbStrictArgsRes =
                  _lhsImbStrictArgsRes
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOmbStrictArgsRes =
                  _lhsImbStrictArgsRes
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIbindLamMp,_hdIcTrf,_hdIcTrfBoundL,_hdIgathStrictTyL) =
                  hd_ _hdOanaEvalCtxt _hdOlamMp _hdOmbStrictArgsRes _hdOnm _hdOopts 
              ( _tlIbindLamMp,_tlIcTrf,_tlIcTrfBoundL,_tlIgathStrictTyL) =
                  tl_ _tlOlamMp _tlOmbStrictArgsRes _tlOnm _tlOopts 
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIlamMp
       _lhsImbStrictArgsRes
       _lhsInm
       _lhsIopts ->
         (let _lhsObindLamMp :: LamMp
              _lhsOcTrfBoundL :: ([CBound])
              _lhsOgathStrictTyL :: ([RelevTy])
              _lhsOcTrf :: CBoundL 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 133, column 32)
              _lhsOcTrfBoundL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 97, column 35)
              _lhsOgathStrictTyL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindLamMp,_lhsOcTrf,_lhsOcTrfBoundL,_lhsOgathStrictTyL)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         anaEvalCtxt          : AnaEval
         lamArgAnaEvalL       : [AnaEval]
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         appArgAnaEvalL       : [AnaEval]
         appFunKind           : AppFunKind
         appResAnaEval        : AnaEval
         cTrf                 : SELF 
         gathLamMp            : LamMp
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
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
            local lamArgAnaEvalL : _
            local _tup2       : {(AnaEval,[AnaEval])}
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local _tup3       : _
            local argAnaEval  : _
            local argNm       : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local lamArgAnaEvalL : _
            local strictRef   : _
            local mbStrict    : _
            local _tup4       : {([AnaEval],AnaEval)}
            local nm          : {HsName}
            local nmAsp       : {HsName}
            local mbVar       : {Maybe HsName}
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
type T_CExpr  = AnaEval ->
                ([AnaEval]) ->
                LamMp ->
                EHCOpts ->
                ( ([AnaEval]),AppFunKind,AnaEval,CExpr ,LamMp,(Maybe [HsName]),(Maybe HsName))
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CExpr 
              _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappFunKind :: AppFunKind
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _annOlamMp :: LamMp
              _annOopts :: EHCOpts
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _annIcTrf :: CExprAnn 
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- self rule
              _cTrf =
                  CExpr_Ann _annIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOappArgAnaEvalL =
                  _exprIappArgAnaEvalL
              -- copy rule (up)
              _lhsOappFunKind =
                  _exprIappFunKind
              -- copy rule (up)
              _lhsOappResAnaEval =
                  _exprIappResAnaEval
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (up)
              _lhsOmbLam =
                  _exprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _exprImbVar
              -- copy rule (down)
              _annOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _annOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (down)
              _exprOlamArgAnaEvalL =
                  _lhsIlamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _annIcTrf) =
                  ann_ _annOlamMp _annOopts 
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _argOmbStrictArgsRes :: (Maybe (RelevTy,[AnaEval],AnaEval))
              __tup2 :: ((AnaEval,[AnaEval]))
              _argOanaEvalCtxt :: AnaEval
              _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOgathLamMp :: LamMp
              _argOnm :: HsName
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _lhsOappResAnaEval :: AnaEval
              _funcOanaEvalCtxt :: AnaEval
              _funcOlamArgAnaEvalL :: ([AnaEval])
              _funcOlamMp :: LamMp
              _funcOopts :: EHCOpts
              _argOlamMp :: LamMp
              _argOopts :: EHCOpts
              _funcIappArgAnaEvalL :: ([AnaEval])
              _funcIappFunKind :: AppFunKind
              _funcIappResAnaEval :: AnaEval
              _funcIcTrf :: CExpr 
              _funcIgathLamMp :: LamMp
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              _argIbindLamMp :: LamMp
              _argIcTrf :: CBound 
              _argIcTrfBoundL :: ([CBound])
              _argIgathStrictTyL :: ([RelevTy])
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 113, column 25)
              _argOmbStrictArgsRes =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 176, column 17)
              __tup2 =
                  hdAndTl' top _funcIappArgAnaEvalL
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 176, column 17)
              (_argOanaEvalCtxt,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 176, column 17)
              (_,_lhsOappArgAnaEvalL) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 7, column 17)
              _argOnm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 14, column 17)
              _lhsOappFunKind =
                  _funcIappFunKind
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_App _funcIcTrf _argIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOappResAnaEval =
                  _funcIappResAnaEval
              -- copy rule (down)
              _funcOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _funcOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              -- copy rule (down)
              _argOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _argOopts =
                  _lhsIopts
              ( _funcIappArgAnaEvalL,_funcIappFunKind,_funcIappResAnaEval,_funcIcTrf,_funcIgathLamMp,_funcImbLam,_funcImbVar) =
                  func_ _funcOanaEvalCtxt _funcOlamArgAnaEvalL _funcOlamMp _funcOopts 
              ( _argIbindLamMp,_argIcTrf,_argIcTrfBoundL,_argIgathStrictTyL) =
                  arg_ _argOanaEvalCtxt _argOlamMp _argOmbStrictArgsRes _argOnm _argOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _altsOanaEvalCtxt :: AnaEval
              _altsOlamMp :: LamMp
              _altsOopts :: EHCOpts
              _dfltOanaEvalCtxt :: AnaEval
              _dfltOlamArgAnaEvalL :: ([AnaEval])
              _dfltOlamMp :: LamMp
              _dfltOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _altsIcTrf :: CAltL 
              _dfltIappArgAnaEvalL :: ([AnaEval])
              _dfltIappFunKind :: AppFunKind
              _dfltIappResAnaEval :: AnaEval
              _dfltIcTrf :: CExpr 
              _dfltIgathLamMp :: LamMp
              _dfltImbLam :: (Maybe [HsName])
              _dfltImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _altsOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (down)
              _altsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _altsOopts =
                  _lhsIopts
              -- copy rule (down)
              _dfltOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _dfltOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _dfltOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _dfltOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
              ( _altsIcTrf) =
                  alts_ _altsOanaEvalCtxt _altsOlamMp _altsOopts 
              ( _dfltIappArgAnaEvalL,_dfltIappFunKind,_dfltIappResAnaEval,_dfltIcTrf,_dfltIgathLamMp,_dfltImbLam,_dfltImbVar) =
                  dflt_ _dfltOanaEvalCtxt _dfltOlamArgAnaEvalL _dfltOlamMp _dfltOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOcTrf :: CExpr 
              _lhsOappFunKind :: AppFunKind
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _errorExprOanaEvalCtxt :: AnaEval
              _errorExprOlamArgAnaEvalL :: ([AnaEval])
              _errorExprOlamMp :: LamMp
              _errorExprOopts :: EHCOpts
              _errorExprIappArgAnaEvalL :: ([AnaEval])
              _errorExprIappFunKind :: AppFunKind
              _errorExprIappResAnaEval :: AnaEval
              _errorExprIcTrf :: CExpr 
              _errorExprIgathLamMp :: LamMp
              _errorExprImbLam :: (Maybe [HsName])
              _errorExprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- self rule
              _cTrf =
                  CExpr_CaseAltFail failReason_ _errorExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOappFunKind =
                  _errorExprIappFunKind
              -- copy rule (up)
              _lhsOgathLamMp =
                  _errorExprIgathLamMp
              -- copy rule (up)
              _lhsOmbLam =
                  _errorExprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _errorExprImbVar
              -- copy rule (down)
              _errorExprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _errorExprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _errorExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _errorExprOopts =
                  _lhsIopts
              ( _errorExprIappArgAnaEvalL,_errorExprIappFunKind,_errorExprIappResAnaEval,_errorExprIcTrf,_errorExprIgathLamMp,_errorExprImbLam,_errorExprImbVar) =
                  errorExpr_ _errorExprOanaEvalCtxt _errorExprOlamArgAnaEvalL _errorExprOlamMp _errorExprOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_CoeArg
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 12, column 17)
              _lhsOappFunKind =
                  AppFunKind_FFI
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_FFI callconv_ safety_ impEnt_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Hole uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _bodyOanaEvalCtxt :: AnaEval
              _bodyOlamArgAnaEvalL :: ([AnaEval])
              _bodyOlamMp :: LamMp
              _bodyOopts :: EHCOpts
              _bodyIappArgAnaEvalL :: ([AnaEval])
              _bodyIappFunKind :: AppFunKind
              _bodyIappResAnaEval :: AnaEval
              _bodyIcTrf :: CExpr 
              _bodyIgathLamMp :: LamMp
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_HoleLet bindsUid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bodyOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _bodyOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              ( _bodyIappArgAnaEvalL,_bodyIappFunKind,_bodyIappResAnaEval,_bodyIcTrf,_bodyIgathLamMp,_bodyImbLam,_bodyImbVar) =
                  body_ _bodyOanaEvalCtxt _bodyOlamArgAnaEvalL _bodyOlamMp _bodyOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _funcOanaEvalCtxt :: AnaEval
              _funcOlamArgAnaEvalL :: ([AnaEval])
              _funcOlamMp :: LamMp
              _funcOopts :: EHCOpts
              _funcIappArgAnaEvalL :: ([AnaEval])
              _funcIappFunKind :: AppFunKind
              _funcIappResAnaEval :: AnaEval
              _funcIcTrf :: CExpr 
              _funcIgathLamMp :: LamMp
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_ImplsApp _funcIcTrf uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _funcOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _funcOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              ( _funcIappArgAnaEvalL,_funcIappFunKind,_funcIappResAnaEval,_funcIcTrf,_funcIgathLamMp,_funcImbLam,_funcImbVar) =
                  func_ _funcOanaEvalCtxt _funcOlamArgAnaEvalL _funcOlamMp _funcOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _bodyOanaEvalCtxt :: AnaEval
              _bodyOlamArgAnaEvalL :: ([AnaEval])
              _bodyOlamMp :: LamMp
              _bodyOopts :: EHCOpts
              _bodyIappArgAnaEvalL :: ([AnaEval])
              _bodyIappFunKind :: AppFunKind
              _bodyIappResAnaEval :: AnaEval
              _bodyIcTrf :: CExpr 
              _bodyIgathLamMp :: LamMp
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_ImplsLam uid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bodyOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _bodyOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              ( _bodyIappArgAnaEvalL,_bodyIappFunKind,_bodyIappResAnaEval,_bodyIcTrf,_bodyIgathLamMp,_bodyImbLam,_bodyImbVar) =
                  body_ _bodyOanaEvalCtxt _bodyOlamArgAnaEvalL _bodyOlamMp _bodyOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Integer integer_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _bodyOlamArgAnaEvalL :: ([AnaEval])
              _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _bodyOlamMp :: LamMp
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _bindOlamMp :: LamMp
              _bindOopts :: EHCOpts
              _bodyOanaEvalCtxt :: AnaEval
              _bodyOopts :: EHCOpts
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindInm :: HsName
              _bodyIappArgAnaEvalL :: ([AnaEval])
              _bodyIappFunKind :: AppFunKind
              _bodyIappResAnaEval :: AnaEval
              _bodyIcTrf :: CExpr 
              _bodyIgathLamMp :: LamMp
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 129, column 17)
              __tup3 =
                  hdAndTl' top _lhsIlamArgAnaEvalL
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 129, column 17)
              (_argAnaEval,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 129, column 17)
              (_,_bodyOlamArgAnaEvalL) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonLamInfo.ag"(line 7, column 17)
              _bodyOlamMp =
                  Map.delete _argNm _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 19, column 25)
              _argNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 4, column 17)
              _lhsOmbLam =
                  Just $ maybe [_argNm] (_argNm:) _bodyImbLam
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Lam _bindIcTrf _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              ( _bindIbindLamMp,_bindIcTrf,_bindInm) =
                  bind_ _bindOlamMp _bindOopts 
              ( _bodyIappArgAnaEvalL,_bodyIappFunKind,_bodyIappResAnaEval,_bodyIcTrf,_bodyIgathLamMp,_bodyImbLam,_bodyImbVar) =
                  body_ _bodyOanaEvalCtxt _bodyOlamArgAnaEvalL _bodyOlamMp _bodyOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _bindsOlamMp :: LamMp
              _bindsOopts :: EHCOpts
              _bodyOanaEvalCtxt :: AnaEval
              _bodyOlamArgAnaEvalL :: ([AnaEval])
              _bodyOlamMp :: LamMp
              _bodyOopts :: EHCOpts
              _bindsIbindLamMp :: LamMp
              _bindsIcTrf :: CBindL 
              _bodyIappArgAnaEvalL :: ([AnaEval])
              _bodyIappFunKind :: AppFunKind
              _bodyIappResAnaEval :: AnaEval
              _bodyIcTrf :: CExpr 
              _bodyIgathLamMp :: LamMp
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 7, column 17)
              _lhsOgathLamMp =
                  _bindsIbindLamMp `Map.union` _bodyIgathLamMp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _bodyOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              ( _bindsIbindLamMp,_bindsIcTrf) =
                  binds_ _bindsOlamMp _bindsOopts 
              ( _bodyIappArgAnaEvalL,_bodyIappFunKind,_bodyIappResAnaEval,_bodyIcTrf,_bodyIgathLamMp,_bodyImbLam,_bodyImbVar) =
                  body_ _bodyOanaEvalCtxt _bodyOlamArgAnaEvalL _bodyOlamMp _bodyOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_String str_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 11, column 17)
              _lhsOappFunKind =
                  AppFunKind_Tag tag_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_Tup tag_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _offsetOanaEvalCtxt :: AnaEval
              _offsetOlamArgAnaEvalL :: ([AnaEval])
              _offsetOlamMp :: LamMp
              _offsetOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _offsetIappArgAnaEvalL :: ([AnaEval])
              _offsetIappFunKind :: AppFunKind
              _offsetIappResAnaEval :: AnaEval
              _offsetIcTrf :: CExpr 
              _offsetIgathLamMp :: LamMp
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _offsetOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _offsetOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
              ( _offsetIappArgAnaEvalL,_offsetIappFunKind,_offsetIappResAnaEval,_offsetIcTrf,_offsetIgathLamMp,_offsetImbLam,_offsetImbVar) =
                  offset_ _offsetOanaEvalCtxt _offsetOlamArgAnaEvalL _offsetOlamMp _offsetOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _offsetOanaEvalCtxt :: AnaEval
              _offsetOlamArgAnaEvalL :: ([AnaEval])
              _offsetOlamMp :: LamMp
              _offsetOopts :: EHCOpts
              _fldExprOanaEvalCtxt :: AnaEval
              _fldExprOlamArgAnaEvalL :: ([AnaEval])
              _fldExprOlamMp :: LamMp
              _fldExprOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _offsetIappArgAnaEvalL :: ([AnaEval])
              _offsetIappFunKind :: AppFunKind
              _offsetIappResAnaEval :: AnaEval
              _offsetIcTrf :: CExpr 
              _offsetIgathLamMp :: LamMp
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _fldExprIappArgAnaEvalL :: ([AnaEval])
              _fldExprIappFunKind :: AppFunKind
              _fldExprIappResAnaEval :: AnaEval
              _fldExprIcTrf :: CExpr 
              _fldExprIgathLamMp :: LamMp
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _offsetOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _offsetOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (down)
              _fldExprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _fldExprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
              ( _offsetIappArgAnaEvalL,_offsetIappFunKind,_offsetIappResAnaEval,_offsetIcTrf,_offsetIgathLamMp,_offsetImbLam,_offsetImbVar) =
                  offset_ _offsetOanaEvalCtxt _offsetOlamArgAnaEvalL _offsetOlamMp _offsetOopts 
              ( _fldExprIappArgAnaEvalL,_fldExprIappFunKind,_fldExprIappResAnaEval,_fldExprIcTrf,_fldExprIgathLamMp,_fldExprImbLam,_fldExprImbVar) =
                  fldExpr_ _fldExprOanaEvalCtxt _fldExprOlamArgAnaEvalL _fldExprOlamMp _fldExprOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsOcTrf :: CExpr 
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _offsetOanaEvalCtxt :: AnaEval
              _offsetOlamArgAnaEvalL :: ([AnaEval])
              _offsetOlamMp :: LamMp
              _offsetOopts :: EHCOpts
              _fldExprOanaEvalCtxt :: AnaEval
              _fldExprOlamArgAnaEvalL :: ([AnaEval])
              _fldExprOlamMp :: LamMp
              _fldExprOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _offsetIappArgAnaEvalL :: ([AnaEval])
              _offsetIappFunKind :: AppFunKind
              _offsetIappResAnaEval :: AnaEval
              _offsetIcTrf :: CExpr 
              _offsetIgathLamMp :: LamMp
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _fldExprIappArgAnaEvalL :: ([AnaEval])
              _fldExprIappFunKind :: AppFunKind
              _fldExprIappResAnaEval :: AnaEval
              _fldExprIcTrf :: CExpr 
              _fldExprIgathLamMp :: LamMp
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 179, column 17)
              _lhsOappResAnaEval =
                  top
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- self rule
              _cTrf =
                  CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _offsetOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _offsetOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (down)
              _fldExprOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _fldExprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
              ( _offsetIappArgAnaEvalL,_offsetIappFunKind,_offsetIappResAnaEval,_offsetIcTrf,_offsetIgathLamMp,_offsetImbLam,_offsetImbVar) =
                  offset_ _offsetOanaEvalCtxt _offsetOlamArgAnaEvalL _offsetOlamMp _offsetOopts 
              ( _fldExprIappArgAnaEvalL,_fldExprIappFunKind,_fldExprIappResAnaEval,_fldExprIcTrf,_fldExprIgathLamMp,_fldExprImbLam,_fldExprImbVar) =
                  fldExpr_ _fldExprOanaEvalCtxt _fldExprOlamArgAnaEvalL _fldExprOlamMp _fldExprOopts 
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamArgAnaEvalL
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CExpr 
              __tup4 :: (([AnaEval],AnaEval))
              _lhsOappArgAnaEvalL :: ([AnaEval])
              _lhsOappResAnaEval :: AnaEval
              _lhsOgathLamMp :: LamMp
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _mbVar :: (Maybe HsName)
              _lhsOmbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 131, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 161, column 17)
              _strictRef =
                  acbrefAspAnd acbaspkeyStrict ref_
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 161, column 17)
              _mbStrict =
                  case fmap libindaspRelevTy $ lamMpLookupAsp2 _strictRef _lhsIlamMp of
                    Just (t@(RelevTy_Fun _ _ _ a r))
                      -> Just (t, map relevtyAnaEval a, relevtyAnaEval r)
                    _ -> Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 168, column 17)
              _lhsOcTrf =
                  CExpr_Var $ maybe ref_ (const _strictRef) _mbStrict
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 174, column 17)
              __tup4 =
                  maybe ([],top) tup123to23 _mbStrict
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 174, column 17)
              (_lhsOappArgAnaEvalL,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 174, column 17)
              (_,_lhsOappResAnaEval) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nm =
                  acbrefNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nmAsp =
                  mkHNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 13, column 17)
              _lhsOappFunKind =
                  AppFunKind_Fun ref_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 21, column 17)
              _mbVar =
                  Just _nm
              -- self rule
              _cTrf =
                  CExpr_Var ref_
              -- copy rule (from local)
              _lhsOmbVar =
                  _mbVar
          in  ( _lhsOappArgAnaEvalL,_lhsOappFunKind,_lhsOappResAnaEval,_lhsOcTrf,_lhsOgathLamMp,_lhsOmbLam,_lhsOmbVar)))
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
         (let _lhsOcTrf :: CExprAnn 
              -- self rule
              _cTrf =
                  CExprAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CExprAnn 
              -- self rule
              _cTrf =
                  CExprAnn_Debug info_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CExprAnn 
              -- self rule
              _cTrf =
                  CExprAnn_Ty ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
         (let _lhsOcTrf :: CMetaBind 
              -- self rule
              _cTrf =
                  CMetaBind_Apply0
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaBind 
              -- self rule
              _cTrf =
                  CMetaBind_Function0
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaBind 
              -- self rule
              _cTrf =
                  CMetaBind_Function1
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaBind 
              -- self rule
              _cTrf =
                  CMetaBind_Plain
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
         (let _lhsOcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  CMetaVal_Dict
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  CMetaVal_DictClass tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  CMetaVal_Track track_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  CMetaVal_Val
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
         (let _lhsOcTrf :: CMetas 
              _x1OlamMp :: LamMp
              _x1Oopts :: EHCOpts
              _x2OlamMp :: LamMp
              _x2Oopts :: EHCOpts
              _x1IcTrf :: CMetaBind 
              _x2IcTrf :: CMetaVal 
              -- self rule
              _cTrf =
                  (_x1IcTrf,_x2IcTrf)
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _x1OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x1Oopts =
                  _lhsIopts
              -- copy rule (down)
              _x2OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x2Oopts =
                  _lhsIopts
              ( _x1IcTrf) =
                  x1_ _x1OlamMp _x1Oopts 
              ( _x2IcTrf) =
                  x2_ _x2OlamMp _x2Oopts 
          in  ( _lhsOcTrf)))
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
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
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
         (let _anaEvalCtxt :: AnaEval
              _lhsOcTrf :: CModule 
              _lhsOgathLamMp :: LamMp
              _exprOanaEvalCtxt :: AnaEval
              _exprOlamArgAnaEvalL :: ([AnaEval])
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprIappArgAnaEvalL :: ([AnaEval])
              _exprIappFunKind :: AppFunKind
              _exprIappResAnaEval :: AnaEval
              _exprIcTrf :: CExpr 
              _exprIgathLamMp :: LamMp
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 89, column 17)
              _anaEvalCtxt =
                  top
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 126, column 17)
              _lamArgAnaEvalL =
                  []
              -- self rule
              _cTrf =
                  CModule_Mod moduleNm_ _exprIcTrf ctagsMp_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (from local)
              _exprOanaEvalCtxt =
                  _anaEvalCtxt
              -- copy rule (from local)
              _exprOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              ( _exprIappArgAnaEvalL,_exprIappFunKind,_exprIappResAnaEval,_exprIcTrf,_exprIgathLamMp,_exprImbLam,_exprImbVar) =
                  expr_ _exprOanaEvalCtxt _exprOlamArgAnaEvalL _exprOlamMp _exprOopts 
          in  ( _lhsOcTrf,_lhsOgathLamMp)))
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
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_BoolExpr cexpr_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _restOlamMp :: LamMp
              _restOopts :: EHCOpts
              _bindsOlamMp :: LamMp
              _bindsOopts :: EHCOpts
              _restIcTrf :: CPatRest 
              _bindsIcTrf :: CPatFldL 
              _bindsIfldNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _bindsIfldNmL
              -- self rule
              _cTrf =
                  CPat_Con tag_ _restIcTrf _bindsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _restOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _restOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              ( _restIcTrf) =
                  rest_ _restOlamMp _restOopts 
              ( _bindsIcTrf,_bindsIfldNmL) =
                  binds_ _bindsOlamMp _bindsOopts 
          in  ( _lhsOcTrf,_lhsOfldNmL)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Var pnm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL)))
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
            local anaEvalCtxt : {AnaEval}
            local lamArgAnaEvalL : _
            local fldNm       : _
            local cTrf        : _
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
         (let _anaEvalCtxt :: AnaEval
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPatFld 
              _offsetOanaEvalCtxt :: AnaEval
              _offsetOlamArgAnaEvalL :: ([AnaEval])
              _offsetOlamMp :: LamMp
              _offsetOopts :: EHCOpts
              _bindOlamMp :: LamMp
              _bindOopts :: EHCOpts
              _fldAnnsOlamMp :: LamMp
              _fldAnnsOopts :: EHCOpts
              _offsetIappArgAnaEvalL :: ([AnaEval])
              _offsetIappFunKind :: AppFunKind
              _offsetIappResAnaEval :: AnaEval
              _offsetIcTrf :: CExpr 
              _offsetIgathLamMp :: LamMp
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindInm :: HsName
              _fldAnnsIcTrf :: CBindAnnL 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 92, column 17)
              _anaEvalCtxt =
                  bot
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 126, column 17)
              _lamArgAnaEvalL =
                  []
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 23, column 17)
              _fldNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 28, column 17)
              _lhsOfldNmL =
                  [_fldNm]
              -- self rule
              _cTrf =
                  CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (from local)
              _offsetOanaEvalCtxt =
                  _anaEvalCtxt
              -- copy rule (from local)
              _offsetOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (down)
              _fldAnnsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldAnnsOopts =
                  _lhsIopts
              ( _offsetIappArgAnaEvalL,_offsetIappFunKind,_offsetIappResAnaEval,_offsetIcTrf,_offsetIgathLamMp,_offsetImbLam,_offsetImbVar) =
                  offset_ _offsetOanaEvalCtxt _offsetOlamArgAnaEvalL _offsetOlamMp _offsetOopts 
              ( _bindIbindLamMp,_bindIcTrf,_bindInm) =
                  bind_ _bindOlamMp _bindOopts 
              ( _fldAnnsIcTrf) =
                  fldAnns_ _fldAnnsOlamMp _fldAnnsOopts 
          in  ( _lhsOcTrf,_lhsOfldNmL)))
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
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _hdOlamMp :: LamMp
              _hdOopts :: EHCOpts
              _tlOlamMp :: LamMp
              _tlOopts :: EHCOpts
              _hdIcTrf :: CPatFld 
              _hdIfldNmL :: ([HsName])
              _tlIcTrf :: CPatFldL 
              _tlIfldNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _hdIfldNmL ++ _tlIfldNmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIcTrf,_hdIfldNmL) =
                  hd_ _hdOlamMp _hdOopts 
              ( _tlIcTrf,_tlIfldNmL) =
                  tl_ _tlOlamMp _tlOopts 
          in  ( _lhsOcTrf,_lhsOfldNmL)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL)))
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
         (let _lhsOcTrf :: CPatRest 
              -- self rule
              _cTrf =
                  CPatRest_Empty
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: CPatRest 
              -- self rule
              _cTrf =
                  CPatRest_Var nm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
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
    (let ( _lhsOcTrf,_lhsOgathLamMp) = sem _lhsIlamMp _lhsIopts 
     in  (Syn_CodeAGItf _lhsOcTrf _lhsOgathLamMp ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _moduleOlamMp :: LamMp
              _lhsOcTrf :: CModule 
              _lhsOgathLamMp :: LamMp
              _moduleOopts :: EHCOpts
              _moduleIcTrf :: CModule 
              _moduleIgathLamMp :: LamMp
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 68, column 17)
              _howUnionGathLamInfo =
                  Map.union _gathLamMp
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 72, column 17)
              _howMergeLamInfo =
                  (\(LamInfo {laminfoBindAspMp=m}) i -> i {laminfoBindAspMp = m `Map.union` laminfoBindAspMp i})
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 15, column 17)
              _gathLamMp =
                  lamMpMergeInto _howMergeLamInfo const _moduleIgathLamMp _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 16, column 17)
              _moduleOlamMp =
                  _howUnionGathLamInfo _lhsIlamMp
              -- copy rule (up)
              _lhsOcTrf =
                  _moduleIcTrf
              -- copy rule (from local)
              _lhsOgathLamMp =
                  _gathLamMp
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              ( _moduleIcTrf,_moduleIgathLamMp) =
                  module_ _moduleOlamMp _moduleOopts 
          in  ( _lhsOcTrf,_lhsOgathLamMp)))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         anaEvalCtxt          : AnaEval
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attribute:
         cTrf                 : SELF 
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local lamArgAnaEvalL : _
            local cTrf        : _
      alternative Nothing:
         visit 0:
            local lamArgAnaEvalL : _
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
type T_MbCExpr  = AnaEval ->
                  LamMp ->
                  EHCOpts ->
                  ( MbCExpr )
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: MbCExpr 
              _justOanaEvalCtxt :: AnaEval
              _justOlamArgAnaEvalL :: ([AnaEval])
              _justOlamMp :: LamMp
              _justOopts :: EHCOpts
              _justIappArgAnaEvalL :: ([AnaEval])
              _justIappFunKind :: AppFunKind
              _justIappResAnaEval :: AnaEval
              _justIcTrf :: CExpr 
              _justIgathLamMp :: LamMp
              _justImbLam :: (Maybe [HsName])
              _justImbVar :: (Maybe HsName)
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 126, column 17)
              _lamArgAnaEvalL =
                  []
              -- self rule
              _cTrf =
                  Just _justIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _justOanaEvalCtxt =
                  _lhsIanaEvalCtxt
              -- copy rule (from local)
              _justOlamArgAnaEvalL =
                  _lamArgAnaEvalL
              -- copy rule (down)
              _justOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              ( _justIappArgAnaEvalL,_justIappFunKind,_justIappResAnaEval,_justIcTrf,_justIgathLamMp,_justImbLam,_justImbVar) =
                  just_ _justOanaEvalCtxt _justOlamArgAnaEvalL _justOlamMp _justOopts 
          in  ( _lhsOcTrf)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIanaEvalCtxt
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOcTrf :: MbCExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/OptimizeStrictness.ag"(line 126, column 17)
              _lamArgAnaEvalL =
                  []
              -- self rule
              _cTrf =
                  Nothing
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf)))
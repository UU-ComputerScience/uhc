

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Trf/EvalElim.ag)
module EH101.GrinCode.Trf.EvalElim(grEvalElim) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import EH101.Base.Builtin
import EH101.Base.Target
import EH101.Base.Common
import EH101.Opts
import EH101.GrinCode.Common
import EH101.GrinCode
import qualified EH101.Config as Cfg
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Debug















grEvalElim :: EHCOpts -> GrModule -> GrModule
grEvalElim opts grmod
  = trf_Syn_GrAGItf t
  where t = wrap_GrAGItf (sem_GrAGItf $ GrAGItf_AGItf grmod)
            $ (Inh_GrAGItf
                 { opts_Inh_GrAGItf = opts
                 })



optsAllowNodePtrMix :: EHCOpts -> Bool
optsAllowNodePtrMix opts = targetIsGrinBytecode (ehcOptTarget opts)



data IsEval
  = IsEval_EvalToNode                       -- eval to node
  | IsEval_EvalToNodeNm !HsName             -- eval to node, but under different name
  | IsEval_EvalToPointer
  | IsEval_Apply        !HsName !GrValL     -- eval to apply, reconstruct with name and argument values
  | IsEval_None
  deriving Eq

isEvalEvaluated :: EHCOpts -> IsEval -> Bool
isEvalEvaluated opts IsEval_None          = False
isEvalEvaluated opts IsEval_EvalToPointer = optsAllowNodePtrMix opts
isEvalEvaluated opts _                    = True

type IsEvalMp = Map.Map HsName IsEval -- (IsEval,HsName)

isEvalNm :: IsEval -> HsName -> HsName
isEvalNm (IsEval_EvalToNodeNm n) _ = n
isEvalNm _                       n = n



instance Show IsEval where
  show IsEval_EvalToNode        = "IsEval_EvalToNode"
  show (IsEval_EvalToNodeNm n)  = "IsEval_EvalToNodeNm" ++ show n
  show IsEval_EvalToPointer     = "IsEval_EvalToPointer"
  show IsEval_None              = "IsEval_None"
  show (IsEval_Apply f _)       = "IsEval_Apply " ++ show f



type MkSeq    = GrExpr -> GrExpr
type MkPatSeq = GrPatLam -> MkSeq

emptyMkPatSeq :: MkPatSeq
emptyMkPatSeq _ e = e

mkPatSeq :: GrExpr -> MkPatSeq
mkPatSeq = GrExpr_Seq

mkMbPatSeq :: GrExpr -> Maybe MkPatSeq
mkMbPatSeq = Just . mkPatSeq

mkSeq :: HsName -> GrExpr -> MkSeq
mkSeq n e = mkPatSeq e (GrPatLam_Var n)



type Delayed x = (x,IsEval,IsEvalMp)
type DelayedExpr = Delayed GrExpr
type DelayedEval = Delayed MkSeq
type DelayedEvalMp = Map.Map HsName DelayedEval

del2del :: (x -> y) -> Delayed x -> Delayed y
del2del f (x,i,m) = (f x,i,m)

-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attribute:
         trf                  : GrModule 
   alternatives:
      alternative AGItf:
         child module         : GrModule 
-}
-- cata
sem_GrAGItf :: GrAGItf  ->
               T_GrAGItf 
sem_GrAGItf (GrAGItf_AGItf _module )  =
    (sem_GrAGItf_AGItf (sem_GrModule _module ) )
-- semantic domain
type T_GrAGItf  = EHCOpts ->
                  ( GrModule )
data Inh_GrAGItf  = Inh_GrAGItf {opts_Inh_GrAGItf :: !(EHCOpts)}
data Syn_GrAGItf  = Syn_GrAGItf {trf_Syn_GrAGItf :: !(GrModule )}
wrap_GrAGItf :: T_GrAGItf  ->
                Inh_GrAGItf  ->
                Syn_GrAGItf 
wrap_GrAGItf sem (Inh_GrAGItf _lhsIopts )  =
    (let ( _lhsOtrf) = sem _lhsIopts 
     in  (Syn_GrAGItf _lhsOtrf ))
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _moduleOopts ->
          (case (module_ _moduleOopts ) of
           { ( _moduleItrf) ->
               (case (_moduleItrf) of
                { _lhsOtrf ->
                ( _lhsOtrf) }) }) }))
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Del:
         child off            : GrVal 
         visit 1:
            local willUseFor  : _
            local trf         : _
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
         visit 1:
            local willUseFor  : _
            local trf         : _
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
         visit 1:
            local willUseFor  : _
            local trf         : _
-}
-- cata
sem_GrAdapt :: GrAdapt  ->
               T_GrAdapt 
sem_GrAdapt (GrAdapt_Del _off )  =
    (sem_GrAdapt_Del (sem_GrVal _off ) )
sem_GrAdapt (GrAdapt_Ins _off _val )  =
    (sem_GrAdapt_Ins (sem_GrVal _off ) (sem_GrVal _val ) )
sem_GrAdapt (GrAdapt_Upd _off _val )  =
    (sem_GrAdapt_Upd (sem_GrVal _off ) (sem_GrVal _val ) )
-- semantic domain
type T_GrAdapt  = ( FvInfoMp,T_GrAdapt_1 )
type T_GrAdapt_1  = IsEvalMp ->
                    EHCOpts ->
                    ( GrAdapt )
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (case (off_ ) of
     { ( _offIgathFviMp,off_1) ->
         (case (_offIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ((let sem_GrAdapt_Del_1 :: T_GrAdapt_1 
                      sem_GrAdapt_Del_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts ->
                               (case (Set.empty) of
                                { _willUseFor ->
                                (case (_willUseFor) of
                                 { _offOwillUseFor ->
                                 (case (_lhsIopts) of
                                  { _offOopts ->
                                  (case (_lhsIisEvalMp) of
                                   { _offOisEvalMp ->
                                   (case (off_1 _offOisEvalMp _offOopts _offOwillUseFor ) of
                                    { ( _offIgathIsEvalMp,_offIisEval,_offImbDelayedExpr,_offImbGrExpr,_offInmAlias,_offItrf) ->
                                        (case (GrAdapt_Del _offItrf) of
                                         { _trf ->
                                         (case (_trf) of
                                          { _lhsOtrf ->
                                          ( _lhsOtrf) }) }) }) }) }) }) }))
                  in  sem_GrAdapt_Del_1)) of
           { ( sem_GrAdapt_1) ->
           ( _lhsOgathFviMp,sem_GrAdapt_1) }) }) })
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp,val_1) ->
         (case (off_ ) of
          { ( _offIgathFviMp,off_1) ->
              (case (_offIgathFviMp `fviMpUnion` _valIgathFviMp) of
               { _lhsOgathFviMp ->
               (case ((let sem_GrAdapt_Ins_1 :: T_GrAdapt_1 
                           sem_GrAdapt_Ins_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (Set.empty) of
                                     { _willUseFor ->
                                     (case (_willUseFor) of
                                      { _valOwillUseFor ->
                                      (case (_lhsIopts) of
                                       { _valOopts ->
                                       (case (_lhsIisEvalMp) of
                                        { _valOisEvalMp ->
                                        (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                         { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                             (case (_willUseFor) of
                                              { _offOwillUseFor ->
                                              (case (_lhsIopts) of
                                               { _offOopts ->
                                               (case (_lhsIisEvalMp) of
                                                { _offOisEvalMp ->
                                                (case (off_1 _offOisEvalMp _offOopts _offOwillUseFor ) of
                                                 { ( _offIgathIsEvalMp,_offIisEval,_offImbDelayedExpr,_offImbGrExpr,_offInmAlias,_offItrf) ->
                                                     (case (GrAdapt_Ins _offItrf _valItrf) of
                                                      { _trf ->
                                                      (case (_trf) of
                                                       { _lhsOtrf ->
                                                       ( _lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                       in  sem_GrAdapt_Ins_1)) of
                { ( sem_GrAdapt_1) ->
                ( _lhsOgathFviMp,sem_GrAdapt_1) }) }) }) })
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (case (val_ ) of
     { ( _valIgathFviMp,val_1) ->
         (case (off_ ) of
          { ( _offIgathFviMp,off_1) ->
              (case (_offIgathFviMp `fviMpUnion` _valIgathFviMp) of
               { _lhsOgathFviMp ->
               (case ((let sem_GrAdapt_Upd_1 :: T_GrAdapt_1 
                           sem_GrAdapt_Upd_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (Set.empty) of
                                     { _willUseFor ->
                                     (case (_willUseFor) of
                                      { _valOwillUseFor ->
                                      (case (_lhsIopts) of
                                       { _valOopts ->
                                       (case (_lhsIisEvalMp) of
                                        { _valOisEvalMp ->
                                        (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                         { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                             (case (_willUseFor) of
                                              { _offOwillUseFor ->
                                              (case (_lhsIopts) of
                                               { _offOopts ->
                                               (case (_lhsIisEvalMp) of
                                                { _offOisEvalMp ->
                                                (case (off_1 _offOisEvalMp _offOopts _offOwillUseFor ) of
                                                 { ( _offIgathIsEvalMp,_offIisEval,_offImbDelayedExpr,_offImbGrExpr,_offInmAlias,_offItrf) ->
                                                     (case (GrAdapt_Upd _offItrf _valItrf) of
                                                      { _trf ->
                                                      (case (_trf) of
                                                       { _lhsOtrf ->
                                                       ( _lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                       in  sem_GrAdapt_Upd_1)) of
                { ( sem_GrAdapt_1) ->
                ( _lhsOgathFviMp,sem_GrAdapt_1) }) }) }) })
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = ( FvInfoMp,T_GrAdaptL_1 )
type T_GrAdaptL_1  = IsEvalMp ->
                     EHCOpts ->
                     ( GrAdaptL )
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp,tl_1) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,hd_1) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               (case ((let sem_GrAdaptL_Cons_1 :: T_GrAdaptL_1 
                           sem_GrAdaptL_Cons_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (_lhsIopts) of
                                     { _tlOopts ->
                                     (case (_lhsIisEvalMp) of
                                      { _tlOisEvalMp ->
                                      (case (tl_1 _tlOisEvalMp _tlOopts ) of
                                       { ( _tlItrf) ->
                                           (case (_lhsIopts) of
                                            { _hdOopts ->
                                            (case (_lhsIisEvalMp) of
                                             { _hdOisEvalMp ->
                                             (case (hd_1 _hdOisEvalMp _hdOopts ) of
                                              { ( _hdItrf) ->
                                                  (case ((:) _hdItrf _tlItrf) of
                                                   { _trf ->
                                                   (case (_trf) of
                                                    { _lhsOtrf ->
                                                    ( _lhsOtrf) }) }) }) }) }) }) }) }))
                       in  sem_GrAdaptL_Cons_1)) of
                { ( sem_GrAdaptL_1) ->
                ( _lhsOgathFviMp,sem_GrAdaptL_1) }) }) }) })
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrAdaptL_Nil_1 :: T_GrAdaptL_1 
                 sem_GrAdaptL_Nil_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case ([]) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrAdaptL_Nil_1)) of
      { ( sem_GrAdaptL_1) ->
      ( _lhsOgathFviMp,sem_GrAdaptL_1) }) })
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         willUseForMp         : WillUseForMp
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      chained attribute:
         delayedEvalMp        : DelayedEvalMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
         visit 1:
            local trf         : _
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = ( FvInfoMp,WillUseForMp,T_GrAlt_1 )
type T_GrAlt_1  = DelayedEvalMp ->
                  IsEvalMp ->
                  EHCOpts ->
                  ( DelayedEvalMp,GrAlt )
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (case (expr_ ) of
     { ( _exprIalsoWillUseForMp,_exprIgathFviMp,_exprIwillUseForMp,expr_1) ->
         (case (pat_ ) of
          { ( _patIintroNmL,pat_1) ->
              (case (_exprIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL) of
               { _lhsOgathFviMp ->
               (case (_exprIwillUseForMp) of
                { _lhsOwillUseForMp ->
                (case ((let sem_GrAlt_Alt_1 :: T_GrAlt_1 
                            sem_GrAlt_Alt_1  =
                                (\ _lhsIdelayedEvalMp
                                   _lhsIisEvalMp
                                   _lhsIopts ->
                                     (case (_lhsIdelayedEvalMp) of
                                      { _exprOdelayedEvalMp ->
                                      (case (_lhsIopts) of
                                       { _exprOopts ->
                                       (case (Set.empty) of
                                        { _exprOwillUseFor ->
                                        (case (_exprIgathFviMp) of
                                         { _exprOfviMp ->
                                         (case (Nothing) of
                                          { _exprOmbIntroNm ->
                                          (case (_lhsIisEvalMp `Map.difference` Map.fromList (zip _patIintroNmL _patIintroNmL)) of
                                           { _exprOisEvalMp ->
                                           (case (expr_1 _exprOdelayedEvalMp _exprOfviMp _exprOisEvalMp _exprOmbIntroNm _exprOopts _exprOwillUseFor ) of
                                            { ( _exprIdelayedEvalMp,_exprIgathIsEvalMp,_exprIhasSideEffect,_exprIisEval,_exprImbDelayedExpr,_exprImbMkPatSeq,_exprInmAlias,_exprItrf) ->
                                                (case (_exprIdelayedEvalMp) of
                                                 { _lhsOdelayedEvalMp ->
                                                 (case (_lhsIopts) of
                                                  { _patOopts ->
                                                  (case (_lhsIisEvalMp) of
                                                   { _patOisEvalMp ->
                                                   (case (pat_1 _patOisEvalMp _patOopts ) of
                                                    { ( _patIgathFviMp,_patInmAlias,_patItrf) ->
                                                        (case (GrAlt_Alt ann_ _patItrf _exprItrf) of
                                                         { _trf ->
                                                         (case (_trf) of
                                                          { _lhsOtrf ->
                                                          ( _lhsOdelayedEvalMp,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
                        in  sem_GrAlt_Alt_1)) of
                 { ( sem_GrAlt_1) ->
                 ( _lhsOgathFviMp,_lhsOwillUseForMp,sem_GrAlt_1) }) }) }) }) })
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         willUseForMpL        : [WillUseForMp]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      chained attribute:
         delayedEvalMp        : DelayedEvalMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = ( FvInfoMp,([WillUseForMp]),T_GrAltL_1 )
type T_GrAltL_1  = DelayedEvalMp ->
                   IsEvalMp ->
                   EHCOpts ->
                   ( DelayedEvalMp,GrAltL )
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp,_tlIwillUseForMpL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,_hdIwillUseForMp,hd_1) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               (case (_hdIwillUseForMp : _tlIwillUseForMpL) of
                { _lhsOwillUseForMpL ->
                (case ((let sem_GrAltL_Cons_1 :: T_GrAltL_1 
                            sem_GrAltL_Cons_1  =
                                (\ _lhsIdelayedEvalMp
                                   _lhsIisEvalMp
                                   _lhsIopts ->
                                     (case (_lhsIdelayedEvalMp) of
                                      { _hdOdelayedEvalMp ->
                                      (case (_lhsIopts) of
                                       { _hdOopts ->
                                       (case (_lhsIisEvalMp) of
                                        { _hdOisEvalMp ->
                                        (case (hd_1 _hdOdelayedEvalMp _hdOisEvalMp _hdOopts ) of
                                         { ( _hdIdelayedEvalMp,_hdItrf) ->
                                             (case (_hdIdelayedEvalMp) of
                                              { _tlOdelayedEvalMp ->
                                              (case (_lhsIopts) of
                                               { _tlOopts ->
                                               (case (_lhsIisEvalMp) of
                                                { _tlOisEvalMp ->
                                                (case (tl_1 _tlOdelayedEvalMp _tlOisEvalMp _tlOopts ) of
                                                 { ( _tlIdelayedEvalMp,_tlItrf) ->
                                                     (case (_tlIdelayedEvalMp) of
                                                      { _lhsOdelayedEvalMp ->
                                                      (case ((:) _hdItrf _tlItrf) of
                                                       { _trf ->
                                                       (case (_trf) of
                                                        { _lhsOtrf ->
                                                        ( _lhsOdelayedEvalMp,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                        in  sem_GrAltL_Cons_1)) of
                 { ( sem_GrAltL_1) ->
                 ( _lhsOgathFviMp,_lhsOwillUseForMpL,sem_GrAltL_1) }) }) }) }) })
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ([]) of
      { _lhsOwillUseForMpL ->
      (case ((let sem_GrAltL_Nil_1 :: T_GrAltL_1 
                  sem_GrAltL_Nil_1  =
                      (\ _lhsIdelayedEvalMp
                         _lhsIisEvalMp
                         _lhsIopts ->
                           (case (_lhsIdelayedEvalMp) of
                            { _lhsOdelayedEvalMp ->
                            (case ([]) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOdelayedEvalMp,_lhsOtrf) }) }) }))
              in  sem_GrAltL_Nil_1)) of
       { ( sem_GrAltL_1) ->
       ( _lhsOgathFviMp,_lhsOwillUseForMpL,sem_GrAltL_1) }) }) })
-- GrBind ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Arity:
         child nm             : {HsName}
         child arity          : {Int}
         visit 0:
            local trf         : _
      alternative Bind:
         child nm             : {HsName}
         child annot          : {GrBindAnn}
         child argNmL         : {[HsName]}
         child expr           : GrExpr 
         visit 0:
            local trf         : _
      alternative Rec:
         child bindL          : GrBindL 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrBind :: GrBind  ->
              T_GrBind 
sem_GrBind (GrBind_Arity _nm _arity )  =
    (sem_GrBind_Arity _nm _arity )
sem_GrBind (GrBind_Bind _nm _annot _argNmL _expr )  =
    (sem_GrBind_Bind _nm _annot _argNmL (sem_GrExpr _expr ) )
sem_GrBind (GrBind_Rec _bindL )  =
    (sem_GrBind_Rec (sem_GrBindL _bindL ) )
-- semantic domain
type T_GrBind  = EHCOpts ->
                 ( FvInfoMp,GrBind )
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (\ _lhsIopts ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case (GrBind_Arity nm_ arity_) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (\ _lhsIopts ->
         (case (expr_ ) of
          { ( _exprIalsoWillUseForMp,_exprIgathFviMp,_exprIwillUseForMp,expr_1) ->
              (case (_exprIgathFviMp `fviMpDifference` fviMpFromList argNmL_) of
               { _lhsOgathFviMp ->
               (case (_lhsIopts) of
                { _exprOopts ->
                (case (Map.empty) of
                 { _exprOdelayedEvalMp ->
                 (case (_exprIgathFviMp) of
                  { _exprOfviMp ->
                  (case (Map.empty) of
                   { _exprOisEvalMp ->
                   (case (Set.empty) of
                    { _exprOwillUseFor ->
                    (case (Nothing) of
                     { _exprOmbIntroNm ->
                     (case (expr_1 _exprOdelayedEvalMp _exprOfviMp _exprOisEvalMp _exprOmbIntroNm _exprOopts _exprOwillUseFor ) of
                      { ( _exprIdelayedEvalMp,_exprIgathIsEvalMp,_exprIhasSideEffect,_exprIisEval,_exprImbDelayedExpr,_exprImbMkPatSeq,_exprInmAlias,_exprItrf) ->
                          (case (GrBind_Bind nm_ annot_ argNmL_ _exprItrf) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _bindLOopts ->
          (case (bindL_ _bindLOopts ) of
           { ( _bindLIgathFviMp,_bindLItrf) ->
               (case (_bindLIgathFviMp) of
                { _lhsOgathFviMp ->
                (case (GrBind_Rec _bindLItrf) of
                 { _trf ->
                 (case (_trf) of
                  { _lhsOtrf ->
                  ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }))
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrBind 
         child tl             : GrBindL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrBindL :: GrBindL  ->
               T_GrBindL 
sem_GrBindL list  =
    (Prelude.foldr sem_GrBindL_Cons sem_GrBindL_Nil (Prelude.map sem_GrBind list) )
-- semantic domain
type T_GrBindL  = EHCOpts ->
                  ( FvInfoMp,GrBindL )
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts ->
          (case (tl_ _tlOopts ) of
           { ( _tlIgathFviMp,_tlItrf) ->
               (case (_lhsIopts) of
                { _hdOopts ->
                (case (hd_ _hdOopts ) of
                 { ( _hdIgathFviMp,_hdItrf) ->
                     (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                      { _lhsOgathFviMp ->
                      (case ((:) _hdItrf _tlItrf) of
                       { _trf ->
                       (case (_trf) of
                        { _lhsOtrf ->
                        ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }))
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (\ _lhsIopts ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case ([]) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         alsoWillUseForMp     : WillUseForMp
         gathFviMp            : FvInfoMp
         willUseForMp         : WillUseForMp
   visit 1:
      inherited attributes:
         fviMp                : FvInfoMp
         isEvalMp             : IsEvalMp
         mbIntroNm            : Maybe HsName
         opts                 : EHCOpts
         willUseFor           : WillUseForS
      chained attribute:
         delayedEvalMp        : DelayedEvalMp
      synthesized attributes:
         gathIsEvalMp         : IsEvalMp
         hasSideEffect        : Bool
         isEval               : IsEval
         mbDelayedExpr        : Maybe DelayedExpr
         mbMkPatSeq           : Maybe MkPatSeq
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local _tup1       : _
            local delayedIsEvalMp : _
            local delayedIsEvals : _
            local newDelIsEvalMp : _
            local trf         : _
            local appTrf      : {GrExpr}
            local mbDelayedExpr : _
            local delayedMkSeq : _
            local trfNew      : _
            intra willUseForMp : {WillUseForMp}
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative Eval:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local _tup2       : _
            local delayedIsEvalMp : _
            local delayedIsEvals : _
            local newDelIsEvalMp : _
            local mbDelayedExpr : _
            local trf         : _
            local delayedMkSeq : _
            local trfNew      : _
            intra willUseForMp : {WillUseForMp}
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative FetchNode:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
         visit 0:
            local gathBodyFviMp : {FvInfoMp}
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbIntroNm   : _
            local mbDelayedExpr : _
            local trfNew      : _
            intra gathBodyFviMp : {FvInfoMp}
      alternative Store:
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local trf         : _
      alternative Throw:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local trf         : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
         visit 1:
            local mbDelayedExpr : _
            local trf         : _
            local trfNew      : _
-}
-- cata
sem_GrExpr :: GrExpr  ->
              T_GrExpr 
sem_GrExpr (GrExpr_App _nm _argL )  =
    (sem_GrExpr_App _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Call _nm _argL )  =
    (sem_GrExpr_Call _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Case _val _altL )  =
    (sem_GrExpr_Case (sem_GrVal _val ) (sem_GrAltL _altL ) )
sem_GrExpr (GrExpr_Catch _body _arg _handler )  =
    (sem_GrExpr_Catch (sem_GrExpr _body ) _arg (sem_GrExpr _handler ) )
sem_GrExpr (GrExpr_Eval _nm )  =
    (sem_GrExpr_Eval _nm )
sem_GrExpr (GrExpr_FFI _callconv _impEnt _ffiAnnot _argL )  =
    (sem_GrExpr_FFI _callconv _impEnt _ffiAnnot (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_FetchField _nm _offset _mbTag )  =
    (sem_GrExpr_FetchField _nm _offset _mbTag )
sem_GrExpr (GrExpr_FetchNode _nm )  =
    (sem_GrExpr_FetchNode _nm )
sem_GrExpr (GrExpr_FetchUpdate _src _dst )  =
    (sem_GrExpr_FetchUpdate _src _dst )
sem_GrExpr (GrExpr_Seq _expr _pat _body )  =
    (sem_GrExpr_Seq (sem_GrExpr _expr ) (sem_GrPatLam _pat ) (sem_GrExpr _body ) )
sem_GrExpr (GrExpr_Store _val )  =
    (sem_GrExpr_Store (sem_GrVal _val ) )
sem_GrExpr (GrExpr_Throw _nm )  =
    (sem_GrExpr_Throw _nm )
sem_GrExpr (GrExpr_Unit _val _type )  =
    (sem_GrExpr_Unit (sem_GrVal _val ) (sem_GrType _type ) )
sem_GrExpr (GrExpr_UpdateUnit _nm _val )  =
    (sem_GrExpr_UpdateUnit _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrExpr  = ( WillUseForMp,FvInfoMp,WillUseForMp,T_GrExpr_1 )
type T_GrExpr_1  = DelayedEvalMp ->
                   FvInfoMp ->
                   IsEvalMp ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   WillUseForS ->
                   ( DelayedEvalMp,IsEvalMp,Bool,IsEval,(Maybe DelayedExpr),(Maybe MkPatSeq),NmAlias,GrExpr )
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (case (argL_ ) of
     { ( _argLIgathFviMp,argL_1) ->
         (case (Map.map (const $ Set.singleton WillUseFor_NodeField) _argLIgathFviMp) of
          { _lhsOalsoWillUseForMp ->
          (case (fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]) of
           { _gathFviMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_App_1 :: T_GrExpr_1 
                          sem_GrExpr_App_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (let (here,furtheron) = Map.partitionWithKey (\n _ -> willUseForEval n _willUseForMp) _lhsIdelayedEvalMp
                                              (evals,isevals,isevalmps) = unzip3 [ (e,(n,ie),iem) | (n,(e,ie,iem)) <- Map.toList here ]
                                          in  (foldr (.) id evals,furtheron,isevals,Map.unions isevalmps)) of
                                    { __tup1 ->
                                    (case (__tup1) of
                                     { (_,_lhsOdelayedEvalMp,_,_) ->
                                     (case (__tup1) of
                                      { (_,_,_,_delayedIsEvalMp) ->
                                      (case (__tup1) of
                                       { (_,_,_delayedIsEvals,_) ->
                                       (case (Map.unions [Map.fromList _delayedIsEvals, _delayedIsEvalMp]) of
                                        { _newDelIsEvalMp ->
                                        (case (_newDelIsEvalMp) of
                                         { _lhsOgathIsEvalMp ->
                                         (case (False) of
                                          { _lhsOhasSideEffect ->
                                          (case (_lhsIopts) of
                                           { _argLOopts ->
                                           (case (_lhsIisEvalMp) of
                                            { _argLOisEvalMp ->
                                            (case (argL_1 _argLOisEvalMp _argLOopts ) of
                                             { ( _argLInmAliasL,_argLItrf) ->
                                                 (case (GrExpr_App nm_ _argLItrf) of
                                                  { _trf ->
                                                  (case (case Map.lookup nm_ $ Map.union _newDelIsEvalMp _lhsIisEvalMp of
                                                           Just (IsEval_Apply f as) | not $ nm_ `Map.member` _lhsIfviMp
                                                             -> GrExpr_App f (as ++ _argLItrf)
                                                           _ -> _trf) of
                                                   { _appTrf ->
                                                   (case (case _appTrf of
                                                            GrExpr_App f as -> IsEval_Apply f as) of
                                                    { _lhsOisEval ->
                                                    (case (Nothing) of
                                                     { _mbDelayedExpr ->
                                                     (case (_mbDelayedExpr) of
                                                      { _lhsOmbDelayedExpr ->
                                                      (case (__tup1) of
                                                       { (_delayedMkSeq,_,_,_) ->
                                                       (case (_delayedMkSeq _appTrf) of
                                                        { _trfNew ->
                                                        (case (mkMbPatSeq _trfNew) of
                                                         { _lhsOmbMkPatSeq ->
                                                         (case (NmAlias_None) of
                                                          { _lhsOnmAlias ->
                                                          (case (_trfNew) of
                                                           { _lhsOtrf ->
                                                           ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_App_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (case (argL_ ) of
     { ( _argLIgathFviMp,argL_1) ->
         (case (Map.map (const $ Set.singleton WillUseFor_NodeField) _argLIgathFviMp) of
          { _lhsOalsoWillUseForMp ->
          (case (fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]) of
           { _gathFviMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_Call_1 :: T_GrExpr_1 
                          sem_GrExpr_Call_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (_lhsIdelayedEvalMp) of
                                    { _lhsOdelayedEvalMp ->
                                    (case (Map.empty) of
                                     { _lhsOgathIsEvalMp ->
                                     (case (False) of
                                      { _lhsOhasSideEffect ->
                                      (case (IsEval_EvalToNode) of
                                       { _lhsOisEval ->
                                       (case (Nothing) of
                                        { _mbDelayedExpr ->
                                        (case (_mbDelayedExpr) of
                                         { _lhsOmbDelayedExpr ->
                                         (case (_lhsIopts) of
                                          { _argLOopts ->
                                          (case (_lhsIisEvalMp) of
                                           { _argLOisEvalMp ->
                                           (case (argL_1 _argLOisEvalMp _argLOopts ) of
                                            { ( _argLInmAliasL,_argLItrf) ->
                                                (case (GrExpr_Call nm_ _argLItrf) of
                                                 { _trf ->
                                                 (case (_trf) of
                                                  { _trfNew ->
                                                  (case (mkMbPatSeq _trfNew) of
                                                   { _lhsOmbMkPatSeq ->
                                                   (case (NmAlias_None) of
                                                    { _lhsOnmAlias ->
                                                    (case (_trfNew) of
                                                     { _lhsOtrf ->
                                                     ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_Call_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (altL_ ) of
      { ( _altLIgathFviMp,_altLIwillUseForMpL,altL_1) ->
          (case (val_ ) of
           { ( _valIgathFviMp,val_1) ->
               (case (fviMpUnions [_valIgathFviMp, _altLIgathFviMp]) of
                { _gathFviMp ->
                (case (_gathFviMp) of
                 { _lhsOgathFviMp ->
                 (case (foldr1 willUseIntersection _altLIwillUseForMpL) of
                  { _willUseForMp ->
                  (case (_willUseForMp) of
                   { _lhsOwillUseForMp ->
                   (case ((let sem_GrExpr_Case_1 :: T_GrExpr_1 
                               sem_GrExpr_Case_1  =
                                   (\ _lhsIdelayedEvalMp
                                      _lhsIfviMp
                                      _lhsIisEvalMp
                                      _lhsImbIntroNm
                                      _lhsIopts
                                      _lhsIwillUseFor ->
                                        (case (_lhsIdelayedEvalMp) of
                                         { _altLOdelayedEvalMp ->
                                         (case (_lhsIopts) of
                                          { _altLOopts ->
                                          (case (_lhsIisEvalMp) of
                                           { _altLOisEvalMp ->
                                           (case (altL_1 _altLOdelayedEvalMp _altLOisEvalMp _altLOopts ) of
                                            { ( _altLIdelayedEvalMp,_altLItrf) ->
                                                (case (_altLIdelayedEvalMp) of
                                                 { _lhsOdelayedEvalMp ->
                                                 (case (_lhsIwillUseFor) of
                                                  { _valOwillUseFor ->
                                                  (case (_lhsIopts) of
                                                   { _valOopts ->
                                                   (case (_lhsIisEvalMp) of
                                                    { _valOisEvalMp ->
                                                    (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                                     { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                                         (case (_valIgathIsEvalMp) of
                                                          { _lhsOgathIsEvalMp ->
                                                          (case (False) of
                                                           { _lhsOhasSideEffect ->
                                                           (case (IsEval_None) of
                                                            { _lhsOisEval ->
                                                            (case (Nothing) of
                                                             { _mbDelayedExpr ->
                                                             (case (_mbDelayedExpr) of
                                                              { _lhsOmbDelayedExpr ->
                                                              (case (GrExpr_Case _valItrf _altLItrf) of
                                                               { _trf ->
                                                               (case (_trf) of
                                                                { _trfNew ->
                                                                (case (mkMbPatSeq _trfNew) of
                                                                 { _lhsOmbMkPatSeq ->
                                                                 (case (NmAlias_None) of
                                                                  { _lhsOnmAlias ->
                                                                  (case (_trfNew) of
                                                                   { _lhsOtrf ->
                                                                   ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                           in  sem_GrExpr_Case_1)) of
                    { ( sem_GrExpr_1) ->
                    ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) }) })
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (handler_ ) of
      { ( _handlerIalsoWillUseForMp,_handlerIgathFviMp,_handlerIwillUseForMp,handler_1) ->
          (case (body_ ) of
           { ( _bodyIalsoWillUseForMp,_bodyIgathFviMp,_bodyIwillUseForMp,body_1) ->
               (case (fviMpUnions [fviMpSingleton arg_, _bodyIgathFviMp, _handlerIgathFviMp]) of
                { _gathFviMp ->
                (case (_gathFviMp) of
                 { _lhsOgathFviMp ->
                 (case (Map.empty) of
                  { _willUseForMp ->
                  (case (_willUseForMp) of
                   { _lhsOwillUseForMp ->
                   (case ((let sem_GrExpr_Catch_1 :: T_GrExpr_1 
                               sem_GrExpr_Catch_1  =
                                   (\ _lhsIdelayedEvalMp
                                      _lhsIfviMp
                                      _lhsIisEvalMp
                                      _lhsImbIntroNm
                                      _lhsIopts
                                      _lhsIwillUseFor ->
                                        (case (_lhsIdelayedEvalMp) of
                                         { _bodyOdelayedEvalMp ->
                                         (case (_lhsIwillUseFor) of
                                          { _bodyOwillUseFor ->
                                          (case (_lhsIopts) of
                                           { _bodyOopts ->
                                           (case (_lhsImbIntroNm) of
                                            { _bodyOmbIntroNm ->
                                            (case (_lhsIisEvalMp) of
                                             { _bodyOisEvalMp ->
                                             (case (_lhsIfviMp) of
                                              { _bodyOfviMp ->
                                              (case (body_1 _bodyOdelayedEvalMp _bodyOfviMp _bodyOisEvalMp _bodyOmbIntroNm _bodyOopts _bodyOwillUseFor ) of
                                               { ( _bodyIdelayedEvalMp,_bodyIgathIsEvalMp,_bodyIhasSideEffect,_bodyIisEval,_bodyImbDelayedExpr,_bodyImbMkPatSeq,_bodyInmAlias,_bodyItrf) ->
                                                   (case (_bodyIdelayedEvalMp) of
                                                    { _handlerOdelayedEvalMp ->
                                                    (case (_lhsIwillUseFor) of
                                                     { _handlerOwillUseFor ->
                                                     (case (_lhsIopts) of
                                                      { _handlerOopts ->
                                                      (case (_lhsImbIntroNm) of
                                                       { _handlerOmbIntroNm ->
                                                       (case (_lhsIisEvalMp) of
                                                        { _handlerOisEvalMp ->
                                                        (case (_lhsIfviMp) of
                                                         { _handlerOfviMp ->
                                                         (case (handler_1 _handlerOdelayedEvalMp _handlerOfviMp _handlerOisEvalMp _handlerOmbIntroNm _handlerOopts _handlerOwillUseFor ) of
                                                          { ( _handlerIdelayedEvalMp,_handlerIgathIsEvalMp,_handlerIhasSideEffect,_handlerIisEval,_handlerImbDelayedExpr,_handlerImbMkPatSeq,_handlerInmAlias,_handlerItrf) ->
                                                              (case (_handlerIdelayedEvalMp) of
                                                               { _lhsOdelayedEvalMp ->
                                                               (case (_bodyIgathIsEvalMp `Map.union` _handlerIgathIsEvalMp) of
                                                                { _lhsOgathIsEvalMp ->
                                                                (case (False) of
                                                                 { _lhsOhasSideEffect ->
                                                                 (case (IsEval_None) of
                                                                  { _lhsOisEval ->
                                                                  (case (Nothing) of
                                                                   { _mbDelayedExpr ->
                                                                   (case (_mbDelayedExpr) of
                                                                    { _lhsOmbDelayedExpr ->
                                                                    (case (GrExpr_Catch _bodyItrf arg_ _handlerItrf) of
                                                                     { _trf ->
                                                                     (case (_trf) of
                                                                      { _trfNew ->
                                                                      (case (mkMbPatSeq _trfNew) of
                                                                       { _lhsOmbMkPatSeq ->
                                                                       (case (NmAlias_None) of
                                                                        { _lhsOnmAlias ->
                                                                        (case (_trfNew) of
                                                                         { _lhsOtrf ->
                                                                         ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                           in  sem_GrExpr_Catch_1)) of
                    { ( sem_GrExpr_1) ->
                    ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) }) })
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (fviMpSingleton' FvUse_Val nm_) of
      { _gathFviMp ->
      (case (_gathFviMp) of
       { _lhsOgathFviMp ->
       (case (Map.singleton nm_ (Set.singleton WillUseFor_Eval)) of
        { _willUseForMp ->
        (case (_willUseForMp) of
         { _lhsOwillUseForMp ->
         (case ((let sem_GrExpr_Eval_1 :: T_GrExpr_1 
                     sem_GrExpr_Eval_1  =
                         (\ _lhsIdelayedEvalMp
                            _lhsIfviMp
                            _lhsIisEvalMp
                            _lhsImbIntroNm
                            _lhsIopts
                            _lhsIwillUseFor ->
                              (case (let (here,furtheron) = Map.partitionWithKey (\n _ -> willUseForEval n _willUseForMp) _lhsIdelayedEvalMp
                                         (evals,isevals,isevalmps) = unzip3 [ (e,(n,ie),iem) | (n,(e,ie,iem)) <- Map.toList here ]
                                     in  (foldr (.) id evals,furtheron,isevals,Map.unions isevalmps)) of
                               { __tup2 ->
                               (case (__tup2) of
                                { (_,_lhsOdelayedEvalMp,_,_) ->
                                (case (__tup2) of
                                 { (_,_,_,_delayedIsEvalMp) ->
                                 (case (__tup2) of
                                  { (_,_,_delayedIsEvals,_) ->
                                  (case (Map.unions [Map.fromList _delayedIsEvals, _delayedIsEvalMp]) of
                                   { _newDelIsEvalMp ->
                                   (case (Map.unions [maybe Map.empty (\n -> Map.singleton nm_ (IsEval_EvalToNodeNm n)) _lhsImbIntroNm, _newDelIsEvalMp]) of
                                    { _lhsOgathIsEvalMp ->
                                    (case (True) of
                                     { _lhsOhasSideEffect ->
                                     (case (IsEval_EvalToNode) of
                                      { _lhsOisEval ->
                                      (case (Nothing) of
                                       { _mbDelayedExpr ->
                                       (case (_mbDelayedExpr) of
                                        { _lhsOmbDelayedExpr ->
                                        (case (GrExpr_Eval nm_) of
                                         { _trf ->
                                         (case (__tup2) of
                                          { (_delayedMkSeq,_,_,_) ->
                                          (case (let a = Map.lookup nm_ $ Map.union _newDelIsEvalMp _lhsIisEvalMp
                                                 in
                                                     _delayedMkSeq $
                                                     case a of
                                                       Just e | isEvalEvaluated _lhsIopts e
                                                         -> GrExpr_Unit (GrVal_Var $ isEvalNm e nm_) GrType_None
                                                       _ -> _trf) of
                                           { _trfNew ->
                                           (case (mkMbPatSeq _trfNew) of
                                            { _lhsOmbMkPatSeq ->
                                            (case (NmAlias_Eval nm_) of
                                             { _lhsOnmAlias ->
                                             (case (_trfNew) of
                                              { _lhsOtrf ->
                                              ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                 in  sem_GrExpr_Eval_1)) of
          { ( sem_GrExpr_1) ->
          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) })
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (argL_ ) of
      { ( _argLIgathFviMp,argL_1) ->
          (case (_argLIgathFviMp) of
           { _gathFviMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_FFI_1 :: T_GrExpr_1 
                          sem_GrExpr_FFI_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (_lhsIdelayedEvalMp) of
                                    { _lhsOdelayedEvalMp ->
                                    (case (Map.empty) of
                                     { _lhsOgathIsEvalMp ->
                                     (case (False) of
                                      { _lhsOhasSideEffect ->
                                      (case (if grFFIAnnotIsResEvaluated ffiAnnot_ then IsEval_EvalToNode else IsEval_None) of
                                       { _lhsOisEval ->
                                       (case (Nothing) of
                                        { _mbDelayedExpr ->
                                        (case (_mbDelayedExpr) of
                                         { _lhsOmbDelayedExpr ->
                                         (case (_lhsIopts) of
                                          { _argLOopts ->
                                          (case (_lhsIisEvalMp) of
                                           { _argLOisEvalMp ->
                                           (case (argL_1 _argLOisEvalMp _argLOopts ) of
                                            { ( _argLInmAliasL,_argLItrf) ->
                                                (case (GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf) of
                                                 { _trf ->
                                                 (case (_trf) of
                                                  { _trfNew ->
                                                  (case (mkMbPatSeq _trfNew) of
                                                   { _lhsOmbMkPatSeq ->
                                                   (case (NmAlias_None) of
                                                    { _lhsOnmAlias ->
                                                    (case (_trfNew) of
                                                     { _lhsOtrf ->
                                                     ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_FFI_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (fviMpSingleton' FvUse_Val nm_) of
      { _gathFviMp ->
      (case (_gathFviMp) of
       { _lhsOgathFviMp ->
       (case (Map.empty) of
        { _willUseForMp ->
        (case (_willUseForMp) of
         { _lhsOwillUseForMp ->
         (case ((let sem_GrExpr_FetchField_1 :: T_GrExpr_1 
                     sem_GrExpr_FetchField_1  =
                         (\ _lhsIdelayedEvalMp
                            _lhsIfviMp
                            _lhsIisEvalMp
                            _lhsImbIntroNm
                            _lhsIopts
                            _lhsIwillUseFor ->
                              (case (_lhsIdelayedEvalMp) of
                               { _lhsOdelayedEvalMp ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (False) of
                                 { _lhsOhasSideEffect ->
                                 (case (IsEval_None) of
                                  { _lhsOisEval ->
                                  (case (Nothing) of
                                   { _mbDelayedExpr ->
                                   (case (_mbDelayedExpr) of
                                    { _lhsOmbDelayedExpr ->
                                    (case (GrExpr_FetchField nm_ offset_ mbTag_) of
                                     { _trf ->
                                     (case (_trf) of
                                      { _trfNew ->
                                      (case (mkMbPatSeq _trfNew) of
                                       { _lhsOmbMkPatSeq ->
                                       (case (NmAlias_None) of
                                        { _lhsOnmAlias ->
                                        (case (_trfNew) of
                                         { _lhsOtrf ->
                                         ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                 in  sem_GrExpr_FetchField_1)) of
          { ( sem_GrExpr_1) ->
          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) })
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (fviMpSingleton' FvUse_Val nm_) of
      { _gathFviMp ->
      (case (_gathFviMp) of
       { _lhsOgathFviMp ->
       (case (Map.empty) of
        { _willUseForMp ->
        (case (_willUseForMp) of
         { _lhsOwillUseForMp ->
         (case ((let sem_GrExpr_FetchNode_1 :: T_GrExpr_1 
                     sem_GrExpr_FetchNode_1  =
                         (\ _lhsIdelayedEvalMp
                            _lhsIfviMp
                            _lhsIisEvalMp
                            _lhsImbIntroNm
                            _lhsIopts
                            _lhsIwillUseFor ->
                              (case (_lhsIdelayedEvalMp) of
                               { _lhsOdelayedEvalMp ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (False) of
                                 { _lhsOhasSideEffect ->
                                 (case (IsEval_None) of
                                  { _lhsOisEval ->
                                  (case (Nothing) of
                                   { _mbDelayedExpr ->
                                   (case (_mbDelayedExpr) of
                                    { _lhsOmbDelayedExpr ->
                                    (case (GrExpr_FetchNode nm_) of
                                     { _trf ->
                                     (case (_trf) of
                                      { _trfNew ->
                                      (case (mkMbPatSeq _trfNew) of
                                       { _lhsOmbMkPatSeq ->
                                       (case (NmAlias_None) of
                                        { _lhsOnmAlias ->
                                        (case (_trfNew) of
                                         { _lhsOtrf ->
                                         ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                 in  sem_GrExpr_FetchNode_1)) of
          { ( sem_GrExpr_1) ->
          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) })
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (fviMpFromList [src_,dst_]) of
      { _gathFviMp ->
      (case (_gathFviMp) of
       { _lhsOgathFviMp ->
       (case (Map.empty) of
        { _willUseForMp ->
        (case (_willUseForMp) of
         { _lhsOwillUseForMp ->
         (case ((let sem_GrExpr_FetchUpdate_1 :: T_GrExpr_1 
                     sem_GrExpr_FetchUpdate_1  =
                         (\ _lhsIdelayedEvalMp
                            _lhsIfviMp
                            _lhsIisEvalMp
                            _lhsImbIntroNm
                            _lhsIopts
                            _lhsIwillUseFor ->
                              (case (_lhsIdelayedEvalMp) of
                               { _lhsOdelayedEvalMp ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (False) of
                                 { _lhsOhasSideEffect ->
                                 (case (IsEval_None) of
                                  { _lhsOisEval ->
                                  (case (Nothing) of
                                   { _mbDelayedExpr ->
                                   (case (_mbDelayedExpr) of
                                    { _lhsOmbDelayedExpr ->
                                    (case (GrExpr_FetchUpdate src_ dst_) of
                                     { _trf ->
                                     (case (_trf) of
                                      { _trfNew ->
                                      (case (mkMbPatSeq _trfNew) of
                                       { _lhsOmbMkPatSeq ->
                                       (case (NmAlias_None) of
                                        { _lhsOnmAlias ->
                                        (case (_trfNew) of
                                         { _lhsOtrf ->
                                         ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                 in  sem_GrExpr_FetchUpdate_1)) of
          { ( sem_GrExpr_1) ->
          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) })
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (body_ ) of
      { ( _bodyIalsoWillUseForMp,_bodyIgathFviMp,_bodyIwillUseForMp,body_1) ->
          (case (pat_ ) of
           { ( _patIintroNmL,pat_1) ->
               (case (_bodyIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL) of
                { _gathBodyFviMp ->
                (case (expr_ ) of
                 { ( _exprIalsoWillUseForMp,_exprIgathFviMp,_exprIwillUseForMp,expr_1) ->
                     (case (fviMpUnions [_exprIgathFviMp, _gathBodyFviMp]) of
                      { _gathFviMp ->
                      (case (_gathFviMp) of
                       { _lhsOgathFviMp ->
                       (case (willUseUnion _exprIwillUseForMp _bodyIwillUseForMp) of
                        { _willUseForMp ->
                        (case (_willUseForMp `willUseUnion` _exprIalsoWillUseForMp `willUseUnion` _bodyIalsoWillUseForMp) of
                         { _lhsOwillUseForMp ->
                         (case ((let sem_GrExpr_Seq_1 :: T_GrExpr_1 
                                     sem_GrExpr_Seq_1  =
                                         (\ _lhsIdelayedEvalMp
                                            _lhsIfviMp
                                            _lhsIisEvalMp
                                            _lhsImbIntroNm
                                            _lhsIopts
                                            _lhsIwillUseFor ->
                                              (case (_lhsIdelayedEvalMp) of
                                               { _exprOdelayedEvalMp ->
                                               (case (_lhsIopts) of
                                                { _patOopts ->
                                                (case (_lhsIisEvalMp) of
                                                 { _patOisEvalMp ->
                                                 (case (pat_1 _patOisEvalMp _patOopts ) of
                                                  { ( _patIgathFviMp,_patInmAlias,_patItrf) ->
                                                      (case (case _patInmAlias of
                                                             NmAlias_Nm nmp
                                                               ->
                                                                  willUseFor nmp _bodyIwillUseForMp
                                                             _ -> Set.empty) of
                                                       { _exprOwillUseFor ->
                                                       (case (case _patInmAlias of
                                                                NmAlias_Nm nmp -> Just nmp
                                                                _              -> Nothing) of
                                                        { _mbIntroNm ->
                                                        (case (_lhsIopts) of
                                                         { _exprOopts ->
                                                         (case (_mbIntroNm) of
                                                          { _exprOmbIntroNm ->
                                                          (case (_lhsIisEvalMp) of
                                                           { _exprOisEvalMp ->
                                                           (case (_gathBodyFviMp) of
                                                            { _exprOfviMp ->
                                                            (case (expr_1 _exprOdelayedEvalMp _exprOfviMp _exprOisEvalMp _exprOmbIntroNm _exprOopts _exprOwillUseFor ) of
                                                             { ( _exprIdelayedEvalMp,_exprIgathIsEvalMp,_exprIhasSideEffect,_exprIisEval,_exprImbDelayedExpr,_exprImbMkPatSeq,_exprInmAlias,_exprItrf) ->
                                                                 (case (Map.unions [maybeAnd Map.empty (\n e -> Map.singleton n (del2del (mkSeq n) e)) _mbIntroNm _exprImbDelayedExpr, _exprIdelayedEvalMp]) of
                                                                  { _bodyOdelayedEvalMp ->
                                                                  (case (_lhsIwillUseFor) of
                                                                   { _bodyOwillUseFor ->
                                                                   (case (_lhsIopts) of
                                                                    { _bodyOopts ->
                                                                    (case (_lhsIfviMp) of
                                                                     { _bodyOfviMp ->
                                                                     (case (Nothing) of
                                                                      { _bodyOmbIntroNm ->
                                                                      (case (let mp = Map.unions [_exprIgathIsEvalMp,_lhsIisEvalMp]
                                                                             in  case _patInmAlias of
                                                                                   NmAlias_Nm nmp | isJust _exprImbMkPatSeq && isEvalEvaluated _lhsIopts _exprIisEval
                                                                                     -> Map.insert nmp _exprIisEval mp
                                                                                   _ -> mp `Map.difference` Map.fromList (zip _patIintroNmL _patIintroNmL)) of
                                                                       { _bodyOisEvalMp ->
                                                                       (case (body_1 _bodyOdelayedEvalMp _bodyOfviMp _bodyOisEvalMp _bodyOmbIntroNm _bodyOopts _bodyOwillUseFor ) of
                                                                        { ( _bodyIdelayedEvalMp,_bodyIgathIsEvalMp,_bodyIhasSideEffect,_bodyIisEval,_bodyImbDelayedExpr,_bodyImbMkPatSeq,_bodyInmAlias,_bodyItrf) ->
                                                                            (case (_bodyIdelayedEvalMp) of
                                                                             { _lhsOdelayedEvalMp ->
                                                                             (case (_exprIgathIsEvalMp `Map.union` _bodyIgathIsEvalMp) of
                                                                              { _lhsOgathIsEvalMp ->
                                                                              (case (False) of
                                                                               { _lhsOhasSideEffect ->
                                                                               (case (IsEval_None) of
                                                                                { _lhsOisEval ->
                                                                                (case (Nothing) of
                                                                                 { _mbDelayedExpr ->
                                                                                 (case (_mbDelayedExpr) of
                                                                                  { _lhsOmbDelayedExpr ->
                                                                                  (case (maybe (id) (\mk -> mk _patItrf) _exprImbMkPatSeq $ _bodyItrf) of
                                                                                   { _trfNew ->
                                                                                   (case (mkMbPatSeq _trfNew) of
                                                                                    { _lhsOmbMkPatSeq ->
                                                                                    (case (case (_bodyInmAlias,_exprIhasSideEffect) of
                                                                                             (NmAlias_Nm n,True) -> NmAlias_NmAfterSideEffect n
                                                                                             _ -> _bodyInmAlias) of
                                                                                     { _lhsOnmAlias ->
                                                                                     (case (_trfNew) of
                                                                                      { _lhsOtrf ->
                                                                                      ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                 in  sem_GrExpr_Seq_1)) of
                          { ( sem_GrExpr_1) ->
                          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) }) }) }) })
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (case (val_ ) of
     { ( _valIgathFviMp,val_1) ->
         (case (_valIgathFviMp) of
          { _gathFviMp ->
          (case (Map.map (const $ Set.singleton WillUseFor_NodeField) _gathFviMp) of
           { _lhsOalsoWillUseForMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_Store_1 :: T_GrExpr_1 
                          sem_GrExpr_Store_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (_lhsIdelayedEvalMp) of
                                    { _lhsOdelayedEvalMp ->
                                    (case (_lhsIwillUseFor) of
                                     { _valOwillUseFor ->
                                     (case (_lhsIopts) of
                                      { _valOopts ->
                                      (case (_lhsIisEvalMp) of
                                       { _valOisEvalMp ->
                                       (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                        { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                            (case (_valIgathIsEvalMp) of
                                             { _lhsOgathIsEvalMp ->
                                             (case (False) of
                                              { _lhsOhasSideEffect ->
                                              (case (_valIisEval) of
                                               { _lhsOisEval ->
                                               (case (_valImbDelayedExpr) of
                                                { _lhsOmbDelayedExpr ->
                                                (case (GrExpr_Store _valItrf) of
                                                 { _trf ->
                                                 (case (maybeOr (mkMbPatSeq _trf) (mkMbPatSeq) (const Nothing) _valImbGrExpr _valImbDelayedExpr) of
                                                  { _lhsOmbMkPatSeq ->
                                                  (case (_valInmAlias) of
                                                   { _lhsOnmAlias ->
                                                   (case (_trf) of
                                                    { _lhsOtrf ->
                                                    ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_Store_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (fviMpSingleton' FvUse_Val nm_) of
      { _gathFviMp ->
      (case (_gathFviMp) of
       { _lhsOgathFviMp ->
       (case (Map.empty) of
        { _willUseForMp ->
        (case (_willUseForMp) of
         { _lhsOwillUseForMp ->
         (case ((let sem_GrExpr_Throw_1 :: T_GrExpr_1 
                     sem_GrExpr_Throw_1  =
                         (\ _lhsIdelayedEvalMp
                            _lhsIfviMp
                            _lhsIisEvalMp
                            _lhsImbIntroNm
                            _lhsIopts
                            _lhsIwillUseFor ->
                              (case (_lhsIdelayedEvalMp) of
                               { _lhsOdelayedEvalMp ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (False) of
                                 { _lhsOhasSideEffect ->
                                 (case (IsEval_None) of
                                  { _lhsOisEval ->
                                  (case (Nothing) of
                                   { _mbDelayedExpr ->
                                   (case (_mbDelayedExpr) of
                                    { _lhsOmbDelayedExpr ->
                                    (case (GrExpr_Throw nm_) of
                                     { _trf ->
                                     (case (_trf) of
                                      { _trfNew ->
                                      (case (mkMbPatSeq _trfNew) of
                                       { _lhsOmbMkPatSeq ->
                                       (case (NmAlias_None) of
                                        { _lhsOnmAlias ->
                                        (case (_trfNew) of
                                         { _lhsOtrf ->
                                         ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                 in  sem_GrExpr_Throw_1)) of
          { ( sem_GrExpr_1) ->
          ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) })
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (case (val_ ) of
     { ( _valIgathFviMp,val_1) ->
         (case (_valIgathFviMp) of
          { _gathFviMp ->
          (case (Map.map (const $ Set.singleton WillUseFor_NodeField) _gathFviMp) of
           { _lhsOalsoWillUseForMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_Unit_1 :: T_GrExpr_1 
                          sem_GrExpr_Unit_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (_lhsIdelayedEvalMp) of
                                    { _lhsOdelayedEvalMp ->
                                    (case (_lhsIwillUseFor) of
                                     { _valOwillUseFor ->
                                     (case (_lhsIopts) of
                                      { _valOopts ->
                                      (case (_lhsIisEvalMp) of
                                       { _valOisEvalMp ->
                                       (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                        { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                            (case (_valIgathIsEvalMp) of
                                             { _lhsOgathIsEvalMp ->
                                             (case (False) of
                                              { _lhsOhasSideEffect ->
                                              (case (_valIisEval) of
                                               { _lhsOisEval ->
                                               (case (_valImbDelayedExpr) of
                                                { _lhsOmbDelayedExpr ->
                                                (case (_lhsIisEvalMp) of
                                                 { _typeOisEvalMp ->
                                                 (case (type_ _typeOisEvalMp ) of
                                                  { ( _typeIgathFviMp,_typeItrf) ->
                                                      (case (GrExpr_Unit _valItrf _typeItrf) of
                                                       { _trf ->
                                                       (case (maybeOr (mkMbPatSeq _trf) (mkMbPatSeq) (const Nothing) _valImbGrExpr _valImbDelayedExpr) of
                                                        { _lhsOmbMkPatSeq ->
                                                        (case (_valInmAlias) of
                                                         { _lhsOnmAlias ->
                                                         (case (_trf) of
                                                          { _lhsOtrf ->
                                                          ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_Unit_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (case (Map.empty) of
     { _lhsOalsoWillUseForMp ->
     (case (val_ ) of
      { ( _valIgathFviMp,val_1) ->
          (case (fviMpUnions [fviMpSingleton nm_, _valIgathFviMp]) of
           { _gathFviMp ->
           (case (_gathFviMp) of
            { _lhsOgathFviMp ->
            (case (Map.empty) of
             { _willUseForMp ->
             (case (_willUseForMp) of
              { _lhsOwillUseForMp ->
              (case ((let sem_GrExpr_UpdateUnit_1 :: T_GrExpr_1 
                          sem_GrExpr_UpdateUnit_1  =
                              (\ _lhsIdelayedEvalMp
                                 _lhsIfviMp
                                 _lhsIisEvalMp
                                 _lhsImbIntroNm
                                 _lhsIopts
                                 _lhsIwillUseFor ->
                                   (case (_lhsIdelayedEvalMp) of
                                    { _lhsOdelayedEvalMp ->
                                    (case (_lhsIwillUseFor) of
                                     { _valOwillUseFor ->
                                     (case (_lhsIopts) of
                                      { _valOopts ->
                                      (case (_lhsIisEvalMp) of
                                       { _valOisEvalMp ->
                                       (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                                        { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                                            (case (_valIgathIsEvalMp) of
                                             { _lhsOgathIsEvalMp ->
                                             (case (False) of
                                              { _lhsOhasSideEffect ->
                                              (case (IsEval_None) of
                                               { _lhsOisEval ->
                                               (case (Nothing) of
                                                { _mbDelayedExpr ->
                                                (case (_mbDelayedExpr) of
                                                 { _lhsOmbDelayedExpr ->
                                                 (case (GrExpr_UpdateUnit nm_ _valItrf) of
                                                  { _trf ->
                                                  (case (_trf) of
                                                   { _trfNew ->
                                                   (case (mkMbPatSeq _trfNew) of
                                                    { _lhsOmbMkPatSeq ->
                                                    (case (NmAlias_None) of
                                                     { _lhsOnmAlias ->
                                                     (case (_trfNew) of
                                                      { _lhsOtrf ->
                                                      ( _lhsOdelayedEvalMp,_lhsOgathIsEvalMp,_lhsOhasSideEffect,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbMkPatSeq,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                      in  sem_GrExpr_UpdateUnit_1)) of
               { ( sem_GrExpr_1) ->
               ( _lhsOalsoWillUseForMp,_lhsOgathFviMp,_lhsOwillUseForMp,sem_GrExpr_1) }) }) }) }) }) }) })
-- GrFFIAnnot --------------------------------------------------
{-
   alternatives:
      alternative IsResEval:
         child isEvaluated    : {Bool}
-}
-- cata
sem_GrFFIAnnot :: GrFFIAnnot  ->
                  T_GrFFIAnnot 
sem_GrFFIAnnot (GrFFIAnnot_IsResEval _isEvaluated )  =
    (sem_GrFFIAnnot_IsResEval _isEvaluated )
-- semantic domain
type T_GrFFIAnnot  = ( )
sem_GrFFIAnnot_IsResEval :: Bool ->
                            T_GrFFIAnnot 
sem_GrFFIAnnot_IsResEval isEvaluated_  =
    ( )
-- GrGlobal ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Global:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local willUseFor  : _
            local trf         : _
-}
-- cata
sem_GrGlobal :: GrGlobal  ->
                T_GrGlobal 
sem_GrGlobal (GrGlobal_Global _nm _val )  =
    (sem_GrGlobal_Global _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrGlobal  = EHCOpts ->
                   ( FvInfoMp,GrGlobal )
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (\ _lhsIopts ->
         (case (val_ ) of
          { ( _valIgathFviMp,val_1) ->
              (case (_valIgathFviMp) of
               { _lhsOgathFviMp ->
               (case (Set.empty) of
                { _willUseFor ->
                (case (_willUseFor) of
                 { _valOwillUseFor ->
                 (case (_lhsIopts) of
                  { _valOopts ->
                  (case (Map.empty) of
                   { _valOisEvalMp ->
                   (case (val_1 _valOisEvalMp _valOopts _valOwillUseFor ) of
                    { ( _valIgathIsEvalMp,_valIisEval,_valImbDelayedExpr,_valImbGrExpr,_valInmAlias,_valItrf) ->
                        (case (GrGlobal_Global nm_ _valItrf) of
                         { _trf ->
                         (case (_trf) of
                          { _lhsOtrf ->
                          ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }) }))
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrGlobal 
         child tl             : GrGlobalL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrGlobalL :: GrGlobalL  ->
                 T_GrGlobalL 
sem_GrGlobalL list  =
    (Prelude.foldr sem_GrGlobalL_Cons sem_GrGlobalL_Nil (Prelude.map sem_GrGlobal list) )
-- semantic domain
type T_GrGlobalL  = EHCOpts ->
                    ( FvInfoMp,GrGlobalL )
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _tlOopts ->
          (case (tl_ _tlOopts ) of
           { ( _tlIgathFviMp,_tlItrf) ->
               (case (_lhsIopts) of
                { _hdOopts ->
                (case (hd_ _hdOopts ) of
                 { ( _hdIgathFviMp,_hdItrf) ->
                     (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                      { _lhsOgathFviMp ->
                      (case ((:) _hdItrf _tlItrf) of
                       { _trf ->
                       (case (_trf) of
                        { _lhsOtrf ->
                        ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }))
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (\ _lhsIopts ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case ([]) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrModule ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : EHCOpts
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
         visit 0:
            local trf         : _
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = EHCOpts ->
                   ( GrModule )
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (\ _lhsIopts ->
         (case (_lhsIopts) of
          { _bindLOopts ->
          (case (bindL_ _bindLOopts ) of
           { ( _bindLIgathFviMp,_bindLItrf) ->
               (case (_lhsIopts) of
                { _globalLOopts ->
                (case (globalL_ _globalLOopts ) of
                 { ( _globalLIgathFviMp,_globalLItrf) ->
                     (case (GrModule_Mod moduleNm_ _globalLItrf _bindLItrf tagsMp_) of
                      { _trf ->
                      (case (_trf) of
                       { _lhsOtrf ->
                       ( _lhsOtrf) }) }) }) }) }) }))
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative LitInt:
         child int            : {Int}
         visit 1:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
         visit 1:
            local trf         : _
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
         visit 1:
            local trf         : _
      alternative Otherwise:
         visit 1:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 1:
            local trf         : _
-}
-- cata
sem_GrPatAlt :: GrPatAlt  ->
                T_GrPatAlt 
sem_GrPatAlt (GrPatAlt_LitInt _int )  =
    (sem_GrPatAlt_LitInt _int )
sem_GrPatAlt (GrPatAlt_Node _tag _fldL )  =
    (sem_GrPatAlt_Node (sem_GrTag _tag ) _fldL )
sem_GrPatAlt (GrPatAlt_NodeSplit _tag _nm _fldL )  =
    (sem_GrPatAlt_NodeSplit (sem_GrTag _tag ) _nm (sem_GrSplitL _fldL ) )
sem_GrPatAlt (GrPatAlt_Otherwise )  =
    (sem_GrPatAlt_Otherwise )
sem_GrPatAlt (GrPatAlt_Tag _tag )  =
    (sem_GrPatAlt_Tag (sem_GrTag _tag ) )
-- semantic domain
type T_GrPatAlt  = ( ([HsName]),T_GrPatAlt_1 )
type T_GrPatAlt_1  = IsEvalMp ->
                     EHCOpts ->
                     ( FvInfoMp,NmAlias,GrPatAlt )
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_LitInt_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_LitInt_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatAlt_LitInt int_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatAlt_LitInt_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (case (fldL_) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Node_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Node_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (tag_ ) of
                           { ( _tagIgathFviMp,tag_1) ->
                               (case (_tagIgathFviMp) of
                                { _lhsOgathFviMp ->
                                (case (NmAlias_Grp hsnUnknown $ map NmAlias_Nm fldL_) of
                                 { _lhsOnmAlias ->
                                 (case (_lhsIisEvalMp) of
                                  { _tagOisEvalMp ->
                                  (case (tag_1 _tagOisEvalMp ) of
                                   { ( _tagItrf) ->
                                       (case (GrPatAlt_Node _tagItrf fldL_) of
                                        { _trf ->
                                        (case (_trf) of
                                         { _lhsOtrf ->
                                         ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }))
             in  sem_GrPatAlt_Node_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIintroNmL,fldL_1) ->
         (case (nm_ : _fldLIintroNmL) of
          { _lhsOintroNmL ->
          (case ((let sem_GrPatAlt_NodeSplit_1 :: T_GrPatAlt_1 
                      sem_GrPatAlt_NodeSplit_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts ->
                               (case (_lhsIopts) of
                                { _fldLOopts ->
                                (case (_lhsIisEvalMp) of
                                 { _fldLOisEvalMp ->
                                 (case (fldL_1 _fldLOisEvalMp _fldLOopts ) of
                                  { ( _fldLIgathFviMp,_fldLItrf) ->
                                      (case (tag_ ) of
                                       { ( _tagIgathFviMp,tag_1) ->
                                           (case (_tagIgathFviMp `fviMpUnion` _fldLIgathFviMp) of
                                            { _lhsOgathFviMp ->
                                            (case (NmAlias_None) of
                                             { _lhsOnmAlias ->
                                             (case (_lhsIisEvalMp) of
                                              { _tagOisEvalMp ->
                                              (case (tag_1 _tagOisEvalMp ) of
                                               { ( _tagItrf) ->
                                                   (case (GrPatAlt_NodeSplit _tagItrf nm_ _fldLItrf) of
                                                    { _trf ->
                                                    (case (_trf) of
                                                     { _lhsOtrf ->
                                                     ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }))
                  in  sem_GrPatAlt_NodeSplit_1)) of
           { ( sem_GrPatAlt_1) ->
           ( _lhsOintroNmL,sem_GrPatAlt_1) }) }) })
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Otherwise_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Otherwise_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatAlt_Otherwise) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatAlt_Otherwise_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatAlt_Tag_1 :: T_GrPatAlt_1 
                 sem_GrPatAlt_Tag_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (tag_ ) of
                           { ( _tagIgathFviMp,tag_1) ->
                               (case (_tagIgathFviMp) of
                                { _lhsOgathFviMp ->
                                (case (NmAlias_None) of
                                 { _lhsOnmAlias ->
                                 (case (_lhsIisEvalMp) of
                                  { _tagOisEvalMp ->
                                  (case (tag_1 _tagOisEvalMp ) of
                                   { ( _tagItrf) ->
                                       (case (GrPatAlt_Tag _tagItrf) of
                                        { _trf ->
                                        (case (_trf) of
                                         { _lhsOtrf ->
                                         ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }))
             in  sem_GrPatAlt_Tag_1)) of
      { ( sem_GrPatAlt_1) ->
      ( _lhsOintroNmL,sem_GrPatAlt_1) }) })
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Empty:
         visit 1:
            local trf         : _
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative OpaqueAnnot:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative VarNode:
         child fldL           : GrVarL 
         visit 1:
            local trf         : _
-}
-- cata
sem_GrPatLam :: GrPatLam  ->
                T_GrPatLam 
sem_GrPatLam (GrPatLam_BasicAnnot _annot _nm )  =
    (sem_GrPatLam_BasicAnnot _annot _nm )
sem_GrPatLam (GrPatLam_BasicNode _annot _nm )  =
    (sem_GrPatLam_BasicNode _annot _nm )
sem_GrPatLam (GrPatLam_Empty )  =
    (sem_GrPatLam_Empty )
sem_GrPatLam (GrPatLam_EnumAnnot _tycon _nm )  =
    (sem_GrPatLam_EnumAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_EnumNode _nm )  =
    (sem_GrPatLam_EnumNode _nm )
sem_GrPatLam (GrPatLam_OpaqueAnnot _nm )  =
    (sem_GrPatLam_OpaqueAnnot _nm )
sem_GrPatLam (GrPatLam_OpaqueNode _nm )  =
    (sem_GrPatLam_OpaqueNode _nm )
sem_GrPatLam (GrPatLam_PtrAnnot _tycon _nm )  =
    (sem_GrPatLam_PtrAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_PtrNode _nm )  =
    (sem_GrPatLam_PtrNode _nm )
sem_GrPatLam (GrPatLam_Var _nm )  =
    (sem_GrPatLam_Var _nm )
sem_GrPatLam (GrPatLam_VarNode _fldL )  =
    (sem_GrPatLam_VarNode (sem_GrVarL _fldL ) )
-- semantic domain
type T_GrPatLam  = ( ([HsName]),T_GrPatLam_1 )
type T_GrPatLam_1  = IsEvalMp ->
                     EHCOpts ->
                     ( FvInfoMp,NmAlias,GrPatLam )
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_BasicAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_BasicAnnot_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (case annot_ of
                                    BasicAnnot_Size bsz _ _ _
                                      | basicSizeIsWord bsz  -> NmAlias_Nm nm_
                                    _                        -> NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_BasicAnnot annot_ nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_BasicAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_BasicNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_BasicNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) annot_) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_BasicNode annot_ nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_BasicNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_Empty_1 :: T_GrPatLam_1 
                 sem_GrPatLam_Empty_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_Empty) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_Empty_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_EnumAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_EnumAnnot_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_EnumAnnot tycon_ nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_EnumAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_EnumNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_EnumNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_EnumNode nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_EnumNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_OpaqueAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_OpaqueAnnot_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_OpaqueAnnot nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_OpaqueAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_OpaqueNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_OpaqueNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_OpaqueNode nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_OpaqueNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_PtrAnnot_1 :: T_GrPatLam_1 
                 sem_GrPatLam_PtrAnnot_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_PtrAnnot tycon_ nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_PtrAnnot_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_PtrNode_1 :: T_GrPatLam_1 
                 sem_GrPatLam_PtrNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_None) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_PtrNode nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_PtrNode_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrPatLam_Var_1 :: T_GrPatLam_1 
                 sem_GrPatLam_Var_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (NmAlias_Nm    nm_) of
                            { _lhsOnmAlias ->
                            (case (GrPatLam_Var nm_) of
                             { _trf ->
                             (case (_trf) of
                              { _lhsOtrf ->
                              ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }))
             in  sem_GrPatLam_Var_1)) of
      { ( sem_GrPatLam_1) ->
      ( _lhsOintroNmL,sem_GrPatLam_1) }) })
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLIintroNmL,fldL_1) ->
         (case (tail _fldLIintroNmL) of
          { _lhsOintroNmL ->
          (case ((let sem_GrPatLam_VarNode_1 :: T_GrPatLam_1 
                      sem_GrPatLam_VarNode_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts ->
                               (case (_lhsIopts) of
                                { _fldLOopts ->
                                (case (_lhsIisEvalMp) of
                                 { _fldLOisEvalMp ->
                                 (case (fldL_1 _fldLOisEvalMp _fldLOopts ) of
                                  { ( _fldLIgathFviMp,_fldLItrf) ->
                                      (case (_fldLIgathFviMp) of
                                       { _lhsOgathFviMp ->
                                       (case (NmAlias_Grp   hsnUnknown $ map NmAlias_Nm (tail _fldLIintroNmL)) of
                                        { _lhsOnmAlias ->
                                        (case (GrPatLam_VarNode _fldLItrf) of
                                         { _trf ->
                                         (case (_trf) of
                                          { _lhsOtrf ->
                                          ( _lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }))
                  in  sem_GrPatLam_VarNode_1)) of
           { ( sem_GrPatLam_1) ->
           ( _lhsOintroNmL,sem_GrPatLam_1) }) }) })
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
         visit 1:
            local willUseFor  : _
            local trf         : _
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = ( ([HsName]),T_GrSplit_1 )
type T_GrSplit_1  = IsEvalMp ->
                    EHCOpts ->
                    ( FvInfoMp,GrSplit )
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrSplit_Sel_1 :: T_GrSplit_1 
                 sem_GrSplit_Sel_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (off_ ) of
                           { ( _offIgathFviMp,off_1) ->
                               (case (_offIgathFviMp) of
                                { _lhsOgathFviMp ->
                                (case (Set.empty) of
                                 { _willUseFor ->
                                 (case (_willUseFor) of
                                  { _offOwillUseFor ->
                                  (case (_lhsIopts) of
                                   { _offOopts ->
                                   (case (_lhsIisEvalMp) of
                                    { _offOisEvalMp ->
                                    (case (off_1 _offOisEvalMp _offOopts _offOwillUseFor ) of
                                     { ( _offIgathIsEvalMp,_offIisEval,_offImbDelayedExpr,_offImbGrExpr,_offInmAlias,_offItrf) ->
                                         (case (GrSplit_Sel nm_ _offItrf) of
                                          { _trf ->
                                          (case (_trf) of
                                           { _lhsOtrf ->
                                           ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrSplit_Sel_1)) of
      { ( sem_GrSplit_1) ->
      ( _lhsOintroNmL,sem_GrSplit_1) }) })
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ( ([HsName]),T_GrSplitL_1 )
type T_GrSplitL_1  = IsEvalMp ->
                     EHCOpts ->
                     ( FvInfoMp,GrSplitL )
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIintroNmL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIintroNmL,hd_1) ->
              (case (_hdIintroNmL ++ _tlIintroNmL) of
               { _lhsOintroNmL ->
               (case ((let sem_GrSplitL_Cons_1 :: T_GrSplitL_1 
                           sem_GrSplitL_Cons_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (_lhsIopts) of
                                     { _tlOopts ->
                                     (case (_lhsIisEvalMp) of
                                      { _tlOisEvalMp ->
                                      (case (tl_1 _tlOisEvalMp _tlOopts ) of
                                       { ( _tlIgathFviMp,_tlItrf) ->
                                           (case (_lhsIopts) of
                                            { _hdOopts ->
                                            (case (_lhsIisEvalMp) of
                                             { _hdOisEvalMp ->
                                             (case (hd_1 _hdOisEvalMp _hdOopts ) of
                                              { ( _hdIgathFviMp,_hdItrf) ->
                                                  (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                                                   { _lhsOgathFviMp ->
                                                   (case ((:) _hdItrf _tlItrf) of
                                                    { _trf ->
                                                    (case (_trf) of
                                                     { _lhsOtrf ->
                                                     ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }) }))
                       in  sem_GrSplitL_Cons_1)) of
                { ( sem_GrSplitL_1) ->
                ( _lhsOintroNmL,sem_GrSplitL_1) }) }) }) })
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrSplitL_Nil_1 :: T_GrSplitL_1 
                 sem_GrSplitL_Nil_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case ([]) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
             in  sem_GrSplitL_Nil_1)) of
      { ( sem_GrSplitL_1) ->
      ( _lhsOintroNmL,sem_GrSplitL_1) }) })
-- GrTag -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   visit 1:
      inherited attribute:
         isEvalMp             : IsEvalMp
      synthesized attribute:
         trf                  : SELF 
   alternatives:
      alternative App:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Con:
         child grtgAnn        : {GrTagAnn}
         child int            : {Int}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Fun:
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Hole:
         visit 1:
            local trf         : _
      alternative PApp:
         child needs          : {Int}
         child nm             : {HsName}
         visit 1:
            local trf         : _
      alternative Rec:
         visit 1:
            local trf         : _
      alternative Unboxed:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrTag :: GrTag  ->
             T_GrTag 
sem_GrTag (GrTag_App _nm )  =
    (sem_GrTag_App _nm )
sem_GrTag (GrTag_Con _grtgAnn _int _nm )  =
    (sem_GrTag_Con _grtgAnn _int _nm )
sem_GrTag (GrTag_Fun _nm )  =
    (sem_GrTag_Fun _nm )
sem_GrTag (GrTag_Hole )  =
    (sem_GrTag_Hole )
sem_GrTag (GrTag_PApp _needs _nm )  =
    (sem_GrTag_PApp _needs _nm )
sem_GrTag (GrTag_Rec )  =
    (sem_GrTag_Rec )
sem_GrTag (GrTag_Unboxed )  =
    (sem_GrTag_Unboxed )
-- semantic domain
type T_GrTag  = ( FvInfoMp,T_GrTag_1 )
type T_GrTag_1  = IsEvalMp ->
                  ( GrTag )
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_App_1 :: T_GrTag_1 
                 sem_GrTag_App_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_App nm_) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_App_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_Con_1 :: T_GrTag_1 
                 sem_GrTag_Con_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_Con grtgAnn_ int_ nm_) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_Con_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_Fun_1 :: T_GrTag_1 
                 sem_GrTag_Fun_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_Fun nm_) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_Fun_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_Hole_1 :: T_GrTag_1 
                 sem_GrTag_Hole_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_Hole) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_Hole_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_PApp_1 :: T_GrTag_1 
                 sem_GrTag_PApp_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_PApp needs_ nm_) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_PApp_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_Rec_1 :: T_GrTag_1 
                 sem_GrTag_Rec_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_Rec) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_Rec_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrTag_Unboxed_1 :: T_GrTag_1 
                 sem_GrTag_Unboxed_1  =
                     (\ _lhsIisEvalMp ->
                          (case (GrTag_Unboxed) of
                           { _trf ->
                           (case (_trf) of
                            { _lhsOtrf ->
                            ( _lhsOtrf) }) }))
             in  sem_GrTag_Unboxed_1)) of
      { ( sem_GrTag_1) ->
      ( _lhsOgathFviMp,sem_GrTag_1) }) })
-- GrTagL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isEvalMp             : IsEvalMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrTag 
         child tl             : GrTagL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTagL :: GrTagL  ->
              T_GrTagL 
sem_GrTagL list  =
    (Prelude.foldr sem_GrTagL_Cons sem_GrTagL_Nil (Prelude.map sem_GrTag list) )
-- semantic domain
type T_GrTagL  = IsEvalMp ->
                 ( FvInfoMp,GrTagL )
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (\ _lhsIisEvalMp ->
         (case (_lhsIisEvalMp) of
          { _tlOisEvalMp ->
          (case (tl_ _tlOisEvalMp ) of
           { ( _tlIgathFviMp,_tlItrf) ->
               (case (hd_ ) of
                { ( _hdIgathFviMp,hd_1) ->
                    (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                     { _lhsOgathFviMp ->
                     (case (_lhsIisEvalMp) of
                      { _hdOisEvalMp ->
                      (case (hd_1 _hdOisEvalMp ) of
                       { ( _hdItrf) ->
                           (case ((:) _hdItrf _tlItrf) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (\ _lhsIisEvalMp ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case ([]) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrType ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isEvalMp             : IsEvalMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Arrow:
         child args           : GrTypeBaseL 
         child res            : GrTypeBase 
         visit 0:
            local trf         : _
      alternative None:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrType :: GrType  ->
              T_GrType 
sem_GrType (GrType_Arrow _args _res )  =
    (sem_GrType_Arrow (sem_GrTypeBaseL _args ) (sem_GrTypeBase _res ) )
sem_GrType (GrType_None )  =
    (sem_GrType_None )
-- semantic domain
type T_GrType  = IsEvalMp ->
                 ( FvInfoMp,GrType )
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (\ _lhsIisEvalMp ->
         (case (_lhsIisEvalMp) of
          { _resOisEvalMp ->
          (case (res_ _resOisEvalMp ) of
           { ( _resIgathFviMp,_resItrf) ->
               (case (_lhsIisEvalMp) of
                { _argsOisEvalMp ->
                (case (args_ _argsOisEvalMp ) of
                 { ( _argsIgathFviMp,_argsItrf) ->
                     (case (_argsIgathFviMp `fviMpUnion` _resIgathFviMp) of
                      { _lhsOgathFviMp ->
                      (case (GrType_Arrow _argsItrf _resItrf) of
                       { _trf ->
                       (case (_trf) of
                        { _lhsOtrf ->
                        ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (\ _lhsIisEvalMp ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case (GrType_None) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isEvalMp             : IsEvalMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Node:
         visit 0:
            local trf         : _
      alternative Pointer:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTypeBase :: GrTypeBase  ->
                  T_GrTypeBase 
sem_GrTypeBase (GrTypeBase_Node )  =
    (sem_GrTypeBase_Node )
sem_GrTypeBase (GrTypeBase_Pointer )  =
    (sem_GrTypeBase_Pointer )
-- semantic domain
type T_GrTypeBase  = IsEvalMp ->
                     ( FvInfoMp,GrTypeBase )
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (\ _lhsIisEvalMp ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case (GrTypeBase_Node) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (\ _lhsIisEvalMp ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case (GrTypeBase_Pointer) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isEvalMp             : IsEvalMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrTypeBase 
         child tl             : GrTypeBaseL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrTypeBaseL :: GrTypeBaseL  ->
                   T_GrTypeBaseL 
sem_GrTypeBaseL list  =
    (Prelude.foldr sem_GrTypeBaseL_Cons sem_GrTypeBaseL_Nil (Prelude.map sem_GrTypeBase list) )
-- semantic domain
type T_GrTypeBaseL  = IsEvalMp ->
                      ( FvInfoMp,GrTypeBaseL )
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (\ _lhsIisEvalMp ->
         (case (_lhsIisEvalMp) of
          { _tlOisEvalMp ->
          (case (tl_ _tlOisEvalMp ) of
           { ( _tlIgathFviMp,_tlItrf) ->
               (case (_lhsIisEvalMp) of
                { _hdOisEvalMp ->
                (case (hd_ _hdOisEvalMp ) of
                 { ( _hdIgathFviMp,_hdItrf) ->
                     (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                      { _lhsOgathFviMp ->
                      (case ((:) _hdItrf _tlItrf) of
                       { _trf ->
                       (case (_trf) of
                        { _lhsOtrf ->
                        ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (\ _lhsIisEvalMp ->
         (case (Map.empty) of
          { _lhsOgathFviMp ->
          (case ([]) of
           { _trf ->
           (case (_trf) of
            { _lhsOtrf ->
            ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
-- GrVal -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
         willUseFor           : WillUseForS
      synthesized attributes:
         gathIsEvalMp         : IsEvalMp
         isEval               : IsEval
         mbDelayedExpr        : Maybe DelayedExpr
         mbGrExpr             : Maybe GrExpr
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative Empty:
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative LitInt:
         child int            : {Int}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative LitStr:
         child str            : {String}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
         visit 1:
            local willOnlyUseForEval : _
            local _tup3       : _
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
      alternative VarNode:
         child fldL           : GrValL 
         visit 1:
            local mbDelayedExpr : _
            local mbGrExpr    : _
            local trf         : _
-}
-- cata
sem_GrVal :: GrVal  ->
             T_GrVal 
sem_GrVal (GrVal_BasicNode _tag _nm )  =
    (sem_GrVal_BasicNode (sem_GrTag _tag ) _nm )
sem_GrVal (GrVal_Empty )  =
    (sem_GrVal_Empty )
sem_GrVal (GrVal_EnumNode _nm )  =
    (sem_GrVal_EnumNode _nm )
sem_GrVal (GrVal_LitInt _int )  =
    (sem_GrVal_LitInt _int )
sem_GrVal (GrVal_LitStr _str )  =
    (sem_GrVal_LitStr _str )
sem_GrVal (GrVal_Node _tag _fldL )  =
    (sem_GrVal_Node (sem_GrTag _tag ) (sem_GrValL _fldL ) )
sem_GrVal (GrVal_NodeAdapt _nm _fldL )  =
    (sem_GrVal_NodeAdapt _nm (sem_GrAdaptL _fldL ) )
sem_GrVal (GrVal_OpaqueNode _nm )  =
    (sem_GrVal_OpaqueNode _nm )
sem_GrVal (GrVal_PtrNode _nm )  =
    (sem_GrVal_PtrNode _nm )
sem_GrVal (GrVal_Tag _tag )  =
    (sem_GrVal_Tag (sem_GrTag _tag ) )
sem_GrVal (GrVal_Var _nm )  =
    (sem_GrVal_Var _nm )
sem_GrVal (GrVal_VarNode _fldL )  =
    (sem_GrVal_VarNode (sem_GrValL _fldL ) )
-- semantic domain
type T_GrVal  = ( FvInfoMp,T_GrVal_1 )
type T_GrVal_1  = IsEvalMp ->
                  EHCOpts ->
                  WillUseForS ->
                  ( IsEvalMp,IsEval,(Maybe DelayedExpr),(Maybe GrExpr),NmAlias,GrVal )
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,tag_1) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ((let sem_GrVal_BasicNode_1 :: T_GrVal_1 
                      sem_GrVal_BasicNode_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts
                             _lhsIwillUseFor ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (IsEval_None) of
                                 { _lhsOisEval ->
                                 (case (Nothing) of
                                  { _mbDelayedExpr ->
                                  (case (_mbDelayedExpr) of
                                   { _lhsOmbDelayedExpr ->
                                   (case (Nothing) of
                                    { _mbGrExpr ->
                                    (case (_mbGrExpr) of
                                     { _lhsOmbGrExpr ->
                                     (case (NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) BasicAnnot_Dflt) of
                                      { _lhsOnmAlias ->
                                      (case (_lhsIisEvalMp) of
                                       { _tagOisEvalMp ->
                                       (case (tag_1 _tagOisEvalMp ) of
                                        { ( _tagItrf) ->
                                            (case (GrVal_BasicNode _tagItrf nm_) of
                                             { _trf ->
                                             (case (_trf) of
                                              { _lhsOtrf ->
                                              ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_GrVal_BasicNode_1)) of
           { ( sem_GrVal_1) ->
           ( _lhsOgathFviMp,sem_GrVal_1) }) }) })
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_Empty_1 :: T_GrVal_1 
                 sem_GrVal_Empty_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_None) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_None) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_Empty) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_Empty_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_EnumNode_1 :: T_GrVal_1 
                 sem_GrVal_EnumNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_None) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_None) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_EnumNode nm_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_EnumNode_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_LitInt_1 :: T_GrVal_1 
                 sem_GrVal_LitInt_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_EvalToPointer) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_Const hsnUnknown (GrVal_LitInt int_)) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_LitInt int_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_LitInt_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_LitStr_1 :: T_GrVal_1 
                 sem_GrVal_LitStr_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_None) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_Const hsnUnknown (GrVal_LitStr str_)) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_LitStr str_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_LitStr_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp,fldL_1) ->
         (case (tag_ ) of
          { ( _tagIgathFviMp,tag_1) ->
              (case (_tagIgathFviMp `fviMpUnion` _fldLIgathFviMp) of
               { _lhsOgathFviMp ->
               (case ((let sem_GrVal_Node_1 :: T_GrVal_1 
                           sem_GrVal_Node_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts
                                  _lhsIwillUseFor ->
                                    (case (WillUseFor_Eval `Set.member` _lhsIwillUseFor && not (WillUseFor_NodeField `Set.member` _lhsIwillUseFor)) of
                                     { _willOnlyUseForEval ->
                                     (case (_lhsIopts) of
                                      { _fldLOopts ->
                                      (case (_lhsIisEvalMp) of
                                       { _fldLOisEvalMp ->
                                       (case (fldL_1 _fldLOisEvalMp _fldLOopts ) of
                                        { ( _fldLInmAliasL,_fldLItrf) ->
                                            (case (_lhsIisEvalMp) of
                                             { _tagOisEvalMp ->
                                             (case (tag_1 _tagOisEvalMp ) of
                                              { ( _tagItrf) ->
                                                  (case (case (_tagItrf,_fldLItrf) of
                                                           (GrTag_Fun nm,_)
                                                             | _willOnlyUseForEval
                                                             -> (Nothing, IsEval_None, Map.empty, Just (repl, IsEval_EvalToNode, Map.empty))
                                                             where repl = GrExpr_Call nm _fldLItrf
                                                           (GrTag_App _,(GrVal_Var f:fs))
                                                             | _willOnlyUseForEval
                                                             -> ( Nothing
                                                                , IsEval_None
                                                                , Map.empty
                                                                , Just (repl,IsEval_Apply fRepl fs,Map.singleton f (IsEval_EvalToNodeNm f'))
                                                                )
                                                             where f' = hsnUniqifyEval f
                                                                   fRepl = f'
                                                                   repl = GrExpr_Seq (GrExpr_Eval f) (GrPatLam_Var f') (GrExpr_App fRepl fs)
                                                           (GrTag_Con _ _ _,_)
                                                             -> (Nothing, IsEval_EvalToPointer, Map.empty, Nothing)
                                                           (GrTag_Rec ,_)
                                                             -> (Nothing, IsEval_EvalToPointer, Map.empty, Nothing)
                                                           (GrTag_PApp nMiss nm,_)
                                                             | _willOnlyUseForEval && nMiss == 0
                                                               -> (Nothing, IsEval_None, Map.empty, Just (GrExpr_Call nm _fldLItrf, IsEval_EvalToNode, Map.empty))
                                                             | otherwise
                                                               -> (Nothing, IsEval_EvalToPointer, Map.empty, Nothing)
                                                           (t,_)
                                                             -> (Nothing, IsEval_None, Map.empty, Nothing)) of
                                                   { __tup3 ->
                                                   (case (__tup3) of
                                                    { (_,_,_lhsOgathIsEvalMp,_) ->
                                                    (case (__tup3) of
                                                     { (_,_lhsOisEval,_,_) ->
                                                     (case (__tup3) of
                                                      { (_,_,_,_mbDelayedExpr) ->
                                                      (case (_mbDelayedExpr) of
                                                       { _lhsOmbDelayedExpr ->
                                                       (case (__tup3) of
                                                        { (_mbGrExpr,_,_,_) ->
                                                        (case (_mbGrExpr) of
                                                         { _lhsOmbGrExpr ->
                                                         (case (case _tagItrf of
                                                                  GrTag_Con _ _ _
                                                                    -> NmAlias_Grp hsnUnknown _fldLInmAliasL
                                                                  _ -> NmAlias_None) of
                                                          { _lhsOnmAlias ->
                                                          (case (GrVal_Node _tagItrf _fldLItrf) of
                                                           { _trf ->
                                                           (case (_trf) of
                                                            { _lhsOtrf ->
                                                            ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                       in  sem_GrVal_Node_1)) of
                { ( sem_GrVal_1) ->
                ( _lhsOgathFviMp,sem_GrVal_1) }) }) }) })
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp,fldL_1) ->
         (case (fviMpUnions [fviMpSingleton nm_, _fldLIgathFviMp]) of
          { _lhsOgathFviMp ->
          (case ((let sem_GrVal_NodeAdapt_1 :: T_GrVal_1 
                      sem_GrVal_NodeAdapt_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts
                             _lhsIwillUseFor ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (IsEval_None) of
                                 { _lhsOisEval ->
                                 (case (Nothing) of
                                  { _mbDelayedExpr ->
                                  (case (_mbDelayedExpr) of
                                   { _lhsOmbDelayedExpr ->
                                   (case (Nothing) of
                                    { _mbGrExpr ->
                                    (case (_mbGrExpr) of
                                     { _lhsOmbGrExpr ->
                                     (case (NmAlias_None) of
                                      { _lhsOnmAlias ->
                                      (case (_lhsIopts) of
                                       { _fldLOopts ->
                                       (case (_lhsIisEvalMp) of
                                        { _fldLOisEvalMp ->
                                        (case (fldL_1 _fldLOisEvalMp _fldLOopts ) of
                                         { ( _fldLItrf) ->
                                             (case (GrVal_NodeAdapt nm_ _fldLItrf) of
                                              { _trf ->
                                              (case (_trf) of
                                               { _lhsOtrf ->
                                               ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_GrVal_NodeAdapt_1)) of
           { ( sem_GrVal_1) ->
           ( _lhsOgathFviMp,sem_GrVal_1) }) }) })
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_OpaqueNode_1 :: T_GrVal_1 
                 sem_GrVal_OpaqueNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_None) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_None) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_OpaqueNode nm_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_OpaqueNode_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_PtrNode_1 :: T_GrVal_1 
                 sem_GrVal_PtrNode_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (IsEval_None) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_None) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_PtrNode nm_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_PtrNode_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (case (tag_ ) of
     { ( _tagIgathFviMp,tag_1) ->
         (case (_tagIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ((let sem_GrVal_Tag_1 :: T_GrVal_1 
                      sem_GrVal_Tag_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts
                             _lhsIwillUseFor ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (IsEval_None) of
                                 { _lhsOisEval ->
                                 (case (Nothing) of
                                  { _mbDelayedExpr ->
                                  (case (_mbDelayedExpr) of
                                   { _lhsOmbDelayedExpr ->
                                   (case (Nothing) of
                                    { _mbGrExpr ->
                                    (case (_mbGrExpr) of
                                     { _lhsOmbGrExpr ->
                                     (case (NmAlias_None) of
                                      { _lhsOnmAlias ->
                                      (case (_lhsIisEvalMp) of
                                       { _tagOisEvalMp ->
                                       (case (tag_1 _tagOisEvalMp ) of
                                        { ( _tagItrf) ->
                                            (case (GrVal_Tag _tagItrf) of
                                             { _trf ->
                                             (case (_trf) of
                                              { _lhsOtrf ->
                                              ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_GrVal_Tag_1)) of
           { ( sem_GrVal_1) ->
           ( _lhsOgathFviMp,sem_GrVal_1) }) }) })
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (case (fviMpSingleton' FvUse_Val nm_) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrVal_Var_1 :: T_GrVal_1 
                 sem_GrVal_Var_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts
                        _lhsIwillUseFor ->
                          (case (Map.empty) of
                           { _lhsOgathIsEvalMp ->
                           (case (maybe IsEval_None
                                        id
                                        (Map.lookup nm_ _lhsIisEvalMp)) of
                            { _lhsOisEval ->
                            (case (Nothing) of
                             { _mbDelayedExpr ->
                             (case (_mbDelayedExpr) of
                              { _lhsOmbDelayedExpr ->
                              (case (Nothing) of
                               { _mbGrExpr ->
                               (case (_mbGrExpr) of
                                { _lhsOmbGrExpr ->
                                (case (NmAlias_Nm nm_) of
                                 { _lhsOnmAlias ->
                                 (case (GrVal_Var nm_) of
                                  { _trf ->
                                  (case (_trf) of
                                   { _lhsOtrf ->
                                   ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }))
             in  sem_GrVal_Var_1)) of
      { ( sem_GrVal_1) ->
      ( _lhsOgathFviMp,sem_GrVal_1) }) })
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (case (fldL_ ) of
     { ( _fldLIgathFviMp,fldL_1) ->
         (case (_fldLIgathFviMp) of
          { _lhsOgathFviMp ->
          (case ((let sem_GrVal_VarNode_1 :: T_GrVal_1 
                      sem_GrVal_VarNode_1  =
                          (\ _lhsIisEvalMp
                             _lhsIopts
                             _lhsIwillUseFor ->
                               (case (Map.empty) of
                                { _lhsOgathIsEvalMp ->
                                (case (IsEval_None) of
                                 { _lhsOisEval ->
                                 (case (Nothing) of
                                  { _mbDelayedExpr ->
                                  (case (_mbDelayedExpr) of
                                   { _lhsOmbDelayedExpr ->
                                   (case (Nothing) of
                                    { _mbGrExpr ->
                                    (case (_mbGrExpr) of
                                     { _lhsOmbGrExpr ->
                                     (case (NmAlias_None) of
                                      { _lhsOnmAlias ->
                                      (case (_lhsIopts) of
                                       { _fldLOopts ->
                                       (case (_lhsIisEvalMp) of
                                        { _fldLOisEvalMp ->
                                        (case (fldL_1 _fldLOisEvalMp _fldLOopts ) of
                                         { ( _fldLInmAliasL,_fldLItrf) ->
                                             (case (GrVal_VarNode _fldLItrf) of
                                              { _trf ->
                                              (case (_trf) of
                                               { _lhsOtrf ->
                                               ( _lhsOgathIsEvalMp,_lhsOisEval,_lhsOmbDelayedExpr,_lhsOmbGrExpr,_lhsOnmAlias,_lhsOtrf) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_GrVal_VarNode_1)) of
           { ( sem_GrVal_1) ->
           ( _lhsOgathFviMp,sem_GrVal_1) }) }) })
-- GrValL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathFviMp            : FvInfoMp
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         nmAliasL             : [NmAlias]
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = ( FvInfoMp,T_GrValL_1 )
type T_GrValL_1  = IsEvalMp ->
                   EHCOpts ->
                   ( ([NmAlias]),GrValL )
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathFviMp,tl_1) ->
         (case (hd_ ) of
          { ( _hdIgathFviMp,hd_1) ->
              (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
               { _lhsOgathFviMp ->
               (case ((let sem_GrValL_Cons_1 :: T_GrValL_1 
                           sem_GrValL_Cons_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (_lhsIopts) of
                                     { _tlOopts ->
                                     (case (_lhsIisEvalMp) of
                                      { _tlOisEvalMp ->
                                      (case (tl_1 _tlOisEvalMp _tlOopts ) of
                                       { ( _tlInmAliasL,_tlItrf) ->
                                           (case (_lhsIopts) of
                                            { _hdOopts ->
                                            (case (_lhsIisEvalMp) of
                                             { _hdOisEvalMp ->
                                             (case (Set.empty) of
                                              { _hdOwillUseFor ->
                                              (case (hd_1 _hdOisEvalMp _hdOopts _hdOwillUseFor ) of
                                               { ( _hdIgathIsEvalMp,_hdIisEval,_hdImbDelayedExpr,_hdImbGrExpr,_hdInmAlias,_hdItrf) ->
                                                   (case (_hdInmAlias : _tlInmAliasL) of
                                                    { _lhsOnmAliasL ->
                                                    (case ((:) _hdItrf _tlItrf) of
                                                     { _trf ->
                                                     (case (_trf) of
                                                      { _lhsOtrf ->
                                                      ( _lhsOnmAliasL,_lhsOtrf) }) }) }) }) }) }) }) }) }) }))
                       in  sem_GrValL_Cons_1)) of
                { ( sem_GrValL_1) ->
                ( _lhsOgathFviMp,sem_GrValL_1) }) }) }) })
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (case (Map.empty) of
     { _lhsOgathFviMp ->
     (case ((let sem_GrValL_Nil_1 :: T_GrValL_1 
                 sem_GrValL_Nil_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case ([]) of
                           { _lhsOnmAliasL ->
                           (case ([]) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOnmAliasL,_lhsOtrf) }) }) }))
             in  sem_GrValL_Nil_1)) of
      { ( sem_GrValL_1) ->
      ( _lhsOgathFviMp,sem_GrValL_1) }) })
-- GrVar -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Ignore:
         visit 1:
            local trf         : _
      alternative KnownTag:
         child tag            : GrTag 
         visit 1:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 1:
            local trf         : _
-}
-- cata
sem_GrVar :: GrVar  ->
             T_GrVar 
sem_GrVar (GrVar_Ignore )  =
    (sem_GrVar_Ignore )
sem_GrVar (GrVar_KnownTag _tag )  =
    (sem_GrVar_KnownTag (sem_GrTag _tag ) )
sem_GrVar (GrVar_Var _nm )  =
    (sem_GrVar_Var _nm )
-- semantic domain
type T_GrVar  = ( ([HsName]),T_GrVar_1 )
type T_GrVar_1  = IsEvalMp ->
                  EHCOpts ->
                  ( FvInfoMp,GrVar )
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (case ([ ]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_Ignore_1 :: T_GrVar_1 
                 sem_GrVar_Ignore_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (GrVar_Ignore) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
             in  sem_GrVar_Ignore_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (case ([ error "introNmL known tag" ]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_KnownTag_1 :: T_GrVar_1 
                 sem_GrVar_KnownTag_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (tag_ ) of
                           { ( _tagIgathFviMp,tag_1) ->
                               (case (_tagIgathFviMp) of
                                { _lhsOgathFviMp ->
                                (case (_lhsIisEvalMp) of
                                 { _tagOisEvalMp ->
                                 (case (tag_1 _tagOisEvalMp ) of
                                  { ( _tagItrf) ->
                                      (case (GrVar_KnownTag _tagItrf) of
                                       { _trf ->
                                       (case (_trf) of
                                        { _lhsOtrf ->
                                        ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }))
             in  sem_GrVar_KnownTag_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (case ([nm_]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVar_Var_1 :: T_GrVar_1 
                 sem_GrVar_Var_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case (GrVar_Var nm_) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
             in  sem_GrVar_Var_1)) of
      { ( sem_GrVar_1) ->
      ( _lhsOintroNmL,sem_GrVar_1) }) })
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         introNmL             : [HsName]
   visit 1:
      inherited attributes:
         isEvalMp             : IsEvalMp
         opts                 : EHCOpts
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
         visit 1:
            local trf         : _
      alternative Nil:
         visit 1:
            local trf         : _
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ( ([HsName]),T_GrVarL_1 )
type T_GrVarL_1  = IsEvalMp ->
                   EHCOpts ->
                   ( FvInfoMp,GrVarL )
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIintroNmL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIintroNmL,hd_1) ->
              (case (_hdIintroNmL ++ _tlIintroNmL) of
               { _lhsOintroNmL ->
               (case ((let sem_GrVarL_Cons_1 :: T_GrVarL_1 
                           sem_GrVarL_Cons_1  =
                               (\ _lhsIisEvalMp
                                  _lhsIopts ->
                                    (case (_lhsIopts) of
                                     { _tlOopts ->
                                     (case (_lhsIisEvalMp) of
                                      { _tlOisEvalMp ->
                                      (case (tl_1 _tlOisEvalMp _tlOopts ) of
                                       { ( _tlIgathFviMp,_tlItrf) ->
                                           (case (_lhsIopts) of
                                            { _hdOopts ->
                                            (case (_lhsIisEvalMp) of
                                             { _hdOisEvalMp ->
                                             (case (hd_1 _hdOisEvalMp _hdOopts ) of
                                              { ( _hdIgathFviMp,_hdItrf) ->
                                                  (case (_hdIgathFviMp `fviMpUnion` _tlIgathFviMp) of
                                                   { _lhsOgathFviMp ->
                                                   (case ((:) _hdItrf _tlItrf) of
                                                    { _trf ->
                                                    (case (_trf) of
                                                     { _lhsOtrf ->
                                                     ( _lhsOgathFviMp,_lhsOtrf) }) }) }) }) }) }) }) }) }))
                       in  sem_GrVarL_Cons_1)) of
                { ( sem_GrVarL_1) ->
                ( _lhsOintroNmL,sem_GrVarL_1) }) }) }) })
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (case ([]) of
     { _lhsOintroNmL ->
     (case ((let sem_GrVarL_Nil_1 :: T_GrVarL_1 
                 sem_GrVarL_Nil_1  =
                     (\ _lhsIisEvalMp
                        _lhsIopts ->
                          (case (Map.empty) of
                           { _lhsOgathFviMp ->
                           (case ([]) of
                            { _trf ->
                            (case (_trf) of
                             { _lhsOtrf ->
                             ( _lhsOgathFviMp,_lhsOtrf) }) }) }))
             in  sem_GrVarL_Nil_1)) of
      { ( sem_GrVarL_1) ->
      ( _lhsOintroNmL,sem_GrVarL_1) }) })
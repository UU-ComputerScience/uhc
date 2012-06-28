

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace)
module EH101.Core.Trf.ExplicitStackTrace(cmodTrfExplicitStackTrace) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.Core
import EH101.Ty
import EH101.LamInfo
import EH101.AbstractCore
import Data.Maybe
import qualified Data.Map as Map











cmodTrfExplicitStackTrace :: EHCOpts -> LamMp -> CModule -> (CModule,LamMp)
cmodTrfExplicitStackTrace opts lamMp cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf
                               { opts_Inh_CodeAGItf = opts
                               , lamMp_Inh_CodeAGItf = lamMp
                               })
     in   ( cTrf_Syn_CodeAGItf t
          , debugLamMp_Syn_CodeAGItf t `Map.union`
            gathLamMp_Syn_CodeAGItf  t
            -- [ | (n,) <- debugLamMp_Syn_CodeAGItf t ]
          )



mbStackTrace :: EHCOpts -> Bool -> HsName -> StackTraceInfo
mbStackTrace opts isCand nm
  | isCand && ehcOptTargetFlavor opts == TargetFlavor_Debug
              = StackTraceInfo_HasStackTraceEquiv (hsnSuffix nm "-$explstacktraced")
  | otherwise = StackTraceInfo_None

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local lev         : _
            local whatAbove   : {WhatExpr}
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
               LamMp ->
               Int ->
               (Maybe HsName) ->
               EHCOpts ->
               Int ->
               ( CAlt ,LamMp,Int)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CAlt 
              _lhsOuniq :: Int
              _patOlamMp :: LamMp
              _patOlev :: Int
              _patOmbInStackTraceCtxt :: (Maybe HsName)
              _patOopts :: EHCOpts
              _patOuniq :: Int
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _patIcTrf :: CPat 
              _patIdebugLamMp :: LamMp
              _patIfldNmL :: ([HsName])
              _patIuniq :: Int
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 10, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 82, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _patIdebugLamMp `Map.union` _exprIdebugLamMp
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _patOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _patOlev =
                  _lev
              -- copy rule (down)
              _patOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _patOopts =
                  _lhsIopts
              -- copy rule (down)
              _patOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _exprOlev =
                  _lev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exprOuniq =
                  _patIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _patIcTrf,_patIdebugLamMp,_patIfldNmL,_patIuniq) =
                  pat_ _patOlamMp _patOlev _patOmbInStackTraceCtxt _patOopts _patOuniq 
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                LamMp ->
                Int ->
                (Maybe HsName) ->
                EHCOpts ->
                Int ->
                ( CAltL ,LamMp,Int)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CAltL 
              _lhsOuniq :: Int
              _hdOevalCtx :: EvalCtx
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOmbInStackTraceCtxt :: (Maybe HsName)
              _hdOopts :: EHCOpts
              _hdOuniq :: Int
              _tlOevalCtx :: EvalCtx
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOmbInStackTraceCtxt :: (Maybe HsName)
              _tlOopts :: EHCOpts
              _tlOuniq :: Int
              _hdIcTrf :: CAlt 
              _hdIdebugLamMp :: LamMp
              _hdIuniq :: Int
              _tlIcTrf :: CAltL 
              _tlIdebugLamMp :: LamMp
              _tlIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _hdIdebugLamMp `Map.union` _tlIdebugLamMp
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIcTrf,_hdIdebugLamMp,_hdIuniq) =
                  hd_ _hdOevalCtx _hdOisLamBody _hdOisStrict _hdOlamMp _hdOlev _hdOmbInStackTraceCtxt _hdOopts _hdOuniq 
              ( _tlIcTrf,_tlIdebugLamMp,_tlIuniq) =
                  tl_ _tlOevalCtx _tlOisLamBody _tlOisStrict _tlOlamMp _tlOlev _tlOmbInStackTraceCtxt _tlOopts _tlOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CAltL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         bindCTrfL            : [CBind]
         bindLamMp            : LamMp
         cTrf                 : SELF 
         debugLamMp           : LamMp
         nm                   : HsName
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cTrf        : {CBind}
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
                LamMp ->
                CBindCateg ->
                Int ->
                (Maybe HsName) ->
                EHCOpts ->
                Int ->
                ( ([CBind]),LamMp,CBind ,LamMp,HsName,Int)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBind
              _lhsOcTrf :: CBind 
              _lhsOuniq :: Int
              _bindAspectsOevalCtx :: EvalCtx
              _bindAspectsOisGlobal :: Bool
              _bindAspectsOisLamBody :: Bool
              _bindAspectsOisStrict :: Bool
              _bindAspectsOlamMp :: LamMp
              _bindAspectsOletBindingsCateg :: CBindCateg
              _bindAspectsOlev :: Int
              _bindAspectsOmbInStackTraceCtxt :: (Maybe HsName)
              _bindAspectsOopts :: EHCOpts
              _bindAspectsOuniq :: Int
              _bindAspectsIbindCTrfL :: ([CBind])
              _bindAspectsIbindLamMp :: LamMp
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIdebugLamMp :: LamMp
              _bindAspectsIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 4, column 17)
              _bindAspectsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 12, column 17)
              _lhsOnm =
                  nm_
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindAspectsIbindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _bindAspectsIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _bindAspectsIdebugLamMp
              -- self rule
              _cTrf =
                  CBind_Bind nm_ _bindAspectsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bindAspectsIuniq
              -- copy rule (down)
              _bindAspectsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bindAspectsOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _bindAspectsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindAspectsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindAspectsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindAspectsOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _bindAspectsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindAspectsOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bindAspectsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindAspectsOuniq =
                  _lhsIuniq
              ( _bindAspectsIbindCTrfL,_bindAspectsIbindLamMp,_bindAspectsIcTrf,_bindAspectsIdebugLamMp,_bindAspectsIuniq) =
                  bindAspects_ _bindAspectsOevalCtx _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOlamMp _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOmbInStackTraceCtxt _bindAspectsOnm _bindAspectsOopts _bindAspectsOuniq 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOnm,_lhsOuniq)))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                   Int ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   Int ->
                   ( CBindAnn ,LamMp,Int)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBindAnn 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CBindAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                    Int ->
                    (Maybe HsName) ->
                    EHCOpts ->
                    Int ->
                    ( CBindAnnL ,LamMp,Int)
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBindAnnL 
              _lhsOuniq :: Int
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOmbInStackTraceCtxt :: (Maybe HsName)
              _hdOopts :: EHCOpts
              _hdOuniq :: Int
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOmbInStackTraceCtxt :: (Maybe HsName)
              _tlOopts :: EHCOpts
              _tlOuniq :: Int
              _hdIcTrf :: CBindAnn 
              _hdIdebugLamMp :: LamMp
              _hdIuniq :: Int
              _tlIcTrf :: CBindAnnL 
              _tlIdebugLamMp :: LamMp
              _tlIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _hdIdebugLamMp `Map.union` _tlIdebugLamMp
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIcTrf,_hdIdebugLamMp,_hdIuniq) =
                  hd_ _hdOlamMp _hdOlev _hdOmbInStackTraceCtxt _hdOopts _hdOuniq 
              ( _tlIcTrf,_tlIdebugLamMp,_tlIuniq) =
                  tl_ _tlOlamMp _tlOlev _tlOmbInStackTraceCtxt _tlOopts _tlOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBindAnnL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         bindCTrfL            : [CBind]
         bindLamMp            : LamMp
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                 LamMp ->
                 CBindCateg ->
                 Int ->
                 (Maybe HsName) ->
                 EHCOpts ->
                 Int ->
                 ( ([CBind]),LamMp,CBindL ,LamMp,Int)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBindL 
              _lhsOuniq :: Int
              _hdOevalCtx :: EvalCtx
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamMp :: LamMp
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOmbInStackTraceCtxt :: (Maybe HsName)
              _hdOopts :: EHCOpts
              _hdOuniq :: Int
              _tlOevalCtx :: EvalCtx
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamMp :: LamMp
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOmbInStackTraceCtxt :: (Maybe HsName)
              _tlOopts :: EHCOpts
              _tlOuniq :: Int
              _hdIbindCTrfL :: ([CBind])
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBind 
              _hdIdebugLamMp :: LamMp
              _hdInm :: HsName
              _hdIuniq :: Int
              _tlIbindCTrfL :: ([CBind])
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBindL 
              _tlIdebugLamMp :: LamMp
              _tlIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _hdIbindCTrfL ++ _tlIbindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _hdIdebugLamMp `Map.union` _tlIdebugLamMp
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIbindCTrfL,_hdIbindLamMp,_hdIcTrf,_hdIdebugLamMp,_hdInm,_hdIuniq) =
                  hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOmbInStackTraceCtxt _hdOopts _hdOuniq 
              ( _tlIbindCTrfL,_tlIbindLamMp,_tlIcTrf,_tlIdebugLamMp,_tlIuniq) =
                  tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOmbInStackTraceCtxt _tlOopts _tlOuniq 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBindL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         nm                   : HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         bindCTrfL            : [CBind]
         bindLamMp            : LamMp
         cTrf                 : SELF 
         debugLamMp           : LamMp
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local isTraceCandidate : _
            local _tup1       : _
            local mbStackTrace : _
            local uniq        : _
            local nmStackTrace : _
            local _tup2       : _
            local bindCTrfL   : _
            local mbInStackTraceCtxt : _
            local newLamMp    : _
            local whatAbove   : {WhatExpr}
            local cTrf        : {CBound}
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local bindCTrfL   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : {CBound}
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local bindCTrfL   : _
            local cTrf        : {CBound}
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local bindCTrfL   : _
            local cTrf        : {CBound}
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local bindCTrfL   : _
            local cTrf        : {CBound}
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local bindCTrfL   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : {CBound}
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
                 LamMp ->
                 CBindCateg ->
                 Int ->
                 (Maybe HsName) ->
                 HsName ->
                 EHCOpts ->
                 Int ->
                 ( ([CBind]),LamMp,CBound ,LamMp,Int)
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindCTrfL :: ([CBind])
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              _bindMetaOlamMp :: LamMp
              _bindMetaOlev :: Int
              _bindMetaOmbInStackTraceCtxt :: (Maybe HsName)
              _bindMetaOopts :: EHCOpts
              _bindMetaOuniq :: Int
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _bindMetaIcTrf :: CMetas 
              _bindMetaIdebugLamMp :: LamMp
              _bindMetaIuniq :: Int
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 77, column 17)
              _isTraceCandidate =
                  _lhsIisGlobal || _exprIisTraceCandidate
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 95, column 17)
              __tup1 =
                  let mbt = mbStackTrace _lhsIopts _isTraceCandidate _lhsInm
                  in  (mbt,Map.singleton _lhsInm (emptyLamInfo {laminfoStackTrace = mbt}))
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 95, column 17)
              (_mbStackTrace,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 95, column 17)
              (_,_lhsObindLamMp) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 105, column 17)
              _uniq =
                  _lhsIuniq + 1
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 105, column 17)
              _nmStackTrace =
                  mkHNm $ "_$" ++ show _lhsIuniq ++ "_stacktrace"
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 111, column 17)
              __tup2 =
                  case _mbStackTrace of
                    StackTraceInfo_HasStackTraceEquiv dbNm | _isTraceCandidate
                      -> ( [ acoreBind1Cat CBindCateg_Plain dbNm (acoreLam1 _nmStackTrace _exprIcTrf)
                           , acoreBind1MetasTy _lhsInm _bindMetaIcTrf Ty_Any
                             $ acoreLam   _exprItraceCandidateLamArgs
                             $ acoreApp   (acoreVar dbNm)
                                          ( acoreVar (ehcOptBuiltin _lhsIopts ehbnDataListAltNil)
                                          : map acoreVar _exprItraceCandidateLamArgs
                                          )
                           ]
                         , Just _nmStackTrace
                         , Map.fromList
                             [ (dbNm, emptyLamInfo {laminfoStackTrace = StackTraceInfo_IsStackTraceEquiv _lhsInm})
                             ]
                         )
                    _ -> ([acoreBind1Asp1 _lhsInm _cTrf], _lhsImbInStackTraceCtxt, Map.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 111, column 17)
              (_bindCTrfL,_,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 111, column 17)
              (_,_mbInStackTraceCtxt,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 111, column 17)
              (_,_,_newLamMp) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 150, column 17)
              _lhsOdebugLamMp =
                  _newLamMp `Map.union` _exprIdebugLamMp
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- self rule
              _cTrf =
                  CBound_Bind _bindMetaIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (from local)
              _lhsOuniq =
                  _uniq
              -- copy rule (down)
              _bindMetaOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindMetaOlev =
                  _lhsIlev
              -- copy rule (from local)
              _bindMetaOmbInStackTraceCtxt =
                  _mbInStackTraceCtxt
              -- copy rule (down)
              _bindMetaOopts =
                  _lhsIopts
              -- copy rule (from local)
              _bindMetaOuniq =
                  _uniq
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (from local)
              _exprOmbInStackTraceCtxt =
                  _mbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (from local)
              _exprOuniq =
                  _uniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _bindMetaIcTrf,_bindMetaIdebugLamMp,_bindMetaIuniq) =
                  bindMeta_ _bindMetaOlamMp _bindMetaOlev _bindMetaOmbInStackTraceCtxt _bindMetaOopts _bindMetaOuniq 
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 128, column 17)
              _bindCTrfL =
                  [acoreBind1Asp1 _lhsInm _cTrf]
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 76, column 17)
              _whatAbove =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp
              -- self rule
              _cTrf =
                  CBound_FFE callconv_ expEnt_ _exprIcTrf ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              _cmetasOlamMp :: LamMp
              _cmetasOlev :: Int
              _cmetasOmbInStackTraceCtxt :: (Maybe HsName)
              _cmetasOopts :: EHCOpts
              _cmetasOuniq :: Int
              _cmetasIcTrf :: CMetas 
              _cmetasIdebugLamMp :: LamMp
              _cmetasIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 128, column 17)
              _bindCTrfL =
                  [acoreBind1Asp1 _lhsInm _cTrf]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _cmetasIdebugLamMp
              -- self rule
              _cTrf =
                  CBound_Meta aspectKeyS_ _cmetasIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _cmetasIuniq
              -- copy rule (down)
              _cmetasOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _cmetasOlev =
                  _lhsIlev
              -- copy rule (down)
              _cmetasOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _cmetasOopts =
                  _lhsIopts
              -- copy rule (down)
              _cmetasOuniq =
                  _lhsIuniq
              ( _cmetasIcTrf,_cmetasIdebugLamMp,_cmetasIuniq) =
                  cmetas_ _cmetasOlamMp _cmetasOlev _cmetasOmbInStackTraceCtxt _cmetasOopts _cmetasOuniq 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 128, column 17)
              _bindCTrfL =
                  [acoreBind1Asp1 _lhsInm _cTrf]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CBound_RelevTy aspectKeyS_ relevTy_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 128, column 17)
              _bindCTrfL =
                  [acoreBind1Asp1 _lhsInm _cTrf]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CBound_Ty aspectKeyS_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
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
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _cTrf :: CBound
              _lhsOcTrf :: CBound 
              _lhsOuniq :: Int
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 128, column 17)
              _bindCTrfL =
                  [acoreBind1Asp1 _lhsInm _cTrf]
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _bindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp
              -- self rule
              _cTrf =
                  CBound_Val aspectKeyS_ _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         nm                   : HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         bindCTrfL            : [CBind]
         bindLamMp            : LamMp
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                  LamMp ->
                  CBindCateg ->
                  Int ->
                  (Maybe HsName) ->
                  HsName ->
                  EHCOpts ->
                  Int ->
                  ( ([CBind]),LamMp,CBoundL ,LamMp,Int)
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _hdOisTopApp :: Bool
              _hdOisTopTup :: Bool
              _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBoundL 
              _lhsOuniq :: Int
              _hdOevalCtx :: EvalCtx
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamMp :: LamMp
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOmbInStackTraceCtxt :: (Maybe HsName)
              _hdOnm :: HsName
              _hdOopts :: EHCOpts
              _hdOuniq :: Int
              _tlOevalCtx :: EvalCtx
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamMp :: LamMp
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOmbInStackTraceCtxt :: (Maybe HsName)
              _tlOnm :: HsName
              _tlOopts :: EHCOpts
              _tlOuniq :: Int
              _hdIbindCTrfL :: ([CBind])
              _hdIbindLamMp :: LamMp
              _hdIcTrf :: CBound 
              _hdIdebugLamMp :: LamMp
              _hdIuniq :: Int
              _tlIbindCTrfL :: ([CBind])
              _tlIbindLamMp :: LamMp
              _tlIcTrf :: CBoundL 
              _tlIdebugLamMp :: LamMp
              _tlIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopTup =
                  True
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  _hdIbindCTrfL ++ _tlIbindCTrfL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  _hdIbindLamMp `lamMpUnionBindAspMp` _tlIbindLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _hdIdebugLamMp `Map.union` _tlIdebugLamMp
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIbindCTrfL,_hdIbindLamMp,_hdIcTrf,_hdIdebugLamMp,_hdIuniq) =
                  hd_ _hdOevalCtx _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOlamMp _hdOletBindingsCateg _hdOlev _hdOmbInStackTraceCtxt _hdOnm _hdOopts _hdOuniq 
              ( _tlIbindCTrfL,_tlIbindLamMp,_tlIcTrf,_tlIdebugLamMp,_tlIuniq) =
                  tl_ _tlOevalCtx _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamMp _tlOletBindingsCateg _tlOlev _tlOmbInStackTraceCtxt _tlOnm _tlOopts _tlOuniq 
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIevalCtx
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsInm
       _lhsIopts
       _lhsIuniq ->
         (let _lhsObindCTrfL :: ([CBind])
              _lhsObindLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CBoundL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 108, column 30)
              _lhsObindCTrfL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 11, column 30)
              _lhsObindLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsObindCTrfL,_lhsObindLamMp,_lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
         whatAbove            : WhatExpr
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
         gathLamMp            : LamMp
         isTraceCandidate     : Bool
         traceCandidateLamArgs : [HsName]
         traceCandidateLamBody : CExpr 
         whatBelow            : WhatExpr
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
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopTup    : _
            local whatBelow   : _
            local isTopApp'   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local lev         : _
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local argNm       : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local isTopLet    : _
            local evalCtx     : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local nm          : {HsName}
            local nmAsp       : {HsName}
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
type T_CExpr  = EvalCtx ->
                Bool ->
                Bool ->
                Bool ->
                Bool ->
                LamMp ->
                Int ->
                (Maybe HsName) ->
                EHCOpts ->
                Int ->
                WhatExpr ->
                ( CExpr ,LamMp,LamMp,Bool,([HsName]),CExpr ,Int,WhatExpr)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOgathLamMp :: LamMp
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _annOlamMp :: LamMp
              _annOlev :: Int
              _annOmbInStackTraceCtxt :: (Maybe HsName)
              _annOopts :: EHCOpts
              _annOuniq :: Int
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _annIcTrf :: CExprAnn 
              _annIdebugLamMp :: LamMp
              _annIuniq :: Int
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _annIdebugLamMp `Map.union` _exprIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_Ann _annIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (up)
              _lhsOwhatBelow =
                  _exprIwhatBelow
              -- copy rule (down)
              _annOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _annOlev =
                  _lhsIlev
              -- copy rule (down)
              _annOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _annOopts =
                  _lhsIopts
              -- copy rule (down)
              _annOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exprOuniq =
                  _annIuniq
              -- copy rule (down)
              _exprOwhatAbove =
                  _lhsIwhatAbove
              ( _annIcTrf,_annIdebugLamMp,_annIuniq) =
                  ann_ _annOlamMp _annOlev _annOmbInStackTraceCtxt _annOopts _annOuniq 
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _funcOisTopApp :: Bool
              _argOisTopApp :: Bool
              _whatAbove :: WhatExpr
              _argOnm :: HsName
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _funcOevalCtx :: EvalCtx
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopTup :: Bool
              _funcOlamMp :: LamMp
              _funcOlev :: Int
              _funcOmbInStackTraceCtxt :: (Maybe HsName)
              _funcOopts :: EHCOpts
              _funcOuniq :: Int
              _funcOwhatAbove :: WhatExpr
              _argOevalCtx :: EvalCtx
              _argOisGlobal :: Bool
              _argOisLamBody :: Bool
              _argOisStrict :: Bool
              _argOisTopTup :: Bool
              _argOlamMp :: LamMp
              _argOletBindingsCateg :: CBindCateg
              _argOlev :: Int
              _argOmbInStackTraceCtxt :: (Maybe HsName)
              _argOopts :: EHCOpts
              _argOuniq :: Int
              _funcIcTrf :: CExpr 
              _funcIdebugLamMp :: LamMp
              _funcIgathLamMp :: LamMp
              _funcIisTraceCandidate :: Bool
              _funcItraceCandidateLamArgs :: ([HsName])
              _funcItraceCandidateLamBody :: CExpr 
              _funcIuniq :: Int
              _funcIwhatBelow :: WhatExpr
              _argIbindCTrfL :: ([CBind])
              _argIbindLamMp :: LamMp
              _argIcTrf :: CBound 
              _argIdebugLamMp :: LamMp
              _argIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 13, column 17)
              _funcOisTopApp =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 14, column 17)
              _argOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 56, column 17)
              _whatBelow =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 61, column 17)
              _isTopApp' =
                  isNothing $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 67, column 17)
              _whatAbove =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 7, column 17)
              _argOnm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _funcIdebugLamMp `Map.union` _argIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_App _funcIcTrf _argIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _argIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              -- copy rule (down)
              _funcOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _argOevalCtx =
                  _lhsIevalCtx
              -- copy rule (from local)
              _argOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _argOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _argOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _argOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _argOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _argOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _argOlev =
                  _lhsIlev
              -- copy rule (down)
              _argOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _argOopts =
                  _lhsIopts
              -- copy rule (chain)
              _argOuniq =
                  _funcIuniq
              ( _funcIcTrf,_funcIdebugLamMp,_funcIgathLamMp,_funcIisTraceCandidate,_funcItraceCandidateLamArgs,_funcItraceCandidateLamBody,_funcIuniq,_funcIwhatBelow) =
                  func_ _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamMp _funcOlev _funcOmbInStackTraceCtxt _funcOopts _funcOuniq _funcOwhatAbove 
              ( _argIbindCTrfL,_argIbindLamMp,_argIcTrf,_argIdebugLamMp,_argIuniq) =
                  arg_ _argOevalCtx _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOlamMp _argOletBindingsCateg _argOlev _argOmbInStackTraceCtxt _argOnm _argOopts _argOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _altsOevalCtx :: EvalCtx
              _altsOisLamBody :: Bool
              _altsOisStrict :: Bool
              _altsOlamMp :: LamMp
              _altsOlev :: Int
              _altsOmbInStackTraceCtxt :: (Maybe HsName)
              _altsOopts :: EHCOpts
              _altsOuniq :: Int
              _dfltOevalCtx :: EvalCtx
              _dfltOisLamBody :: Bool
              _dfltOisStrict :: Bool
              _dfltOisTopApp :: Bool
              _dfltOisTopTup :: Bool
              _dfltOlamMp :: LamMp
              _dfltOlev :: Int
              _dfltOmbInStackTraceCtxt :: (Maybe HsName)
              _dfltOopts :: EHCOpts
              _dfltOuniq :: Int
              _dfltOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              _altsIcTrf :: CAltL 
              _altsIdebugLamMp :: LamMp
              _altsIuniq :: Int
              _dfltIcTrf :: CExpr 
              _dfltIdebugLamMp :: LamMp
              _dfltIgathLamMp :: LamMp
              _dfltIisTraceCandidate :: Bool
              _dfltItraceCandidateLamArgs :: ([HsName])
              _dfltItraceCandidateLamBody :: CExpr 
              _dfltIuniq :: Int
              _dfltIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp `Map.union` _altsIdebugLamMp `Map.union` _dfltIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _dfltIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _exprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _altsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _altsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _altsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _altsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _altsOlev =
                  _lhsIlev
              -- copy rule (down)
              _altsOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _altsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _altsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _dfltOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _dfltOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _dfltOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _dfltOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _dfltOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _dfltOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _dfltOlev =
                  _lhsIlev
              -- copy rule (down)
              _dfltOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _dfltOopts =
                  _lhsIopts
              -- copy rule (chain)
              _dfltOuniq =
                  _altsIuniq
              -- copy rule (from local)
              _dfltOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
              ( _altsIcTrf,_altsIdebugLamMp,_altsIuniq) =
                  alts_ _altsOevalCtx _altsOisLamBody _altsOisStrict _altsOlamMp _altsOlev _altsOmbInStackTraceCtxt _altsOopts _altsOuniq 
              ( _dfltIcTrf,_dfltIdebugLamMp,_dfltIgathLamMp,_dfltIisTraceCandidate,_dfltItraceCandidateLamArgs,_dfltItraceCandidateLamBody,_dfltIuniq,_dfltIwhatBelow) =
                  dflt_ _dfltOevalCtx _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlamMp _dfltOlev _dfltOmbInStackTraceCtxt _dfltOopts _dfltOuniq _dfltOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOgathLamMp :: LamMp
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _errorExprOevalCtx :: EvalCtx
              _errorExprOisLamBody :: Bool
              _errorExprOisStrict :: Bool
              _errorExprOisTopApp :: Bool
              _errorExprOisTopTup :: Bool
              _errorExprOlamMp :: LamMp
              _errorExprOlev :: Int
              _errorExprOmbInStackTraceCtxt :: (Maybe HsName)
              _errorExprOopts :: EHCOpts
              _errorExprOuniq :: Int
              _errorExprOwhatAbove :: WhatExpr
              _errorExprIcTrf :: CExpr 
              _errorExprIdebugLamMp :: LamMp
              _errorExprIgathLamMp :: LamMp
              _errorExprIisTraceCandidate :: Bool
              _errorExprItraceCandidateLamArgs :: ([HsName])
              _errorExprItraceCandidateLamBody :: CExpr 
              _errorExprIuniq :: Int
              _errorExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _errorExprIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_CaseAltFail failReason_ _errorExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgathLamMp =
                  _errorExprIgathLamMp
              -- copy rule (up)
              _lhsOuniq =
                  _errorExprIuniq
              -- copy rule (up)
              _lhsOwhatBelow =
                  _errorExprIwhatBelow
              -- copy rule (down)
              _errorExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _errorExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _errorExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _errorExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _errorExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _errorExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _errorExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _errorExprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _errorExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _errorExprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _errorExprOwhatAbove =
                  _whatAbove
              ( _errorExprIcTrf,_errorExprIdebugLamMp,_errorExprIgathLamMp,_errorExprIisTraceCandidate,_errorExprItraceCandidateLamArgs,_errorExprItraceCandidateLamBody,_errorExprIuniq,_errorExprIwhatBelow) =
                  errorExpr_ _errorExprOevalCtx _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlamMp _errorExprOlev _errorExprOmbInStackTraceCtxt _errorExprOopts _errorExprOuniq _errorExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_CoeArg
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_FFI callconv_ safety_ impEnt_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Hole uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bodyOevalCtx :: EvalCtx
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOmbInStackTraceCtxt :: (Maybe HsName)
              _bodyOopts :: EHCOpts
              _bodyOuniq :: Int
              _bodyOwhatAbove :: WhatExpr
              _bodyIcTrf :: CExpr 
              _bodyIdebugLamMp :: LamMp
              _bodyIgathLamMp :: LamMp
              _bodyIisTraceCandidate :: Bool
              _bodyItraceCandidateLamArgs :: ([HsName])
              _bodyItraceCandidateLamBody :: CExpr 
              _bodyIuniq :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _bodyIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_HoleLet bindsUid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bodyIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIcTrf,_bodyIdebugLamMp,_bodyIgathLamMp,_bodyIisTraceCandidate,_bodyItraceCandidateLamArgs,_bodyItraceCandidateLamBody,_bodyIuniq,_bodyIwhatBelow) =
                  body_ _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmbInStackTraceCtxt _bodyOopts _bodyOuniq _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _funcOevalCtx :: EvalCtx
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopApp :: Bool
              _funcOisTopTup :: Bool
              _funcOlamMp :: LamMp
              _funcOlev :: Int
              _funcOmbInStackTraceCtxt :: (Maybe HsName)
              _funcOopts :: EHCOpts
              _funcOuniq :: Int
              _funcOwhatAbove :: WhatExpr
              _funcIcTrf :: CExpr 
              _funcIdebugLamMp :: LamMp
              _funcIgathLamMp :: LamMp
              _funcIisTraceCandidate :: Bool
              _funcItraceCandidateLamArgs :: ([HsName])
              _funcItraceCandidateLamBody :: CExpr 
              _funcIuniq :: Int
              _funcIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _funcIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_ImplsApp _funcIcTrf uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _funcIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _funcOopts =
                  _lhsIopts
              -- copy rule (down)
              _funcOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              ( _funcIcTrf,_funcIdebugLamMp,_funcIgathLamMp,_funcIisTraceCandidate,_funcItraceCandidateLamArgs,_funcItraceCandidateLamBody,_funcIuniq,_funcIwhatBelow) =
                  func_ _funcOevalCtx _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamMp _funcOlev _funcOmbInStackTraceCtxt _funcOopts _funcOuniq _funcOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bodyOevalCtx :: EvalCtx
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOmbInStackTraceCtxt :: (Maybe HsName)
              _bodyOopts :: EHCOpts
              _bodyOuniq :: Int
              _bodyOwhatAbove :: WhatExpr
              _bodyIcTrf :: CExpr 
              _bodyIdebugLamMp :: LamMp
              _bodyIgathLamMp :: LamMp
              _bodyIisTraceCandidate :: Bool
              _bodyItraceCandidateLamArgs :: ([HsName])
              _bodyItraceCandidateLamBody :: CExpr 
              _bodyIuniq :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _bodyIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_ImplsLam uid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bodyIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIcTrf,_bodyIdebugLamMp,_bodyIgathLamMp,_bodyIisTraceCandidate,_bodyItraceCandidateLamArgs,_bodyItraceCandidateLamBody,_bodyIuniq,_bodyIwhatBelow) =
                  body_ _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmbInStackTraceCtxt _bodyOopts _bodyOuniq _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 55, column 17)
              _whatBelow =
                  ExprIsInt int_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Integer integer_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bodyOlamMp :: LamMp
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bindOevalCtx :: EvalCtx
              _bindOisGlobal :: Bool
              _bindOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOlamMp :: LamMp
              _bindOletBindingsCateg :: CBindCateg
              _bindOlev :: Int
              _bindOmbInStackTraceCtxt :: (Maybe HsName)
              _bindOopts :: EHCOpts
              _bindOuniq :: Int
              _bodyOevalCtx :: EvalCtx
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlev :: Int
              _bodyOmbInStackTraceCtxt :: (Maybe HsName)
              _bodyOopts :: EHCOpts
              _bodyOuniq :: Int
              _bodyOwhatAbove :: WhatExpr
              _bindIbindCTrfL :: ([CBind])
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindIdebugLamMp :: LamMp
              _bindInm :: HsName
              _bindIuniq :: Int
              _bodyIcTrf :: CExpr 
              _bodyIdebugLamMp :: LamMp
              _bodyIgathLamMp :: LamMp
              _bodyIisTraceCandidate :: Bool
              _bodyItraceCandidateLamArgs :: ([HsName])
              _bodyItraceCandidateLamBody :: CExpr 
              _bodyIuniq :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 63, column 17)
              _lhsOisTraceCandidate =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 63, column 17)
              _lhsOtraceCandidateLamArgs =
                  _argNm : _bodyItraceCandidateLamArgs
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 63, column 17)
              _lhsOtraceCandidateLamBody =
                  _bodyItraceCandidateLamBody
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 7, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 53, column 17)
              _whatBelow =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 66, column 17)
              _whatAbove =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 19, column 25)
              _argNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonLamInfo.ag"(line 7, column 17)
              _bodyOlamMp =
                  Map.delete _argNm _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _bindIdebugLamMp `Map.union` _bodyIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_Lam _bindIcTrf _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bodyIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bindOevalCtx =
                  _lhsIevalCtx
              -- copy rule (from local)
              _bindOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _bindOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _bindOlev =
                  _lev
              -- copy rule (down)
              _bindOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (from local)
              _bodyOlev =
                  _lev
              -- copy rule (down)
              _bodyOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bodyOuniq =
                  _bindIuniq
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindIbindCTrfL,_bindIbindLamMp,_bindIcTrf,_bindIdebugLamMp,_bindInm,_bindIuniq) =
                  bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOmbInStackTraceCtxt _bindOopts _bindOuniq 
              ( _bodyIcTrf,_bodyIdebugLamMp,_bodyIgathLamMp,_bodyIisTraceCandidate,_bodyItraceCandidateLamArgs,_bodyItraceCandidateLamBody,_bodyIuniq,_bodyIwhatBelow) =
                  body_ _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmbInStackTraceCtxt _bodyOopts _bodyOuniq _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _lhsOcTrf :: CExpr 
              _bindsOlev :: Int
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bindsOisStrict :: Bool
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bindsOevalCtx :: EvalCtx
              _bindsOisGlobal :: Bool
              _bindsOisLamBody :: Bool
              _bindsOlamMp :: LamMp
              _bindsOletBindingsCateg :: CBindCateg
              _bindsOmbInStackTraceCtxt :: (Maybe HsName)
              _bindsOopts :: EHCOpts
              _bindsOuniq :: Int
              _bodyOevalCtx :: EvalCtx
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamMp :: LamMp
              _bodyOlev :: Int
              _bodyOmbInStackTraceCtxt :: (Maybe HsName)
              _bodyOopts :: EHCOpts
              _bodyOuniq :: Int
              _bodyOwhatAbove :: WhatExpr
              _bindsIbindCTrfL :: ([CBind])
              _bindsIbindLamMp :: LamMp
              _bindsIcTrf :: CBindL 
              _bindsIdebugLamMp :: LamMp
              _bindsIuniq :: Int
              _bodyIcTrf :: CExpr 
              _bodyIdebugLamMp :: LamMp
              _bodyIgathLamMp :: LamMp
              _bodyIisTraceCandidate :: Bool
              _bodyItraceCandidateLamArgs :: ([HsName])
              _bodyItraceCandidateLamBody :: CExpr 
              _bodyIuniq :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 134, column 17)
              _lhsOcTrf =
                  CExpr_Let categ_ _bindsIbindCTrfL _bodyIcTrf
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 15, column 17)
              _isGlobal =
                  _lhsIlev == cLevModule
              -- "build/101/lib-ehc/EH101/Core/CommonLevLet.ag"(line 2, column 17)
              _bindsOlev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 4, column 17)
              _letBindingsCateg =
                  categ_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 72, column 17)
              _isTopLet =
                  _lhsIwhatAbove == ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 111, column 17)
              _bindsOisStrict =
                  _isGlobal || categ_ == CBindCateg_Strict
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 124, column 17)
              _evalCtx =
                  if categ_ == CBindCateg_Strict
                  then EvalCtx_Eval
                  else EvalCtx_None
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 7, column 17)
              _lhsOgathLamMp =
                  _bindsIbindLamMp `Map.union` _bodyIgathLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _bindsIdebugLamMp `Map.union` _bodyIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bodyIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _bindsOevalCtx =
                  _evalCtx
              -- copy rule (from local)
              _bindsOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _bindsOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _bindsOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _bodyOevalCtx =
                  _evalCtx
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bodyOuniq =
                  _bindsIuniq
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindsIbindCTrfL,_bindsIbindLamMp,_bindsIcTrf,_bindsIdebugLamMp,_bindsIuniq) =
                  binds_ _bindsOevalCtx _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOlamMp _bindsOletBindingsCateg _bindsOlev _bindsOmbInStackTraceCtxt _bindsOopts _bindsOuniq 
              ( _bodyIcTrf,_bodyIdebugLamMp,_bodyIgathLamMp,_bodyIisTraceCandidate,_bodyItraceCandidateLamArgs,_bodyItraceCandidateLamBody,_bodyIuniq,_bodyIwhatBelow) =
                  body_ _bodyOevalCtx _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamMp _bodyOlev _bodyOmbInStackTraceCtxt _bodyOopts _bodyOuniq _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_String str_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Tup tag_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _offsetOevalCtx :: EvalCtx
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOmbInStackTraceCtxt :: (Maybe HsName)
              _offsetOopts :: EHCOpts
              _offsetOuniq :: Int
              _offsetOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIdebugLamMp :: LamMp
              _offsetIgathLamMp :: LamMp
              _offsetIisTraceCandidate :: Bool
              _offsetItraceCandidateLamArgs :: ([HsName])
              _offsetItraceCandidateLamBody :: CExpr 
              _offsetIuniq :: Int
              _offsetIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp `Map.union` _offsetIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _offsetIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOuniq =
                  _exprIuniq
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIdebugLamMp,_offsetIgathLamMp,_offsetIisTraceCandidate,_offsetItraceCandidateLamArgs,_offsetItraceCandidateLamBody,_offsetIuniq,_offsetIwhatBelow) =
                  offset_ _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmbInStackTraceCtxt _offsetOopts _offsetOuniq _offsetOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _offsetOevalCtx :: EvalCtx
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOmbInStackTraceCtxt :: (Maybe HsName)
              _offsetOopts :: EHCOpts
              _offsetOuniq :: Int
              _offsetOwhatAbove :: WhatExpr
              _fldExprOevalCtx :: EvalCtx
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlamMp :: LamMp
              _fldExprOlev :: Int
              _fldExprOmbInStackTraceCtxt :: (Maybe HsName)
              _fldExprOopts :: EHCOpts
              _fldExprOuniq :: Int
              _fldExprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIdebugLamMp :: LamMp
              _offsetIgathLamMp :: LamMp
              _offsetIisTraceCandidate :: Bool
              _offsetItraceCandidateLamArgs :: ([HsName])
              _offsetItraceCandidateLamBody :: CExpr 
              _offsetIuniq :: Int
              _offsetIwhatBelow :: WhatExpr
              _fldExprIcTrf :: CExpr 
              _fldExprIdebugLamMp :: LamMp
              _fldExprIgathLamMp :: LamMp
              _fldExprIisTraceCandidate :: Bool
              _fldExprItraceCandidateLamArgs :: ([HsName])
              _fldExprItraceCandidateLamBody :: CExpr 
              _fldExprIuniq :: Int
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp `Map.union` _offsetIdebugLamMp `Map.union` _fldExprIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _fldExprIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOuniq =
                  _exprIuniq
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldExprOuniq =
                  _offsetIuniq
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIdebugLamMp,_offsetIgathLamMp,_offsetIisTraceCandidate,_offsetItraceCandidateLamArgs,_offsetItraceCandidateLamBody,_offsetIuniq,_offsetIwhatBelow) =
                  offset_ _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmbInStackTraceCtxt _offsetOopts _offsetOuniq _offsetOwhatAbove 
              ( _fldExprIcTrf,_fldExprIdebugLamMp,_fldExprIgathLamMp,_fldExprIisTraceCandidate,_fldExprItraceCandidateLamArgs,_fldExprItraceCandidateLamBody,_fldExprIuniq,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamMp _fldExprOlev _fldExprOmbInStackTraceCtxt _fldExprOopts _fldExprOuniq _fldExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExpr 
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOevalCtx :: EvalCtx
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _offsetOevalCtx :: EvalCtx
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOmbInStackTraceCtxt :: (Maybe HsName)
              _offsetOopts :: EHCOpts
              _offsetOuniq :: Int
              _offsetOwhatAbove :: WhatExpr
              _fldExprOevalCtx :: EvalCtx
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlamMp :: LamMp
              _fldExprOlev :: Int
              _fldExprOmbInStackTraceCtxt :: (Maybe HsName)
              _fldExprOopts :: EHCOpts
              _fldExprOuniq :: Int
              _fldExprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIdebugLamMp :: LamMp
              _offsetIgathLamMp :: LamMp
              _offsetIisTraceCandidate :: Bool
              _offsetItraceCandidateLamArgs :: ([HsName])
              _offsetItraceCandidateLamBody :: CExpr 
              _offsetIuniq :: Int
              _offsetIwhatBelow :: WhatExpr
              _fldExprIcTrf :: CExpr 
              _fldExprIdebugLamMp :: LamMp
              _fldExprIgathLamMp :: LamMp
              _fldExprIisTraceCandidate :: Bool
              _fldExprItraceCandidateLamArgs :: ([HsName])
              _fldExprItraceCandidateLamBody :: CExpr 
              _fldExprIuniq :: Int
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp `Map.union` _offsetIdebugLamMp `Map.union` _fldExprIdebugLamMp
              -- self rule
              _cTrf =
                  CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _fldExprIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (chain)
              _offsetOuniq =
                  _exprIuniq
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _fldExprOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldExprOuniq =
                  _offsetIuniq
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIdebugLamMp,_offsetIgathLamMp,_offsetIisTraceCandidate,_offsetItraceCandidateLamArgs,_offsetItraceCandidateLamBody,_offsetIuniq,_offsetIwhatBelow) =
                  offset_ _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmbInStackTraceCtxt _offsetOopts _offsetOuniq _offsetOwhatAbove 
              ( _fldExprIcTrf,_fldExprIdebugLamMp,_fldExprIgathLamMp,_fldExprIisTraceCandidate,_fldExprItraceCandidateLamArgs,_fldExprItraceCandidateLamBody,_fldExprIuniq,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOevalCtx _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamMp _fldExprOlev _fldExprOmbInStackTraceCtxt _fldExprOopts _fldExprOuniq _fldExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq
       _lhsIwhatAbove ->
         (let _lhsOisTraceCandidate :: Bool
              _lhsOtraceCandidateLamArgs :: ([HsName])
              _lhsOtraceCandidateLamBody :: CExpr 
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOgathLamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOuniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOisTraceCandidate =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamArgs =
                  []
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 69, column 17)
              _lhsOtraceCandidateLamBody =
                  _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 135, column 17)
              _lhsOcTrf =
                  case Map.lookup _nm _lhsIlamMp of
                    Just (LamInfo {laminfoStackTrace=StackTraceInfo_HasStackTraceEquiv dbNm})
                      -> case _lhsImbInStackTraceCtxt of
                           Just stTrNm
                             -> acoreApp1 (acoreVar dbNm)
                                          (acoreApp (acoreVar hsnStackTracePush) [acoreBuiltinString _lhsIopts (show _nm), acoreVar stTrNm])
                           _ -> _cTrf
                    _ -> _cTrf
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 54, column 17)
              _whatBelow =
                  ExprIsVar _nm
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nm =
                  acbrefNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nmAsp =
                  mkHNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 9, column 33)
              _lhsOgathLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Var ref_
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOisTraceCandidate,_lhsOtraceCandidateLamArgs,_lhsOtraceCandidateLamBody,_lhsOuniq,_lhsOwhatBelow)))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                   Int ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   Int ->
                   ( CExprAnn ,LamMp,Int)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExprAnn 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExprAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExprAnn 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExprAnn_Debug info_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CExprAnn 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExprAnn_Ty ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                    Int ->
                    (Maybe HsName) ->
                    EHCOpts ->
                    Int ->
                    ( CMetaBind ,LamMp,Int)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaBind 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaBind_Apply0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaBind 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaBind_Function0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaBind 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaBind_Function1
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaBind 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaBind_Plain
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                   Int ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   Int ->
                   ( CMetaVal ,LamMp,Int)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaVal 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaVal_Dict
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaVal 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaVal_DictClass tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaVal 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaVal 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaVal_Track track_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetaVal 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CMetaVal_Val
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                 Int ->
                 (Maybe HsName) ->
                 EHCOpts ->
                 Int ->
                 ( CMetas ,LamMp,Int)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CMetas 
              _lhsOuniq :: Int
              _x1OlamMp :: LamMp
              _x1Olev :: Int
              _x1OmbInStackTraceCtxt :: (Maybe HsName)
              _x1Oopts :: EHCOpts
              _x1Ouniq :: Int
              _x2OlamMp :: LamMp
              _x2Olev :: Int
              _x2OmbInStackTraceCtxt :: (Maybe HsName)
              _x2Oopts :: EHCOpts
              _x2Ouniq :: Int
              _x1IcTrf :: CMetaBind 
              _x1IdebugLamMp :: LamMp
              _x1Iuniq :: Int
              _x2IcTrf :: CMetaVal 
              _x2IdebugLamMp :: LamMp
              _x2Iuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _x1IdebugLamMp `Map.union` _x2IdebugLamMp
              -- self rule
              _cTrf =
                  (_x1IcTrf,_x2IcTrf)
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _x2Iuniq
              -- copy rule (down)
              _x1OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x1Olev =
                  _lhsIlev
              -- copy rule (down)
              _x1OmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _x1Oopts =
                  _lhsIopts
              -- copy rule (down)
              _x1Ouniq =
                  _lhsIuniq
              -- copy rule (down)
              _x2OlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _x2Olev =
                  _lhsIlev
              -- copy rule (down)
              _x2OmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _x2Oopts =
                  _lhsIopts
              -- copy rule (chain)
              _x2Ouniq =
                  _x1Iuniq
              ( _x1IcTrf,_x1IdebugLamMp,_x1Iuniq) =
                  x1_ _x1OlamMp _x1Olev _x1OmbInStackTraceCtxt _x1Oopts _x1Ouniq 
              ( _x2IcTrf,_x2IdebugLamMp,_x2Iuniq) =
                  x2_ _x2OlamMp _x2Olev _x2OmbInStackTraceCtxt _x2Oopts _x2Ouniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
         gathLamMp            : LamMp
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local mbInStackTraceCtxt : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = LamMp ->
                  Int ->
                  EHCOpts ->
                  Int ->
                  ( CModule ,LamMp,LamMp,Int)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsIopts
       _lhsIuniq ->
         (let _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _exprOisLamBody :: Bool
              _exprOevalCtx :: EvalCtx
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CModule 
              _lhsOgathLamMp :: LamMp
              _lhsOuniq :: Int
              _exprOlamMp :: LamMp
              _exprOlev :: Int
              _exprOmbInStackTraceCtxt :: (Maybe HsName)
              _exprOopts :: EHCOpts
              _exprOuniq :: Int
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIdebugLamMp :: LamMp
              _exprIgathLamMp :: LamMp
              _exprIisTraceCandidate :: Bool
              _exprItraceCandidateLamArgs :: ([HsName])
              _exprItraceCandidateLamBody :: CExpr 
              _exprIuniq :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 83, column 17)
              _mbInStackTraceCtxt =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 85, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 116, column 17)
              _exprOevalCtx =
                  EvalCtx_Eval
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _exprIdebugLamMp
              -- self rule
              _cTrf =
                  CModule_Mod moduleNm_ _exprIcTrf ctagsMp_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgathLamMp =
                  _exprIgathLamMp
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (from local)
              _exprOmbInStackTraceCtxt =
                  _mbInStackTraceCtxt
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIdebugLamMp,_exprIgathLamMp,_exprIisTraceCandidate,_exprItraceCandidateLamArgs,_exprItraceCandidateLamBody,_exprIuniq,_exprIwhatBelow) =
                  expr_ _exprOevalCtx _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamMp _exprOlev _exprOmbInStackTraceCtxt _exprOopts _exprOuniq _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp,_lhsOuniq)))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
               Int ->
               (Maybe HsName) ->
               EHCOpts ->
               Int ->
               ( CPat ,LamMp,([HsName]),Int)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_BoolExpr cexpr_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOuniq :: Int
              _restOlamMp :: LamMp
              _restOlev :: Int
              _restOmbInStackTraceCtxt :: (Maybe HsName)
              _restOopts :: EHCOpts
              _restOuniq :: Int
              _bindsOlamMp :: LamMp
              _bindsOlev :: Int
              _bindsOmbInStackTraceCtxt :: (Maybe HsName)
              _bindsOopts :: EHCOpts
              _bindsOuniq :: Int
              _restIcTrf :: CPatRest 
              _restIdebugLamMp :: LamMp
              _restIuniq :: Int
              _bindsIcTrf :: CPatFldL 
              _bindsIdebugLamMp :: LamMp
              _bindsIfldNmL :: ([HsName])
              _bindsIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _restIdebugLamMp `Map.union` _bindsIdebugLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _bindsIfldNmL
              -- self rule
              _cTrf =
                  CPat_Con tag_ _restIcTrf _bindsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _bindsIuniq
              -- copy rule (down)
              _restOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _restOlev =
                  _lhsIlev
              -- copy rule (down)
              _restOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _restOopts =
                  _lhsIopts
              -- copy rule (down)
              _restOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _bindsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bindsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bindsOuniq =
                  _restIuniq
              ( _restIcTrf,_restIdebugLamMp,_restIuniq) =
                  rest_ _restOlamMp _restOlev _restOmbInStackTraceCtxt _restOopts _restOuniq 
              ( _bindsIcTrf,_bindsIdebugLamMp,_bindsIfldNmL,_bindsIuniq) =
                  binds_ _bindsOlamMp _bindsOlev _bindsOmbInStackTraceCtxt _bindsOopts _bindsOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Var pnm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
         fldNmL               : [HsName]
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local whatAbove   : {WhatExpr}
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
                  Int ->
                  (Maybe HsName) ->
                  EHCOpts ->
                  Int ->
                  ( CPatFld ,LamMp,([HsName]),Int)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _bindOisGlobal :: Bool
              _bindOletBindingsCateg :: CBindCateg
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _offsetOisStrict :: Bool
              _offsetOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOisLamBody :: Bool
              _offsetOevalCtx :: EvalCtx
              _bindOevalCtx :: EvalCtx
              _lhsOfldNmL :: ([HsName])
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CPatFld 
              _lhsOuniq :: Int
              _offsetOlamMp :: LamMp
              _offsetOlev :: Int
              _offsetOmbInStackTraceCtxt :: (Maybe HsName)
              _offsetOopts :: EHCOpts
              _offsetOuniq :: Int
              _offsetOwhatAbove :: WhatExpr
              _bindOlamMp :: LamMp
              _bindOlev :: Int
              _bindOmbInStackTraceCtxt :: (Maybe HsName)
              _bindOopts :: EHCOpts
              _bindOuniq :: Int
              _fldAnnsOlamMp :: LamMp
              _fldAnnsOlev :: Int
              _fldAnnsOmbInStackTraceCtxt :: (Maybe HsName)
              _fldAnnsOopts :: EHCOpts
              _fldAnnsOuniq :: Int
              _offsetIcTrf :: CExpr 
              _offsetIdebugLamMp :: LamMp
              _offsetIgathLamMp :: LamMp
              _offsetIisTraceCandidate :: Bool
              _offsetItraceCandidateLamArgs :: ([HsName])
              _offsetItraceCandidateLamBody :: CExpr 
              _offsetIuniq :: Int
              _offsetIwhatBelow :: WhatExpr
              _bindIbindCTrfL :: ([CBind])
              _bindIbindLamMp :: LamMp
              _bindIcTrf :: CBind 
              _bindIdebugLamMp :: LamMp
              _bindInm :: HsName
              _bindIuniq :: Int
              _fldAnnsIcTrf :: CBindAnnL 
              _fldAnnsIdebugLamMp :: LamMp
              _fldAnnsIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 19, column 17)
              _bindOisGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 8, column 17)
              _bindOletBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 79, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisStrict =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 119, column 17)
              _offsetOevalCtx =
                  EvalCtx_Eval
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 120, column 33)
              _bindOevalCtx =
                  EvalCtx_None
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 23, column 17)
              _fldNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 28, column 17)
              _lhsOfldNmL =
                  [_fldNm]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _offsetIdebugLamMp `Map.union` _bindIdebugLamMp `Map.union` _fldAnnsIdebugLamMp
              -- self rule
              _cTrf =
                  CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _fldAnnsIuniq
              -- copy rule (down)
              _offsetOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _offsetOopts =
                  _lhsIopts
              -- copy rule (down)
              _offsetOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bindOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _bindOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bindOuniq =
                  _offsetIuniq
              -- copy rule (down)
              _fldAnnsOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _fldAnnsOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldAnnsOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _fldAnnsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _fldAnnsOuniq =
                  _bindIuniq
              ( _offsetIcTrf,_offsetIdebugLamMp,_offsetIgathLamMp,_offsetIisTraceCandidate,_offsetItraceCandidateLamArgs,_offsetItraceCandidateLamBody,_offsetIuniq,_offsetIwhatBelow) =
                  offset_ _offsetOevalCtx _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamMp _offsetOlev _offsetOmbInStackTraceCtxt _offsetOopts _offsetOuniq _offsetOwhatAbove 
              ( _bindIbindCTrfL,_bindIbindLamMp,_bindIcTrf,_bindIdebugLamMp,_bindInm,_bindIuniq) =
                  bind_ _bindOevalCtx _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamMp _bindOletBindingsCateg _bindOlev _bindOmbInStackTraceCtxt _bindOopts _bindOuniq 
              ( _fldAnnsIcTrf,_fldAnnsIdebugLamMp,_fldAnnsIuniq) =
                  fldAnns_ _fldAnnsOlamMp _fldAnnsOlev _fldAnnsOmbInStackTraceCtxt _fldAnnsOopts _fldAnnsOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                   Int ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   Int ->
                   ( CPatFldL ,LamMp,([HsName]),Int)
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOuniq :: Int
              _hdOlamMp :: LamMp
              _hdOlev :: Int
              _hdOmbInStackTraceCtxt :: (Maybe HsName)
              _hdOopts :: EHCOpts
              _hdOuniq :: Int
              _tlOlamMp :: LamMp
              _tlOlev :: Int
              _tlOmbInStackTraceCtxt :: (Maybe HsName)
              _tlOopts :: EHCOpts
              _tlOuniq :: Int
              _hdIcTrf :: CPatFld 
              _hdIdebugLamMp :: LamMp
              _hdIfldNmL :: ([HsName])
              _hdIuniq :: Int
              _tlIcTrf :: CPatFldL 
              _tlIdebugLamMp :: LamMp
              _tlIfldNmL :: ([HsName])
              _tlIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _hdIdebugLamMp `Map.union` _tlIdebugLamMp
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _hdIfldNmL ++ _tlIfldNmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIcTrf,_hdIdebugLamMp,_hdIfldNmL,_hdIuniq) =
                  hd_ _hdOlamMp _hdOlev _hdOmbInStackTraceCtxt _hdOopts _hdOuniq 
              ( _tlIcTrf,_tlIdebugLamMp,_tlIfldNmL,_tlIuniq) =
                  tl_ _tlOlamMp _tlOlev _tlOmbInStackTraceCtxt _tlOopts _tlOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOfldNmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOfldNmL,_lhsOuniq)))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                   Int ->
                   (Maybe HsName) ->
                   EHCOpts ->
                   Int ->
                   ( CPatRest ,LamMp,Int)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CPatRest 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CPatRest_Empty
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CPatRest 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CPatRest_Var nm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         cTrf                 : CModule 
         debugLamMp           : LamMp
         gathLamMp            : LamMp
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local howUnionGathLamInfo : _
            local howMergeLamInfo : _
            local uniq        : _
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
                    ( CModule ,LamMp,LamMp)
data Inh_CodeAGItf  = Inh_CodeAGItf {lamMp_Inh_CodeAGItf :: !(LamMp),opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule ),debugLamMp_Syn_CodeAGItf :: !(LamMp),gathLamMp_Syn_CodeAGItf :: !(LamMp)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIlamMp _lhsIopts )  =
    (let ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp) = sem _lhsIlamMp _lhsIopts 
     in  (Syn_CodeAGItf _lhsOcTrf _lhsOdebugLamMp _lhsOgathLamMp ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIlamMp
       _lhsIopts ->
         (let _moduleOlev :: Int
              _moduleOlamMp :: LamMp
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: CModule 
              _lhsOgathLamMp :: LamMp
              _moduleOopts :: EHCOpts
              _moduleOuniq :: Int
              _moduleIcTrf :: CModule 
              _moduleIdebugLamMp :: LamMp
              _moduleIgathLamMp :: LamMp
              _moduleIuniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 51, column 17)
              _howUnionGathLamInfo =
                  Map.union _gathLamMp
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 55, column 17)
              _howMergeLamInfo =
                  (\(LamInfo {laminfoStackTrace=t}) i -> i {laminfoStackTrace=t})
              -- "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 102, column 17)
              _uniq =
                  0
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 4, column 17)
              _moduleOlev =
                  cLevModule
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 15, column 17)
              _gathLamMp =
                  lamMpMergeInto _howMergeLamInfo const _moduleIgathLamMp _lhsIlamMp
              -- "build/101/lib-ehc/EH101/Core/CommonGathLamInfo.ag"(line 16, column 17)
              _moduleOlamMp =
                  _howUnionGathLamInfo _lhsIlamMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _moduleIdebugLamMp
              -- copy rule (up)
              _lhsOcTrf =
                  _moduleIcTrf
              -- copy rule (from local)
              _lhsOgathLamMp =
                  _gathLamMp
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              -- copy rule (from local)
              _moduleOuniq =
                  _uniq
              ( _moduleIcTrf,_moduleIdebugLamMp,_moduleIgathLamMp,_moduleIuniq) =
                  module_ _moduleOlamMp _moduleOlev _moduleOopts _moduleOuniq 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOgathLamMp)))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lamMp                : LamMp
         lev                  : Int
         mbInStackTraceCtxt   : Maybe HsName
         opts                 : EHCOpts
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         cTrf                 : SELF 
         debugLamMp           : LamMp
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
                  LamMp ->
                  Int ->
                  (Maybe HsName) ->
                  EHCOpts ->
                  Int ->
                  ( MbCExpr ,LamMp,Int)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _justOisTopApp :: Bool
              _justOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: MbCExpr 
              _lhsOuniq :: Int
              _justOevalCtx :: EvalCtx
              _justOisLamBody :: Bool
              _justOisStrict :: Bool
              _justOlamMp :: LamMp
              _justOlev :: Int
              _justOmbInStackTraceCtxt :: (Maybe HsName)
              _justOopts :: EHCOpts
              _justOuniq :: Int
              _justOwhatAbove :: WhatExpr
              _justIcTrf :: CExpr 
              _justIdebugLamMp :: LamMp
              _justIgathLamMp :: LamMp
              _justIisTraceCandidate :: Bool
              _justItraceCandidateLamArgs :: ([HsName])
              _justItraceCandidateLamBody :: CExpr 
              _justIuniq :: Int
              _justIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 88, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  _justIdebugLamMp
              -- self rule
              _cTrf =
                  Just _justIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOuniq =
                  _justIuniq
              -- copy rule (down)
              _justOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _justOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _justOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _justOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _justOlev =
                  _lhsIlev
              -- copy rule (down)
              _justOmbInStackTraceCtxt =
                  _lhsImbInStackTraceCtxt
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _justOwhatAbove =
                  _whatAbove
              ( _justIcTrf,_justIdebugLamMp,_justIgathLamMp,_justIisTraceCandidate,_justItraceCandidateLamArgs,_justItraceCandidateLamBody,_justIuniq,_justIwhatBelow) =
                  just_ _justOevalCtx _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlamMp _justOlev _justOmbInStackTraceCtxt _justOopts _justOuniq _justOwhatAbove 
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIevalCtx
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamMp
       _lhsIlev
       _lhsImbInStackTraceCtxt
       _lhsIopts
       _lhsIuniq ->
         (let _lhsOdebugLamMp :: LamMp
              _lhsOcTrf :: MbCExpr 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ExplicitStackTrace.ag"(line 147, column 22)
              _lhsOdebugLamMp =
                  Map.empty
              -- self rule
              _cTrf =
                  Nothing
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOcTrf,_lhsOdebugLamMp,_lhsOuniq)))
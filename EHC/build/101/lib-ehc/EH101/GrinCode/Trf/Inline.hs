

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag)
module EH101.GrinCode.Trf.Inline(grInline) where

import qualified EH.Util.FastSeq as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.GrinCode.Common
import EH101.GrinCode
import EH.Util.Pretty
import EH.Util.Utils
import EH101.GrinCode.Trf.AliasRename
import qualified EH101.Config as Cfg
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Debug
import EH101.GrinCode.FreeVars





















grInline :: Bool -> HsNameS -> GrInlMp -> GrModule -> (GrModule,GrInlMp)
grInline allow expNmS inlMp grmod
  = (trf_Syn_GrAGItf t,gathInlMp_Syn_GrAGItf t)
  where t = wrap_GrAGItf (sem_GrAGItf $ GrAGItf_AGItf grmod)
            $ Inh_GrAGItf
                { expNmS_Inh_GrAGItf = expNmS
                , inlMp_Inh_GrAGItf = inlMp
                , allowOmitBind_Inh_GrAGItf = allow
                }



inlMayExport :: HsNameS -> HsNameS -> HsName -> GrInl -> Bool
inlMayExport onlyInThisModule expNmS n inl
  = case inl of
      GrInl_Call _ e
        -> n `Set.member` expNmS
           && Set.null (onlyInThisModule `Set.intersection` Map.keysSet (grFreeVars e))
      _ -> False



inlGrVar :: GrInlMp -> [HsName] -> (GrExpr -> GrExpr,FvS)
inlGrVar inlMp nmL
  = (foldr (.) id mks,Set.unions inls)
  where inl nm = case Map.lookup nm inlMp of
                   Just (GrInl_CAF e) -> (GrExpr_Seq e (GrPatLam_Var nm),Set.singleton nm)
                   _ -> (id,Set.empty)
        (mks,inls) = unzip $ map inl nmL

inlNmsAreInlineable :: [Maybe HsName] -> Bool
inlNmsAreInlineable = and . map isJust

inlRename :: Int -> [Maybe HsName] -> [HsName] -> GrExpr -> GrExpr
inlRename uniq asFrom as e
  = grAliasRename (Just $ (hsnUniqifyInt HsNameUniqifier_Inline uniq)) (mkNmAliasMp $ zip as (map fromJust asFrom)) e

-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowOmitBind        : Bool
         expNmS               : HsNameS
         inlMp                : GrInlMp
      synthesized attributes:
         gathInlMp            : GrInlMp
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
type T_GrAGItf  = Bool ->
                  HsNameS ->
                  GrInlMp ->
                  ( GrInlMp,GrModule )
data Inh_GrAGItf  = Inh_GrAGItf {allowOmitBind_Inh_GrAGItf :: !(Bool),expNmS_Inh_GrAGItf :: !(HsNameS),inlMp_Inh_GrAGItf :: !(GrInlMp)}
data Syn_GrAGItf  = Syn_GrAGItf {gathInlMp_Syn_GrAGItf :: !(GrInlMp),trf_Syn_GrAGItf :: !(GrModule )}
wrap_GrAGItf :: T_GrAGItf  ->
                Inh_GrAGItf  ->
                Syn_GrAGItf 
wrap_GrAGItf sem (Inh_GrAGItf _lhsIallowOmitBind _lhsIexpNmS _lhsIinlMp )  =
    (let ( _lhsOgathInlMp,_lhsOtrf) = sem _lhsIallowOmitBind _lhsIexpNmS _lhsIinlMp 
     in  (Syn_GrAGItf _lhsOgathInlMp _lhsOtrf ))
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIinlMp ->
         (let _lhsOgathInlMp :: GrInlMp
              _lhsOtrf :: GrModule 
              _moduleOallowOmitBind :: Bool
              _moduleOexpNmS :: HsNameS
              _moduleOinlMp :: GrInlMp
              _moduleIgathInlMp :: GrInlMp
              _moduleItrf :: GrModule 
              -- copy rule (up)
              _lhsOgathInlMp =
                  _moduleIgathInlMp
              -- copy rule (up)
              _lhsOtrf =
                  _moduleItrf
              -- copy rule (down)
              _moduleOallowOmitBind =
                  _lhsIallowOmitBind
              -- copy rule (down)
              _moduleOexpNmS =
                  _lhsIexpNmS
              -- copy rule (down)
              _moduleOinlMp =
                  _lhsIinlMp
              ( _moduleIgathInlMp,_moduleItrf) =
                  module_ _moduleOallowOmitBind _moduleOexpNmS _moduleOinlMp 
          in  ( _lhsOgathInlMp,_lhsOtrf)))
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         asNmL                : [Maybe HsName]
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Del:
         child off            : GrVal 
         visit 0:
            local willUseFor  : _
            local trf         : _
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
         visit 0:
            local willUseFor  : _
            local trf         : _
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
         visit 0:
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
type T_GrAdapt  = ( ([Maybe HsName]),FvInfoMp,GrAdapt )
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (let _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrAdapt 
         _offOwillUseFor :: WillUseForS
         _offIasNmL :: ([Maybe HsName])
         _offIgathFviMp :: FvInfoMp
         _offInmAlias :: NmAlias
         _offItrf :: GrVal 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 36, column 17)
         _willUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             _offIasNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _offIgathFviMp
         -- self rule
         _trf =
             GrAdapt_Del _offItrf
         -- self rule
         _lhsOtrf =
             _trf
         -- copy rule (from local)
         _offOwillUseFor =
             _willUseFor
         ( _offIasNmL,_offIgathFviMp,_offInmAlias,_offItrf) =
             off_ _offOwillUseFor 
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOtrf))
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (let _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrAdapt 
         _offOwillUseFor :: WillUseForS
         _valOwillUseFor :: WillUseForS
         _offIasNmL :: ([Maybe HsName])
         _offIgathFviMp :: FvInfoMp
         _offInmAlias :: NmAlias
         _offItrf :: GrVal 
         _valIasNmL :: ([Maybe HsName])
         _valIgathFviMp :: FvInfoMp
         _valInmAlias :: NmAlias
         _valItrf :: GrVal 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 36, column 17)
         _willUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             _offIasNmL ++ _valIasNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _offIgathFviMp `fviMpUnion` _valIgathFviMp
         -- self rule
         _trf =
             GrAdapt_Ins _offItrf _valItrf
         -- self rule
         _lhsOtrf =
             _trf
         -- copy rule (from local)
         _offOwillUseFor =
             _willUseFor
         -- copy rule (from local)
         _valOwillUseFor =
             _willUseFor
         ( _offIasNmL,_offIgathFviMp,_offInmAlias,_offItrf) =
             off_ _offOwillUseFor 
         ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
             val_ _valOwillUseFor 
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOtrf))
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (let _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrAdapt 
         _offOwillUseFor :: WillUseForS
         _valOwillUseFor :: WillUseForS
         _offIasNmL :: ([Maybe HsName])
         _offIgathFviMp :: FvInfoMp
         _offInmAlias :: NmAlias
         _offItrf :: GrVal 
         _valIasNmL :: ([Maybe HsName])
         _valIgathFviMp :: FvInfoMp
         _valInmAlias :: NmAlias
         _valItrf :: GrVal 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 36, column 17)
         _willUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             _offIasNmL ++ _valIasNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _offIgathFviMp `fviMpUnion` _valIgathFviMp
         -- self rule
         _trf =
             GrAdapt_Upd _offItrf _valItrf
         -- self rule
         _lhsOtrf =
             _trf
         -- copy rule (from local)
         _offOwillUseFor =
             _willUseFor
         -- copy rule (from local)
         _valOwillUseFor =
             _willUseFor
         ( _offIasNmL,_offIgathFviMp,_offInmAlias,_offItrf) =
             off_ _offOwillUseFor 
         ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
             val_ _valOwillUseFor 
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOtrf))
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         asNmL                : [Maybe HsName]
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = ( ([Maybe HsName]),FvInfoMp,GrAdaptL )
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (let _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrAdaptL 
         _hdIasNmL :: ([Maybe HsName])
         _hdIgathFviMp :: FvInfoMp
         _hdItrf :: GrAdapt 
         _tlIasNmL :: ([Maybe HsName])
         _tlIgathFviMp :: FvInfoMp
         _tlItrf :: GrAdaptL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             _hdIasNmL ++ _tlIasNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIasNmL,_hdIgathFviMp,_hdItrf) =
             hd_ 
         ( _tlIasNmL,_tlIgathFviMp,_tlItrf) =
             tl_ 
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOtrf))
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (let _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrAdaptL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             []
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOtrf))
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inlMp                : GrInlMp
         isCAF                : Bool
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         gathFviMp            : FvInfoMp
         gathInlNmS           : FvS
         inlineCost           : Int
         trf                  : SELF 
         willUseForMp         : WillUseForMp
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = GrInlMp ->
                Bool ->
                Int ->
                ( FvInfoMp,FvS,Int,GrAlt ,Int,WillUseForMp)
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq ->
         (let _lhsOgathFviMp :: FvInfoMp
              _exprOwillUseFor :: WillUseForS
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOtrf :: GrAlt 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _exprOinlMp :: GrInlMp
              _exprOisCAF :: Bool
              _exprOuniq :: Int
              _patIgathFviMp :: FvInfoMp
              _patIintroNmL :: ([HsName])
              _patInmAlias :: NmAlias
              _patItrf :: GrPatAlt 
              _exprIgathFviMp :: FvInfoMp
              _exprIgathInlNmS :: FvS
              _exprIhasSideEffect :: Bool
              _exprIinlineCost :: Int
              _exprIisConWrapper :: Bool
              _exprIisFFIWrapper :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              _exprIuniq :: Int
              _exprIwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 26, column 17)
              _lhsOgathFviMp =
                  _exprIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 27, column 17)
              _exprOwillUseFor =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _exprIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  _exprIinlineCost
              -- self rule
              _trf =
                  GrAlt_Alt ann_ _patItrf _exprItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (up)
              _lhsOwillUseForMp =
                  _exprIwillUseForMp
              -- copy rule (down)
              _exprOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _exprOisCAF =
                  _lhsIisCAF
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              ( _patIgathFviMp,_patIintroNmL,_patInmAlias,_patItrf) =
                  pat_ 
              ( _exprIgathFviMp,_exprIgathInlNmS,_exprIhasSideEffect,_exprIinlineCost,_exprIisConWrapper,_exprIisFFIWrapper,_exprInmAlias,_exprItrf,_exprIuniq,_exprIwillUseForMp) =
                  expr_ _exprOinlMp _exprOisCAF _exprOuniq _exprOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOinlineCost,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inlMp                : GrInlMp
         isCAF                : Bool
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         gathFviMp            : FvInfoMp
         gathInlNmS           : FvS
         inlineCost           : Int
         trf                  : SELF 
         willUseForMpL        : [WillUseForMp]
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = GrInlMp ->
                 Bool ->
                 Int ->
                 ( FvInfoMp,FvS,Int,GrAltL ,Int,([WillUseForMp]))
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq ->
         (let _lhsOwillUseForMpL :: ([WillUseForMp])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOtrf :: GrAltL 
              _lhsOuniq :: Int
              _hdOinlMp :: GrInlMp
              _hdOisCAF :: Bool
              _hdOuniq :: Int
              _tlOinlMp :: GrInlMp
              _tlOisCAF :: Bool
              _tlOuniq :: Int
              _hdIgathFviMp :: FvInfoMp
              _hdIgathInlNmS :: FvS
              _hdIinlineCost :: Int
              _hdItrf :: GrAlt 
              _hdIuniq :: Int
              _hdIwillUseForMp :: WillUseForMp
              _tlIgathFviMp :: FvInfoMp
              _tlIgathInlNmS :: FvS
              _tlIinlineCost :: Int
              _tlItrf :: GrAltL 
              _tlIuniq :: Int
              _tlIwillUseForMpL :: ([WillUseForMp])
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 15, column 25)
              _lhsOwillUseForMpL =
                  _hdIwillUseForMp : _tlIwillUseForMpL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _hdIgathInlNmS `Set.union` _tlIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  _hdIinlineCost + _tlIinlineCost
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _hdOisCAF =
                  _lhsIisCAF
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _tlOisCAF =
                  _lhsIisCAF
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIgathFviMp,_hdIgathInlNmS,_hdIinlineCost,_hdItrf,_hdIuniq,_hdIwillUseForMp) =
                  hd_ _hdOinlMp _hdOisCAF _hdOuniq 
              ( _tlIgathFviMp,_tlIgathInlNmS,_tlIinlineCost,_tlItrf,_tlIuniq,_tlIwillUseForMpL) =
                  tl_ _tlOinlMp _tlOisCAF _tlOuniq 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOinlineCost,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMpL)))
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq ->
         (let _lhsOwillUseForMpL :: ([WillUseForMp])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOtrf :: GrAltL 
              _lhsOuniq :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 14, column 25)
              _lhsOwillUseForMpL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOinlineCost,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMpL)))
-- GrBind ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowOmitBind        : Bool
         expNmS               : HsNameS
         fviMp                : FvInfoMp
         inlMp                : GrInlMp
         inlNmS               : FvS
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         gathFviMp            : FvInfoMp
         gathInlMp            : GrInlMp
         gathInlNmS           : FvS
         introNmS             : Set.Set HsName
         trf                  : SELF 
         trfSq                : Seq.FastSeq GrBind
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
            local _tup1       : _
            local omitBind    : _
            local isCAF       : _
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
type T_GrBind  = Bool ->
                 HsNameS ->
                 FvInfoMp ->
                 GrInlMp ->
                 FvS ->
                 Int ->
                 ( FvInfoMp,GrInlMp,FvS,(Set.Set HsName),GrBind ,(Seq.FastSeq GrBind),Int)
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIfviMp
       _lhsIinlMp
       _lhsIinlNmS
       _lhsIuniq ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlMp :: GrInlMp
              _lhsOgathInlNmS :: FvS
              _lhsOintroNmS :: (Set.Set HsName)
              _lhsOtrfSq :: (Seq.FastSeq GrBind)
              _lhsOtrf :: GrBind 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 76, column 30)
              _lhsOgathInlMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 61, column 29)
              _lhsOintroNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 202, column 26)
              _lhsOtrfSq =
                  Seq.empty
              -- self rule
              _trf =
                  GrBind_Arity nm_ arity_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOgathFviMp,_lhsOgathInlMp,_lhsOgathInlNmS,_lhsOintroNmS,_lhsOtrf,_lhsOtrfSq,_lhsOuniq)))
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIfviMp
       _lhsIinlMp
       _lhsIinlNmS
       _lhsIuniq ->
         (let _lhsOintroNmS :: (Set.Set HsName)
              _exprOwillUseFor :: WillUseForS
              _lhsOgathInlMp :: GrInlMp
              _lhsOtrfSq :: (Seq.FastSeq GrBind)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOtrf :: GrBind 
              _lhsOuniq :: Int
              _exprOinlMp :: GrInlMp
              _exprOisCAF :: Bool
              _exprOuniq :: Int
              _exprIgathFviMp :: FvInfoMp
              _exprIgathInlNmS :: FvS
              _exprIhasSideEffect :: Bool
              _exprIinlineCost :: Int
              _exprIisConWrapper :: Bool
              _exprIisFFIWrapper :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              _exprIuniq :: Int
              _exprIwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 64, column 17)
              _lhsOintroNmS =
                  Set.singleton nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 72, column 17)
              _exprOwillUseFor =
                  Set.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 80, column 17)
              __tup1 =
                  case Map.lookup nm_ _lhsIfviMp of
                    Just (FvInfo 1 use)
                      | not _isCAF
                        && FvUse_Call `Set.member` use
                        && not (nm_ `Set.member` _lhsIexpNmS)
                        -> (Map.singleton nm_ (GrInl_Call argNmL_ _exprItrf), True)
                    Just (FvInfo 1 use)
                      | _isCAF
                        && willUseForEval nm_ _exprIwillUseForMp
                        && FvUse_Val `Set.member` use
                        && not (nm_ `Set.member` _lhsIexpNmS)
                        -> (Map.singleton nm_ (GrInl_CAF _exprItrf), True)
                    _ | not _isCAF
                        && (  _exprIisFFIWrapper
                           || _exprIisConWrapper
                           || _exprIinlineCost <= 10
                           )
                        -> (Map.singleton nm_ (GrInl_Call argNmL_ _exprItrf), False)
                      | otherwise
                        -> (Map.empty, False)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 80, column 17)
              (_lhsOgathInlMp,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 80, column 17)
              (_,_omitBind) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 150, column 17)
              _isCAF =
                  null argNmL_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 208, column 17)
              _lhsOtrfSq =
                  if _lhsIallowOmitBind && _omitBind     && nm_ `Set.member` _lhsIinlNmS
                  then Seq.empty
                  else Seq.singleton _trf
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 29, column 17)
              _lhsOgathFviMp =
                  _exprIgathFviMp `fviMpDifference` fviMpFromList argNmL_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _exprIgathInlNmS
              -- self rule
              _trf =
                  GrBind_Bind nm_ annot_ argNmL_ _exprItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _exprIuniq
              -- copy rule (down)
              _exprOinlMp =
                  _lhsIinlMp
              -- copy rule (from local)
              _exprOisCAF =
                  _isCAF
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              ( _exprIgathFviMp,_exprIgathInlNmS,_exprIhasSideEffect,_exprIinlineCost,_exprIisConWrapper,_exprIisFFIWrapper,_exprInmAlias,_exprItrf,_exprIuniq,_exprIwillUseForMp) =
                  expr_ _exprOinlMp _exprOisCAF _exprOuniq _exprOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlMp,_lhsOgathInlNmS,_lhsOintroNmS,_lhsOtrf,_lhsOtrfSq,_lhsOuniq)))
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIfviMp
       _lhsIinlMp
       _lhsIinlNmS
       _lhsIuniq ->
         (let _lhsOgathInlMp :: GrInlMp
              _lhsOtrfSq :: (Seq.FastSeq GrBind)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOintroNmS :: (Set.Set HsName)
              _lhsOtrf :: GrBind 
              _lhsOuniq :: Int
              _bindLOallowOmitBind :: Bool
              _bindLOexpNmS :: HsNameS
              _bindLOfviMp :: FvInfoMp
              _bindLOinlMp :: GrInlMp
              _bindLOinlNmS :: FvS
              _bindLOuniq :: Int
              _bindLIgathFviMp :: FvInfoMp
              _bindLIgathInlMp :: GrInlMp
              _bindLIgathInlNmS :: FvS
              _bindLIintroNmS :: (Set.Set HsName)
              _bindLItrf :: GrBindL 
              _bindLItrfSq :: (Seq.FastSeq GrBind)
              _bindLIuniq :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 101, column 17)
              _lhsOgathInlMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 211, column 17)
              _lhsOtrfSq =
                  Seq.singleton $ GrBind_Rec $ Seq.toList _bindLItrfSq
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _bindLIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _bindLIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 61, column 29)
              _lhsOintroNmS =
                  _bindLIintroNmS
              -- self rule
              _trf =
                  GrBind_Rec _bindLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _bindLIuniq
              -- copy rule (down)
              _bindLOallowOmitBind =
                  _lhsIallowOmitBind
              -- copy rule (down)
              _bindLOexpNmS =
                  _lhsIexpNmS
              -- copy rule (down)
              _bindLOfviMp =
                  _lhsIfviMp
              -- copy rule (down)
              _bindLOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _bindLOinlNmS =
                  _lhsIinlNmS
              -- copy rule (down)
              _bindLOuniq =
                  _lhsIuniq
              ( _bindLIgathFviMp,_bindLIgathInlMp,_bindLIgathInlNmS,_bindLIintroNmS,_bindLItrf,_bindLItrfSq,_bindLIuniq) =
                  bindL_ _bindLOallowOmitBind _bindLOexpNmS _bindLOfviMp _bindLOinlMp _bindLOinlNmS _bindLOuniq 
          in  ( _lhsOgathFviMp,_lhsOgathInlMp,_lhsOgathInlNmS,_lhsOintroNmS,_lhsOtrf,_lhsOtrfSq,_lhsOuniq)))
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowOmitBind        : Bool
         expNmS               : HsNameS
         fviMp                : FvInfoMp
         inlMp                : GrInlMp
         inlNmS               : FvS
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         gathFviMp            : FvInfoMp
         gathInlMp            : GrInlMp
         gathInlNmS           : FvS
         introNmS             : Set.Set HsName
         trf                  : SELF 
         trfSq                : Seq.FastSeq GrBind
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
type T_GrBindL  = Bool ->
                  HsNameS ->
                  FvInfoMp ->
                  GrInlMp ->
                  FvS ->
                  Int ->
                  ( FvInfoMp,GrInlMp,FvS,(Set.Set HsName),GrBindL ,(Seq.FastSeq GrBind),Int)
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIfviMp
       _lhsIinlMp
       _lhsIinlNmS
       _lhsIuniq ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlMp :: GrInlMp
              _lhsOgathInlNmS :: FvS
              _lhsOintroNmS :: (Set.Set HsName)
              _lhsOtrfSq :: (Seq.FastSeq GrBind)
              _lhsOtrf :: GrBindL 
              _lhsOuniq :: Int
              _hdOallowOmitBind :: Bool
              _hdOexpNmS :: HsNameS
              _hdOfviMp :: FvInfoMp
              _hdOinlMp :: GrInlMp
              _hdOinlNmS :: FvS
              _hdOuniq :: Int
              _tlOallowOmitBind :: Bool
              _tlOexpNmS :: HsNameS
              _tlOfviMp :: FvInfoMp
              _tlOinlMp :: GrInlMp
              _tlOinlNmS :: FvS
              _tlOuniq :: Int
              _hdIgathFviMp :: FvInfoMp
              _hdIgathInlMp :: GrInlMp
              _hdIgathInlNmS :: FvS
              _hdIintroNmS :: (Set.Set HsName)
              _hdItrf :: GrBind 
              _hdItrfSq :: (Seq.FastSeq GrBind)
              _hdIuniq :: Int
              _tlIgathFviMp :: FvInfoMp
              _tlIgathInlMp :: GrInlMp
              _tlIgathInlNmS :: FvS
              _tlIintroNmS :: (Set.Set HsName)
              _tlItrf :: GrBindL 
              _tlItrfSq :: (Seq.FastSeq GrBind)
              _tlIuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 76, column 30)
              _lhsOgathInlMp =
                  _hdIgathInlMp `Map.union` _tlIgathInlMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _hdIgathInlNmS `Set.union` _tlIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 61, column 29)
              _lhsOintroNmS =
                  _hdIintroNmS `Set.union` _tlIintroNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 202, column 26)
              _lhsOtrfSq =
                  _hdItrfSq Seq.:++: _tlItrfSq
              -- self rule
              _trf =
                  (:) _hdItrf _tlItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _tlIuniq
              -- copy rule (down)
              _hdOallowOmitBind =
                  _lhsIallowOmitBind
              -- copy rule (down)
              _hdOexpNmS =
                  _lhsIexpNmS
              -- copy rule (down)
              _hdOfviMp =
                  _lhsIfviMp
              -- copy rule (down)
              _hdOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _hdOinlNmS =
                  _lhsIinlNmS
              -- copy rule (down)
              _hdOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _tlOallowOmitBind =
                  _lhsIallowOmitBind
              -- copy rule (down)
              _tlOexpNmS =
                  _lhsIexpNmS
              -- copy rule (down)
              _tlOfviMp =
                  _lhsIfviMp
              -- copy rule (down)
              _tlOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _tlOinlNmS =
                  _lhsIinlNmS
              -- copy rule (chain)
              _tlOuniq =
                  _hdIuniq
              ( _hdIgathFviMp,_hdIgathInlMp,_hdIgathInlNmS,_hdIintroNmS,_hdItrf,_hdItrfSq,_hdIuniq) =
                  hd_ _hdOallowOmitBind _hdOexpNmS _hdOfviMp _hdOinlMp _hdOinlNmS _hdOuniq 
              ( _tlIgathFviMp,_tlIgathInlMp,_tlIgathInlNmS,_tlIintroNmS,_tlItrf,_tlItrfSq,_tlIuniq) =
                  tl_ _tlOallowOmitBind _tlOexpNmS _tlOfviMp _tlOinlMp _tlOinlNmS _tlOuniq 
          in  ( _lhsOgathFviMp,_lhsOgathInlMp,_lhsOgathInlNmS,_lhsOintroNmS,_lhsOtrf,_lhsOtrfSq,_lhsOuniq)))
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIfviMp
       _lhsIinlMp
       _lhsIinlNmS
       _lhsIuniq ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlMp :: GrInlMp
              _lhsOgathInlNmS :: FvS
              _lhsOintroNmS :: (Set.Set HsName)
              _lhsOtrfSq :: (Seq.FastSeq GrBind)
              _lhsOtrf :: GrBindL 
              _lhsOuniq :: Int
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 76, column 30)
              _lhsOgathInlMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 61, column 29)
              _lhsOintroNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 202, column 26)
              _lhsOtrfSq =
                  Seq.empty
              -- self rule
              _trf =
                  []
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
          in  ( _lhsOgathFviMp,_lhsOgathInlMp,_lhsOgathInlNmS,_lhsOintroNmS,_lhsOtrf,_lhsOtrfSq,_lhsOuniq)))
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inlMp                : GrInlMp
         isCAF                : Bool
         willUseFor           : WillUseForS
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         gathFviMp            : FvInfoMp
         gathInlNmS           : FvS
         hasSideEffect        : Bool
         inlineCost           : Int
         isConWrapper         : Bool
         isFFIWrapper         : Bool
         nmAlias              : NmAlias
         trf                  : SELF 
         willUseForMp         : WillUseForMp
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local _tup2       : _
            local gathInlNmS  : _
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local _tup3       : _
            local grVarTrf    : _
            local grVarInlNmS : _
            local _tup4       : _
            local gathInlNmS  : _
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Eval:
         child nm             : {HsName}
         visit 0:
            local _tup5       : _
            local gathInlNmS  : _
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative FetchNode:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
         visit 0:
            local gathBodyFviMp : {FvInfoMp}
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Store:
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Throw:
         child nm             : {HsName}
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local gathFviMp   : _
            local willUseForMp : {WillUseForMp}
            local trf         : _
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
type T_GrExpr  = GrInlMp ->
                 Bool ->
                 Int ->
                 WillUseForS ->
                 ( FvInfoMp,FvS,Bool,Int,Bool,Bool,NmAlias,GrExpr ,Int,WillUseForMp)
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _argLIasNmL :: ([Maybe HsName])
              _argLIgathFviMp :: FvInfoMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 193, column 17)
              __tup2 =
                  if _lhsIisCAF
                  then let (grVarTrf,grVarInlNmS) = inlGrVar _lhsIinlMp (nm_ : Map.keys _argLIgathFviMp)
                       in  (grVarTrf _trf, grVarInlNmS)
                  else (_trf, Set.empty)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 193, column 17)
              (_lhsOtrf,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 193, column 17)
              (_,_gathInlNmS) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 14, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _gathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_App nm_ _argLItrf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              ( _argLIasNmL,_argLIgathFviMp,_argLInmAliasL,_argLItrf) =
                  argL_ 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOuniq :: Int
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOwillUseForMp :: WillUseForMp
              _argLIasNmL :: ([Maybe HsName])
              _argLIgathFviMp :: FvInfoMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 155, column 25)
              _lhsOuniq =
                  _lhsIuniq+1
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 181, column 33)
              __tup3 =
                  if _lhsIisCAF
                  then inlGrVar _lhsIinlMp (Map.keys _argLIgathFviMp)
                  else (id,Set.empty)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 181, column 33)
              (_grVarTrf,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 181, column 33)
              (_,_grVarInlNmS) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 187, column 17)
              __tup4 =
                  case Map.lookup nm_ _lhsIinlMp of
                    Just (GrInl_Call as e) | inlNmsAreInlineable _argLIasNmL
                      -> ( _grVarTrf $ inlRename _lhsIuniq _argLIasNmL as e
                         , Set.insert nm_ _grVarInlNmS
                         )
                    _ -> (_grVarTrf _trf, _grVarInlNmS)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 187, column 17)
              (_lhsOtrf,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 187, column 17)
              (_,_gathInlNmS) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 14, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _gathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Call nm_ _argLItrf
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              ( _argLIasNmL,_argLIgathFviMp,_argLInmAliasL,_argLItrf) =
                  argL_ 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _valOwillUseFor :: WillUseForS
              _altLOinlMp :: GrInlMp
              _altLOisCAF :: Bool
              _altLOuniq :: Int
              _valIasNmL :: ([Maybe HsName])
              _valIgathFviMp :: FvInfoMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              _altLIgathFviMp :: FvInfoMp
              _altLIgathInlNmS :: FvS
              _altLIinlineCost :: Int
              _altLItrf :: GrAltL 
              _altLIuniq :: Int
              _altLIwillUseForMpL :: ([WillUseForMp])
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 23, column 17)
              _gathFviMp =
                  fviMpUnions [_valIgathFviMp, _altLIgathFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 6, column 25)
              _willUseForMp =
                  foldr1 willUseIntersection _altLIwillUseForMpL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _altLIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  _altLIinlineCost
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Case _valItrf _altLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _altLIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _valOwillUseFor =
                  _lhsIwillUseFor
              -- copy rule (down)
              _altLOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _altLOisCAF =
                  _lhsIisCAF
              -- copy rule (down)
              _altLOuniq =
                  _lhsIuniq
              ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
                  val_ _valOwillUseFor 
              ( _altLIgathFviMp,_altLIgathInlNmS,_altLIinlineCost,_altLItrf,_altLIuniq,_altLIwillUseForMpL) =
                  altL_ _altLOinlMp _altLOisCAF _altLOuniq 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _bodyOinlMp :: GrInlMp
              _bodyOisCAF :: Bool
              _bodyOuniq :: Int
              _bodyOwillUseFor :: WillUseForS
              _handlerOinlMp :: GrInlMp
              _handlerOisCAF :: Bool
              _handlerOuniq :: Int
              _handlerOwillUseFor :: WillUseForS
              _bodyIgathFviMp :: FvInfoMp
              _bodyIgathInlNmS :: FvS
              _bodyIhasSideEffect :: Bool
              _bodyIinlineCost :: Int
              _bodyIisConWrapper :: Bool
              _bodyIisFFIWrapper :: Bool
              _bodyInmAlias :: NmAlias
              _bodyItrf :: GrExpr 
              _bodyIuniq :: Int
              _bodyIwillUseForMp :: WillUseForMp
              _handlerIgathFviMp :: FvInfoMp
              _handlerIgathInlNmS :: FvS
              _handlerIhasSideEffect :: Bool
              _handlerIinlineCost :: Int
              _handlerIisConWrapper :: Bool
              _handlerIisFFIWrapper :: Bool
              _handlerInmAlias :: NmAlias
              _handlerItrf :: GrExpr 
              _handlerIuniq :: Int
              _handlerIwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 19, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton arg_, _bodyIgathFviMp, _handlerIgathFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _bodyIgathInlNmS `Set.union` _handlerIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  _bodyIinlineCost + _handlerIinlineCost
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  _bodyIisFFIWrapper || _handlerIisFFIWrapper
              -- self rule
              _trf =
                  GrExpr_Catch _bodyItrf arg_ _handlerItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _handlerIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _bodyOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _bodyOisCAF =
                  _lhsIisCAF
              -- copy rule (down)
              _bodyOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _bodyOwillUseFor =
                  _lhsIwillUseFor
              -- copy rule (down)
              _handlerOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _handlerOisCAF =
                  _lhsIisCAF
              -- copy rule (chain)
              _handlerOuniq =
                  _bodyIuniq
              -- copy rule (down)
              _handlerOwillUseFor =
                  _lhsIwillUseFor
              ( _bodyIgathFviMp,_bodyIgathInlNmS,_bodyIhasSideEffect,_bodyIinlineCost,_bodyIisConWrapper,_bodyIisFFIWrapper,_bodyInmAlias,_bodyItrf,_bodyIuniq,_bodyIwillUseForMp) =
                  body_ _bodyOinlMp _bodyOisCAF _bodyOuniq _bodyOwillUseFor 
              ( _handlerIgathFviMp,_handlerIgathInlNmS,_handlerIhasSideEffect,_handlerIinlineCost,_handlerIisConWrapper,_handlerIisFFIWrapper,_handlerInmAlias,_handlerItrf,_handlerIuniq,_handlerIwillUseForMp) =
                  handler_ _handlerOinlMp _handlerOisCAF _handlerOuniq _handlerOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 197, column 17)
              __tup5 =
                  case Map.lookup nm_ _lhsIinlMp of
                    Just (GrInl_CAF e) | _lhsIisCAF
                      -> (e, Set.singleton nm_)
                    _ -> (_trf, Set.empty)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 197, column 17)
              (_lhsOtrf,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 197, column 17)
              (_,_gathInlNmS) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 41, column 17)
              _lhsOnmAlias =
                  NmAlias_Eval nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 54, column 25)
              _lhsOhasSideEffect =
                  True
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 7, column 17)
              _willUseForMp =
                  Map.singleton nm_ (Set.singleton WillUseFor_Eval)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _gathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Eval nm_
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisFFIWrapper :: Bool
              _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _argLIasNmL :: ([Maybe HsName])
              _argLIgathFviMp :: FvInfoMp
              _argLInmAliasL :: ([NmAlias])
              _argLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 106, column 17)
              _lhsOisFFIWrapper =
                  True
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 15, column 17)
              _gathFviMp =
                  _argLIgathFviMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- self rule
              _trf =
                  GrExpr_FFI callconv_ impEnt_ ffiAnnot_ _argLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              ( _argLIasNmL,_argLIgathFviMp,_argLInmAliasL,_argLItrf) =
                  argL_ 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_FetchField nm_ offset_ mbTag_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_FetchNode nm_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 18, column 17)
              _gathFviMp =
                  fviMpFromList [src_,dst_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_FetchUpdate src_ dst_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _gathBodyFviMp :: FvInfoMp
              _willUseForMp :: WillUseForMp
              _exprOwillUseFor :: WillUseForS
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _exprOinlMp :: GrInlMp
              _exprOisCAF :: Bool
              _exprOuniq :: Int
              _bodyOinlMp :: GrInlMp
              _bodyOisCAF :: Bool
              _bodyOuniq :: Int
              _bodyOwillUseFor :: WillUseForS
              _exprIgathFviMp :: FvInfoMp
              _exprIgathInlNmS :: FvS
              _exprIhasSideEffect :: Bool
              _exprIinlineCost :: Int
              _exprIisConWrapper :: Bool
              _exprIisFFIWrapper :: Bool
              _exprInmAlias :: NmAlias
              _exprItrf :: GrExpr 
              _exprIuniq :: Int
              _exprIwillUseForMp :: WillUseForMp
              _patIgathFviMp :: FvInfoMp
              _patIintroNmL :: ([HsName])
              _patInmAlias :: NmAlias
              _patItrf :: GrPatLam 
              _bodyIgathFviMp :: FvInfoMp
              _bodyIgathInlNmS :: FvS
              _bodyIhasSideEffect :: Bool
              _bodyIinlineCost :: Int
              _bodyIisConWrapper :: Bool
              _bodyIisFFIWrapper :: Bool
              _bodyInmAlias :: NmAlias
              _bodyItrf :: GrExpr 
              _bodyIuniq :: Int
              _bodyIwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 38, column 17)
              _lhsOnmAlias =
                  case (_bodyInmAlias,_exprIhasSideEffect) of
                    (NmAlias_Nm n,True) -> NmAlias_NmAfterSideEffect n
                    _ -> _bodyInmAlias
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 12, column 17)
              _gathBodyFviMp =
                  _bodyIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 12, column 17)
              _gathFviMp =
                  fviMpUnions [_exprIgathFviMp, _gathBodyFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 5, column 17)
              _willUseForMp =
                  willUseUnion _exprIwillUseForMp _bodyIwillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 20, column 17)
              _exprOwillUseFor =
                  case _patInmAlias of
                  NmAlias_Nm nmp
                    ->
                       willUseFor nmp _bodyIwillUseForMp
                  _ -> Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  _exprIgathInlNmS `Set.union` _bodyIgathInlNmS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  _exprIinlineCost + _bodyIinlineCost
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  _exprIisFFIWrapper || _bodyIisFFIWrapper
              -- self rule
              _trf =
                  GrExpr_Seq _exprItrf _patItrf _bodyItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOuniq =
                  _bodyIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _exprOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _exprOisCAF =
                  _lhsIisCAF
              -- copy rule (down)
              _exprOuniq =
                  _lhsIuniq
              -- copy rule (down)
              _bodyOinlMp =
                  _lhsIinlMp
              -- copy rule (down)
              _bodyOisCAF =
                  _lhsIisCAF
              -- copy rule (chain)
              _bodyOuniq =
                  _exprIuniq
              -- copy rule (down)
              _bodyOwillUseFor =
                  _lhsIwillUseFor
              ( _exprIgathFviMp,_exprIgathInlNmS,_exprIhasSideEffect,_exprIinlineCost,_exprIisConWrapper,_exprIisFFIWrapper,_exprInmAlias,_exprItrf,_exprIuniq,_exprIwillUseForMp) =
                  expr_ _exprOinlMp _exprOisCAF _exprOuniq _exprOwillUseFor 
              ( _patIgathFviMp,_patIintroNmL,_patInmAlias,_patItrf) =
                  pat_ 
              ( _bodyIgathFviMp,_bodyIgathInlNmS,_bodyIhasSideEffect,_bodyIinlineCost,_bodyIisConWrapper,_bodyIisFFIWrapper,_bodyInmAlias,_bodyItrf,_bodyIuniq,_bodyIwillUseForMp) =
                  body_ _bodyOinlMp _bodyOisCAF _bodyOuniq _bodyOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _valOwillUseFor :: WillUseForS
              _valIasNmL :: ([Maybe HsName])
              _valIgathFviMp :: FvInfoMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 20, column 17)
              _gathFviMp =
                  _valIgathFviMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Store _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOnmAlias =
                  _valInmAlias
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _valOwillUseFor =
                  _lhsIwillUseFor
              ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
                  val_ _valOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Throw nm_
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOnmAlias :: NmAlias
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _valOwillUseFor :: WillUseForS
              _valIasNmL :: ([Maybe HsName])
              _valIgathFviMp :: FvInfoMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              _typeIgathFviMp :: FvInfoMp
              _typeItrf :: GrType 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 111, column 18)
              _lhsOisConWrapper =
                  True
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 21, column 17)
              _gathFviMp =
                  _valIgathFviMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_Unit _valItrf _typeItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (up)
              _lhsOnmAlias =
                  _valInmAlias
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _valOwillUseFor =
                  _lhsIwillUseFor
              ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
                  val_ _valOwillUseFor 
              ( _typeIgathFviMp,_typeItrf) =
                  type_ 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (\ _lhsIinlMp
       _lhsIisCAF
       _lhsIuniq
       _lhsIwillUseFor ->
         (let _lhsOisConWrapper :: Bool
              _lhsOnmAlias :: NmAlias
              _lhsOhasSideEffect :: Bool
              _willUseForMp :: WillUseForMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathInlNmS :: FvS
              _lhsOinlineCost :: Int
              _lhsOisFFIWrapper :: Bool
              _lhsOtrf :: GrExpr 
              _lhsOuniq :: Int
              _lhsOwillUseForMp :: WillUseForMp
              _valOwillUseFor :: WillUseForS
              _valIasNmL :: ([Maybe HsName])
              _valIgathFviMp :: FvInfoMp
              _valInmAlias :: NmAlias
              _valItrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 112, column 18)
              _lhsOisConWrapper =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 43, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 55, column 17)
              _lhsOhasSideEffect =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 22, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton nm_, _valIgathFviMp]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 9, column 17)
              _willUseForMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 133, column 41)
              _lhsOgathInlNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 114, column 33)
              _lhsOinlineCost =
                  1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 103, column 32)
              _lhsOisFFIWrapper =
                  False
              -- self rule
              _trf =
                  GrExpr_UpdateUnit nm_ _valItrf
              -- self rule
              _lhsOtrf =
                  _trf
              -- copy rule (chain)
              _lhsOuniq =
                  _lhsIuniq
              -- copy rule (from local)
              _lhsOwillUseForMp =
                  _willUseForMp
              -- copy rule (down)
              _valOwillUseFor =
                  _lhsIwillUseFor
              ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
                  val_ _valOwillUseFor 
          in  ( _lhsOgathFviMp,_lhsOgathInlNmS,_lhsOhasSideEffect,_lhsOinlineCost,_lhsOisConWrapper,_lhsOisFFIWrapper,_lhsOnmAlias,_lhsOtrf,_lhsOuniq,_lhsOwillUseForMp)))
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
    (let 
     in  ( ))
-- GrGlobal ----------------------------------------------------
{-
   visit 0:
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
type T_GrGlobal  = ( FvInfoMp,GrGlobal )
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrGlobal 
         _valOwillUseFor :: WillUseForS
         _valIasNmL :: ([Maybe HsName])
         _valIgathFviMp :: FvInfoMp
         _valInmAlias :: NmAlias
         _valItrf :: GrVal 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 30, column 17)
         _willUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _valIgathFviMp
         -- self rule
         _trf =
             GrGlobal_Global nm_ _valItrf
         -- self rule
         _lhsOtrf =
             _trf
         -- copy rule (from local)
         _valOwillUseFor =
             _willUseFor
         ( _valIasNmL,_valIgathFviMp,_valInmAlias,_valItrf) =
             val_ _valOwillUseFor 
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
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
type T_GrGlobalL  = ( FvInfoMp,GrGlobalL )
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrGlobalL 
         _hdIgathFviMp :: FvInfoMp
         _hdItrf :: GrGlobal 
         _tlIgathFviMp :: FvInfoMp
         _tlItrf :: GrGlobalL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIgathFviMp,_hdItrf) =
             hd_ 
         ( _tlIgathFviMp,_tlItrf) =
             tl_ 
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrGlobalL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrModule ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowOmitBind        : Bool
         expNmS               : HsNameS
         inlMp                : GrInlMp
      synthesized attributes:
         gathInlMp            : GrInlMp
         trf                  : SELF 
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
         visit 0:
            local inlNmS      : _
            local uniq        : _
            local trf         : _
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = Bool ->
                   HsNameS ->
                   GrInlMp ->
                   ( GrInlMp,GrModule )
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (\ _lhsIallowOmitBind
       _lhsIexpNmS
       _lhsIinlMp ->
         (let _bindLOfviMp :: FvInfoMp
              _bindLOinlMp :: GrInlMp
              _lhsOgathInlMp :: GrInlMp
              _lhsOtrf :: GrModule 
              _bindLOallowOmitBind :: Bool
              _bindLOexpNmS :: HsNameS
              _bindLOinlNmS :: FvS
              _bindLOuniq :: Int
              _globalLIgathFviMp :: FvInfoMp
              _globalLItrf :: GrGlobalL 
              _bindLIgathFviMp :: FvInfoMp
              _bindLIgathInlMp :: GrInlMp
              _bindLIgathInlNmS :: FvS
              _bindLIintroNmS :: (Set.Set HsName)
              _bindLItrf :: GrBindL 
              _bindLItrfSq :: (Seq.FastSeq GrBind)
              _bindLIuniq :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 69, column 17)
              _bindLOfviMp =
                  _bindLIgathFviMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 129, column 17)
              _bindLOinlMp =
                  _bindLIgathInlMp `Map.union` _lhsIinlMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 130, column 17)
              _lhsOgathInlMp =
                  let onlyInThisModule = _bindLIintroNmS `Set.difference` _lhsIexpNmS
                  in  Map.filterWithKey (inlMayExport onlyInThisModule _lhsIexpNmS) _bindLIgathInlMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 137, column 17)
              _inlNmS =
                  _bindLIgathInlNmS
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 158, column 17)
              _uniq =
                  0
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 205, column 17)
              _lhsOtrf =
                  GrModule_Mod moduleNm_ _globalLItrf (Seq.toList _bindLItrfSq) tagsMp_
              -- self rule
              _trf =
                  GrModule_Mod moduleNm_ _globalLItrf _bindLItrf tagsMp_
              -- copy rule (down)
              _bindLOallowOmitBind =
                  _lhsIallowOmitBind
              -- copy rule (down)
              _bindLOexpNmS =
                  _lhsIexpNmS
              -- copy rule (from local)
              _bindLOinlNmS =
                  _inlNmS
              -- copy rule (from local)
              _bindLOuniq =
                  _uniq
              ( _globalLIgathFviMp,_globalLItrf) =
                  globalL_ 
              ( _bindLIgathFviMp,_bindLIgathInlMp,_bindLIgathInlNmS,_bindLIintroNmS,_bindLItrf,_bindLItrfSq,_bindLIuniq) =
                  bindL_ _bindLOallowOmitBind _bindLOexpNmS _bindLOfviMp _bindLOinlMp _bindLOinlNmS _bindLOuniq 
          in  ( _lhsOgathInlMp,_lhsOtrf)))
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative LitInt:
         child int            : {Int}
         visit 0:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
         visit 0:
            local trf         : _
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
         visit 0:
            local trf         : _
      alternative Otherwise:
         visit 0:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 0:
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
type T_GrPatAlt  = ( FvInfoMp,([HsName]),NmAlias,GrPatAlt )
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrPatAlt 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             GrPatAlt_LitInt int_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatAlt 
         _tagIgathFviMp :: FvInfoMp
         _tagItrf :: GrTag 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 5, column 17)
         _lhsOnmAlias =
             NmAlias_Grp hsnUnknown $ map NmAlias_Nm fldL_
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 4, column 17)
         _lhsOintroNmL =
             fldL_
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _tagIgathFviMp
         -- self rule
         _trf =
             GrPatAlt_Node _tagItrf fldL_
         -- self rule
         _lhsOtrf =
             _trf
         ( _tagIgathFviMp,_tagItrf) =
             tag_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatAlt 
         _tagIgathFviMp :: FvInfoMp
         _tagItrf :: GrTag 
         _fldLIgathFviMp :: FvInfoMp
         _fldLIintroNmL :: ([HsName])
         _fldLItrf :: GrSplitL 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 5, column 17)
         _lhsOintroNmL =
             nm_ : _fldLIintroNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _tagIgathFviMp `fviMpUnion` _fldLIgathFviMp
         -- self rule
         _trf =
             GrPatAlt_NodeSplit _tagItrf nm_ _fldLItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _tagIgathFviMp,_tagItrf) =
             tag_ 
         ( _fldLIgathFviMp,_fldLIintroNmL,_fldLItrf) =
             fldL_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrPatAlt 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             GrPatAlt_Otherwise
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrPatAlt 
         _tagIgathFviMp :: FvInfoMp
         _tagItrf :: GrTag 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 6, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _tagIgathFviMp
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             GrPatAlt_Tag _tagItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _tagIgathFviMp,_tagItrf) =
             tag_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Empty:
         visit 0:
            local trf         : _
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative OpaqueAnnot:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative VarNode:
         child fldL           : GrVarL 
         visit 0:
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
type T_GrPatLam  = ( FvInfoMp,([HsName]),NmAlias,GrPatLam )
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 10, column 17)
         _lhsOnmAlias =
             case annot_ of
               BasicAnnot_Size bsz _ _ _
                 | basicSizeIsWord bsz  -> NmAlias_Nm nm_
               _                        -> NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_BasicAnnot annot_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 15, column 17)
         _lhsOnmAlias =
             NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) annot_
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_BasicNode annot_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             GrPatLam_Empty
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_EnumAnnot tycon_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_EnumNode nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_OpaqueAnnot nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_OpaqueNode nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_PtrAnnot tycon_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 20, column 17)
         _lhsOnmAlias =
             NmAlias_None
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_PtrNode nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 9, column 25)
         _lhsOnmAlias =
             NmAlias_Nm    nm_
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 8, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrPatLam_Var nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (let _lhsOnmAlias :: NmAlias
         _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrPatLam 
         _fldLIgathFviMp :: FvInfoMp
         _fldLIintroNmL :: ([HsName])
         _fldLItrf :: GrVarL 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 14, column 17)
         _lhsOnmAlias =
             NmAlias_Grp   hsnUnknown $ map NmAlias_Nm (tail _fldLIintroNmL)
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 9, column 17)
         _lhsOintroNmL =
             tail _fldLIintroNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _fldLIgathFviMp
         -- self rule
         _trf =
             GrPatLam_VarNode _fldLItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _fldLIgathFviMp,_fldLIintroNmL,_fldLItrf) =
             fldL_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOnmAlias,_lhsOtrf))
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         trf                  : SELF 
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
         visit 0:
            local willUseFor  : _
            local trf         : _
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = ( FvInfoMp,([HsName]),GrSplit )
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (let _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrSplit 
         _offOwillUseFor :: WillUseForS
         _offIasNmL :: ([Maybe HsName])
         _offIgathFviMp :: FvInfoMp
         _offInmAlias :: NmAlias
         _offItrf :: GrVal 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 19, column 17)
         _lhsOintroNmL =
             [nm_]
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 39, column 17)
         _willUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _offIgathFviMp
         -- self rule
         _trf =
             GrSplit_Sel nm_ _offItrf
         -- self rule
         _lhsOtrf =
             _trf
         -- copy rule (from local)
         _offOwillUseFor =
             _willUseFor
         ( _offIasNmL,_offIgathFviMp,_offInmAlias,_offItrf) =
             off_ _offOwillUseFor 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ( FvInfoMp,([HsName]),GrSplitL )
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrSplitL 
         _hdIgathFviMp :: FvInfoMp
         _hdIintroNmL :: ([HsName])
         _hdItrf :: GrSplit 
         _tlIgathFviMp :: FvInfoMp
         _tlIintroNmL :: ([HsName])
         _tlItrf :: GrSplitL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             _hdIintroNmL ++ _tlIintroNmL
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIgathFviMp,_hdIintroNmL,_hdItrf) =
             hd_ 
         ( _tlIgathFviMp,_tlIintroNmL,_tlItrf) =
             tl_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrSplitL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
-- GrTag -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         trf                  : SELF 
   alternatives:
      alternative App:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Con:
         child grtgAnn        : {GrTagAnn}
         child int            : {Int}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Fun:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Hole:
         visit 0:
            local trf         : _
      alternative PApp:
         child needs          : {Int}
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Rec:
         visit 0:
            local trf         : _
      alternative Unboxed:
         visit 0:
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
type T_GrTag  = ( FvInfoMp,GrTag )
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTag_App nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTag_Con grtgAnn_ int_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 4, column 17)
         _lhsOgathFviMp =
             fviMpSingleton' FvUse_Val nm_
         -- self rule
         _trf =
             GrTag_Fun nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTag_Hole
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 4, column 17)
         _lhsOgathFviMp =
             fviMpSingleton' FvUse_Val nm_
         -- self rule
         _trf =
             GrTag_PApp needs_ nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTag_Rec
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTag 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTag_Unboxed
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrTagL ------------------------------------------------------
{-
   visit 0:
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
type T_GrTagL  = ( FvInfoMp,GrTagL )
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTagL 
         _hdIgathFviMp :: FvInfoMp
         _hdItrf :: GrTag 
         _tlIgathFviMp :: FvInfoMp
         _tlItrf :: GrTagL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIgathFviMp,_hdItrf) =
             hd_ 
         ( _tlIgathFviMp,_tlItrf) =
             tl_ 
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTagL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrType ------------------------------------------------------
{-
   visit 0:
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
type T_GrType  = ( FvInfoMp,GrType )
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrType 
         _argsIgathFviMp :: FvInfoMp
         _argsItrf :: GrTypeBaseL 
         _resIgathFviMp :: FvInfoMp
         _resItrf :: GrTypeBase 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _argsIgathFviMp `fviMpUnion` _resIgathFviMp
         -- self rule
         _trf =
             GrType_Arrow _argsItrf _resItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _argsIgathFviMp,_argsItrf) =
             args_ 
         ( _resIgathFviMp,_resItrf) =
             res_ 
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrType 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrType_None
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
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
type T_GrTypeBase  = ( FvInfoMp,GrTypeBase )
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTypeBase 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTypeBase_Node
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTypeBase 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrTypeBase_Pointer
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
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
type T_GrTypeBaseL  = ( FvInfoMp,GrTypeBaseL )
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTypeBaseL 
         _hdIgathFviMp :: FvInfoMp
         _hdItrf :: GrTypeBase 
         _tlIgathFviMp :: FvInfoMp
         _tlItrf :: GrTypeBaseL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIgathFviMp,_hdItrf) =
             hd_ 
         ( _tlIgathFviMp,_tlItrf) =
             tl_ 
     in  ( _lhsOgathFviMp,_lhsOtrf))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrTypeBaseL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOtrf))
-- GrVal -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         willUseFor           : WillUseForS
      synthesized attributes:
         asNmL                : [Maybe HsName]
         gathFviMp            : FvInfoMp
         nmAlias              : NmAlias
         trf                  : SELF 
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Empty:
         visit 0:
            local trf         : _
      alternative EnumNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative LitInt:
         child int            : {Int}
         visit 0:
            local trf         : _
      alternative LitStr:
         child str            : {String}
         visit 0:
            local trf         : _
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
         visit 0:
            local trf         : _
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
         visit 0:
            local trf         : _
      alternative OpaqueNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative PtrNode:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative Tag:
         child tag            : GrTag 
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local trf         : _
      alternative VarNode:
         child fldL           : GrValL 
         visit 0:
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
type T_GrVal  = WillUseForS ->
                ( ([Maybe HsName]),FvInfoMp,NmAlias,GrVal )
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              _tagIgathFviMp :: FvInfoMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 31, column 17)
              _lhsOnmAlias =
                  NmAlias_Basic hsnUnknown (NmAlias_Nm nm_) BasicAnnot_Dflt
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- self rule
              _trf =
                  GrVal_BasicNode _tagItrf nm_
              -- self rule
              _lhsOtrf =
                  _trf
              ( _tagIgathFviMp,_tagItrf) =
                  tag_ 
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_Empty
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_EnumNode nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 32, column 17)
              _lhsOnmAlias =
                  NmAlias_Const hsnUnknown (GrVal_LitInt int_)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_LitInt int_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 33, column 17)
              _lhsOnmAlias =
                  NmAlias_Const hsnUnknown (GrVal_LitStr str_)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_LitStr str_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              _tagIgathFviMp :: FvInfoMp
              _tagItrf :: GrTag 
              _fldLIasNmL :: ([Maybe HsName])
              _fldLIgathFviMp :: FvInfoMp
              _fldLInmAliasL :: ([NmAlias])
              _fldLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 27, column 17)
              _lhsOnmAlias =
                  case _tagItrf of
                    GrTag_Con _ _ _
                      -> NmAlias_Grp hsnUnknown _fldLInmAliasL
                    _ -> NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp `fviMpUnion` _fldLIgathFviMp
              -- self rule
              _trf =
                  GrVal_Node _tagItrf _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              ( _tagIgathFviMp,_tagItrf) =
                  tag_ 
              ( _fldLIasNmL,_fldLIgathFviMp,_fldLInmAliasL,_fldLItrf) =
                  fldL_ 
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              _fldLIasNmL :: ([Maybe HsName])
              _fldLIgathFviMp :: FvInfoMp
              _fldLItrf :: GrAdaptL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 8, column 17)
              _lhsOgathFviMp =
                  fviMpUnions [fviMpSingleton nm_, _fldLIgathFviMp]
              -- self rule
              _trf =
                  GrVal_NodeAdapt nm_ _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              ( _fldLIasNmL,_fldLIgathFviMp,_fldLItrf) =
                  fldL_ 
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_OpaqueNode nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _trf =
                  GrVal_PtrNode nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              _tagIgathFviMp :: FvInfoMp
              _tagItrf :: GrTag 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- self rule
              _trf =
                  GrVal_Tag _tagItrf
              -- self rule
              _lhsOtrf =
                  _trf
              ( _tagIgathFviMp,_tagItrf) =
                  tag_ 
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 142, column 17)
              _lhsOasNmL =
                  [Just nm_]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 26, column 17)
              _lhsOnmAlias =
                  NmAlias_Nm nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 7, column 17)
              _lhsOgathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- self rule
              _trf =
                  GrVal_Var nm_
              -- self rule
              _lhsOtrf =
                  _trf
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (\ _lhsIwillUseFor ->
         (let _lhsOasNmL :: ([Maybe HsName])
              _lhsOnmAlias :: NmAlias
              _lhsOgathFviMp :: FvInfoMp
              _lhsOtrf :: GrVal 
              _fldLIasNmL :: ([Maybe HsName])
              _fldLIgathFviMp :: FvInfoMp
              _fldLInmAliasL :: ([NmAlias])
              _fldLItrf :: GrValL 
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 143, column 17)
              _lhsOasNmL =
                  [Nothing]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 35, column 17)
              _lhsOnmAlias =
                  NmAlias_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _fldLIgathFviMp
              -- self rule
              _trf =
                  GrVal_VarNode _fldLItrf
              -- self rule
              _lhsOtrf =
                  _trf
              ( _fldLIasNmL,_fldLIgathFviMp,_fldLInmAliasL,_fldLItrf) =
                  fldL_ 
          in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAlias,_lhsOtrf)))
-- GrValL ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         asNmL                : [Maybe HsName]
         gathFviMp            : FvInfoMp
         nmAliasL             : [NmAlias]
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = ( ([Maybe HsName]),FvInfoMp,([NmAlias]),GrValL )
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (let _lhsOnmAliasL :: ([NmAlias])
         _hdOwillUseFor :: WillUseForS
         _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrValL 
         _hdIasNmL :: ([Maybe HsName])
         _hdIgathFviMp :: FvInfoMp
         _hdInmAlias :: NmAlias
         _hdItrf :: GrVal 
         _tlIasNmL :: ([Maybe HsName])
         _tlIgathFviMp :: FvInfoMp
         _tlInmAliasL :: ([NmAlias])
         _tlItrf :: GrValL 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 48, column 17)
         _lhsOnmAliasL =
             _hdInmAlias : _tlInmAliasL
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonWillEval.ag"(line 33, column 17)
         _hdOwillUseFor =
             Set.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             _hdIasNmL ++ _tlIasNmL
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIasNmL,_hdIgathFviMp,_hdInmAlias,_hdItrf) =
             hd_ _hdOwillUseFor 
         ( _tlIasNmL,_tlIgathFviMp,_tlInmAliasL,_tlItrf) =
             tl_ 
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAliasL,_lhsOtrf))
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (let _lhsOnmAliasL :: ([NmAlias])
         _lhsOasNmL :: ([Maybe HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrValL 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonAliasAG.ag"(line 49, column 17)
         _lhsOnmAliasL =
             []
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/Inline.ag"(line 139, column 27)
         _lhsOasNmL =
             []
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOasNmL,_lhsOgathFviMp,_lhsOnmAliasL,_lhsOtrf))
-- GrVar -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         trf                  : SELF 
   alternatives:
      alternative Ignore:
         visit 0:
            local trf         : _
      alternative KnownTag:
         child tag            : GrTag 
         visit 0:
            local trf         : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
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
type T_GrVar  = ( FvInfoMp,([HsName]),GrVar )
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (let _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrVar 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 16, column 17)
         _lhsOintroNmL =
             [ ]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrVar_Ignore
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (let _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrVar 
         _tagIgathFviMp :: FvInfoMp
         _tagItrf :: GrTag 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 15, column 17)
         _lhsOintroNmL =
             [ error "introNmL known tag" ]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _tagIgathFviMp
         -- self rule
         _trf =
             GrVar_KnownTag _tagItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _tagIgathFviMp,_tagItrf) =
             tag_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (let _lhsOintroNmL :: ([HsName])
         _lhsOgathFviMp :: FvInfoMp
         _lhsOtrf :: GrVar 
         -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 14, column 17)
         _lhsOintroNmL =
             [nm_]
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- self rule
         _trf =
             GrVar_Var nm_
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         trf                  : SELF 
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
         visit 0:
            local trf         : _
      alternative Nil:
         visit 0:
            local trf         : _
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ( FvInfoMp,([HsName]),GrVarL )
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrVarL 
         _hdIgathFviMp :: FvInfoMp
         _hdIintroNmL :: ([HsName])
         _hdItrf :: GrVar 
         _tlIgathFviMp :: FvInfoMp
         _tlIintroNmL :: ([HsName])
         _tlItrf :: GrVarL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             _hdIintroNmL ++ _tlIintroNmL
         -- self rule
         _trf =
             (:) _hdItrf _tlItrf
         -- self rule
         _lhsOtrf =
             _trf
         ( _hdIgathFviMp,_hdIintroNmL,_hdItrf) =
             hd_ 
         ( _tlIgathFviMp,_tlIintroNmL,_tlItrf) =
             tl_ 
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (let _lhsOgathFviMp :: FvInfoMp
         _lhsOintroNmL :: ([HsName])
         _lhsOtrf :: GrVarL 
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
         _lhsOgathFviMp =
             Map.empty
         -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
         _lhsOintroNmL =
             []
         -- self rule
         _trf =
             []
         -- self rule
         _lhsOtrf =
             _trf
     in  ( _lhsOgathFviMp,_lhsOintroNmL,_lhsOtrf))